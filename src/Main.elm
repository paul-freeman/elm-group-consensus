module Main exposing (main)

import Array
import Bitwise
import Browser
import Debouncer.Messages as Debouncer exposing (Debouncer, fromSeconds, provideInput, settleWhenQuietFor, toDebouncer)
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode as D
import Json.Decode.Pipeline
import List.Extra exposing (takeWhile)
import Peer exposing (Address, Neighbor(..), State)
import Process
import Random exposing (Generator, Seed)
import Task
import Time


config =
    { groupSize = 8
    , offlineRecheck = 60000.0
    , messageTimeout = 3000.0
    , syncTimeout = 100.0
    , checkPeersTimeout = 250.0
    , deterministic = False -- no Process.sleep calls
    , groupSyncDebouncing = False
    }


type alias Model =
    { peers : Dict Address Peer.Model
    , globalPrefixGoal : List Char
    , seed : Seed
    , neighborhoodSize : Int
    }


initialModel : ( Model, Cmd Msg )
initialModel =
    let
        neighborhoodSize =
            ceiling (logBase 2 config.groupSize) + 1

        peers =
            List.range 0 (config.groupSize - 1)
                |> List.map
                    (\n ->
                        ( n
                        , Peer.initialModel
                            { syncTimeout = config.syncTimeout
                            , groupSize = config.groupSize
                            , neighborhoodSize = neighborhoodSize
                            }
                            n
                        )
                    )
                |> Dict.fromList
    in
    ( { peers = peers
      , globalPrefixGoal = []
      , seed = Random.initialSeed 1001
      , neighborhoodSize = neighborhoodSize
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | RandomizePrefix Address
    | CalculateGlobalPrefixGoal
    | ResetMessageCount
    | ChangePeerStatus Address Address Neighbor
    | AllOnline
    | AllOffline
    | ToggleOnline Address
    | StartGroupSync Address
    | SendStateDelay
        { from : Address
        , to : Address
        , state : State
        }
        Latency
    | StateReceive
        { from : Address
        , to : Address
        , state : State
        }
    | MessageLost
        { from : Address
        , to : Address
        }
    | MarkUnknown Address Address
    | PeerMsg Address Peer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            ( model, Cmd.none )

        RandomizePrefix address ->
            Maybe.map
                (\peer ->
                    let
                        ( newPrefix, newSeed ) =
                            Random.step prefixGen model.seed
                    in
                    update (PeerMsg address <| Peer.UpdatePrefix newPrefix) { model | seed = newSeed }
                )
                (Dict.get address model.peers)
                |> Maybe.withDefault ( model, Cmd.none )

        CalculateGlobalPrefixGoal ->
            let
                prefixes =
                    Dict.filter (\_ peer -> peer.isOnline) model.peers
                        |> Dict.map (\_ peer -> peer.peerState)
                        |> Dict.values

                newGoal =
                    case ( List.head prefixes, List.tail prefixes ) of
                        ( Just head, Just tail ) ->
                            List.foldl Peer.longestPrefix head tail

                        ( Just head, Nothing ) ->
                            head

                        _ ->
                            []
            in
            ( { model | globalPrefixGoal = newGoal }, Cmd.none )

        ResetMessageCount ->
            ( { model
                | peers =
                    Dict.map
                        (\_ peer ->
                            { peer | messagesSent = 0, messagesReceived = 0 }
                        )
                        model.peers
              }
            , Cmd.none
            )

        ChangePeerStatus peerAddress otherPeerAddress otherPeerStatus ->
            Maybe.map2
                (\peer otherPeer ->
                    let
                        oldPeerStatus =
                            Dict.get otherPeerAddress peer.neighborStates
                                |> Maybe.withDefault Unknown

                        newPeerStatus =
                            otherPeerStatus

                        modelOne =
                            { model
                                | peers =
                                    Dict.insert peerAddress
                                        { peer
                                            | neighborStates =
                                                Dict.insert
                                                    otherPeerAddress
                                                    newPeerStatus
                                                    peer.neighborStates
                                        }
                                        model.peers
                            }

                        ( modelTwo, cmdTwo ) =
                            ( modelOne, Cmd.none )

                        ( modelThree, cmdThree ) =
                            update (PeerMsg peerAddress <| Peer.CalculateLocalVote) modelTwo

                        ( modelFour, cmdFour ) =
                            if oldPeerStatus /= newPeerStatus then
                                update (StartGroupSync peerAddress) modelThree

                            else
                                ( modelThree, Cmd.none )
                    in
                    ( modelFour, Cmd.batch [ cmdTwo, cmdThree, cmdFour ] )
                )
                (Dict.get peerAddress model.peers)
                (Dict.get otherPeerAddress model.peers)
                |> Maybe.withDefault ( model, Cmd.none )

        ToggleOnline address ->
            Maybe.map
                (\peer ->
                    if peer.isOnline then
                        ( { model
                            | peers =
                                Dict.insert peer.address
                                    { peer | isOnline = False }
                                    model.peers
                          }
                        , Cmd.none
                        )

                    else
                        let
                            modelOne =
                                { model
                                    | peers =
                                        Dict.insert peer.address
                                            { peer | isOnline = True }
                                            model.peers
                                }

                            ( modelTwo, cmdTwo ) =
                                update (PeerMsg address <| Peer.CalculatePeers) modelOne

                            ( modelThree, cmdThree ) =
                                update CalculateGlobalPrefixGoal modelTwo

                            ( modelFour, cmdFour ) =
                                update (PeerMsg peer.address <| Peer.CalculateLocalVote) modelThree
                        in
                        ( modelFour, Cmd.batch [ cmdTwo, cmdThree, cmdFour ] )
                )
                (Dict.get address model.peers)
                |> Maybe.withDefault ( model, Cmd.none )

        AllOnline ->
            Dict.foldl
                (\address peer ( currentModel, cmds ) ->
                    if not peer.isOnline then
                        let
                            ( newModel, newCmd ) =
                                update (ToggleOnline address) currentModel
                        in
                        ( newModel, cmds ++ [ newCmd ] )

                    else
                        ( currentModel, cmds )
                )
                ( model, [] )
                model.peers
                |> Tuple.mapSecond Cmd.batch

        AllOffline ->
            Dict.foldl
                (\address peer ( currentModel, cmds ) ->
                    if peer.isOnline then
                        let
                            ( newModel, newCmd ) =
                                update (ToggleOnline address) currentModel
                        in
                        ( newModel, cmds ++ [ newCmd ] )

                    else
                        ( currentModel, cmds )
                )
                ( model, [] )
                model.peers
                |> Tuple.mapSecond Cmd.batch

        StartGroupSync peerAddress ->
            Maybe.map
                (\peer ->
                    if not peer.isOnline then
                        -- can't sync when offline
                        ( model, Cmd.none )

                    else
                        let
                            ( finalModel, peerCmds ) =
                                peer.peers
                                    -- don't sync with yourself
                                    |> List.filter ((/=) peerAddress)
                                    |> List.foldl
                                        (\neighborAddress ( currentModel, cmds ) ->
                                            let
                                                ( latency, newSeed ) =
                                                    Random.step latencyGen currentModel.seed

                                                ( newModel, newCmd ) =
                                                    update
                                                        (SendStateDelay
                                                            { from = peerAddress
                                                            , to = neighborAddress
                                                            , state = peer.vote
                                                            }
                                                            latency
                                                        )
                                                        { currentModel | seed = newSeed }
                                            in
                                            ( newModel, cmds ++ [ newCmd ] )
                                        )
                                        ( model, [] )
                        in
                        ( finalModel, Cmd.batch peerCmds )
                )
                (Dict.get peerAddress model.peers)
                |> Maybe.withDefault ( model, Cmd.none )

        SendStateDelay { from, to, state } latency ->
            Maybe.map2
                (\fromPeer toPeer ->
                    if not toPeer.isOnline then
                        -- they can't receive from you if they are offline
                        let
                            ( modelOne, cmdOne ) =
                                update (PeerMsg from <| Peer.IncrementMessageSent) model
                        in
                        ( modelOne
                        , Cmd.batch
                            [ cmdOne
                            , Task.perform
                                (always (MessageLost { from = from, to = to }))
                                (if config.deterministic then
                                    Time.now

                                 else
                                    Process.sleep config.messageTimeout |> Task.andThen (always Time.now)
                                )
                            ]
                        )

                    else
                        case latency of
                            Delayed time ->
                                let
                                    ( modelOne, cmdOne ) =
                                        update (PeerMsg from <| Peer.IncrementMessageSent) model

                                    ( modelTwo, cmdTwo ) =
                                        update (PeerMsg to <| Peer.IncrementMessageReceived) modelOne
                                in
                                ( modelTwo
                                , Cmd.batch
                                    [ cmdOne
                                    , cmdTwo
                                    , Task.perform
                                        (always <|
                                            StateReceive
                                                { from = from
                                                , to = to
                                                , state = state
                                                }
                                        )
                                        (if config.deterministic then
                                            Time.now

                                         else
                                            Process.sleep time
                                                |> Task.andThen (always Time.now)
                                        )
                                    ]
                                )

                            Failed ->
                                let
                                    ( modelOne, cmdOne ) =
                                        update (PeerMsg from <| Peer.IncrementMessageSent) model
                                in
                                ( modelOne
                                , Cmd.batch
                                    [ cmdOne
                                    , Task.perform
                                        (always (MessageLost { from = from, to = to }))
                                        (if config.deterministic then
                                            Time.now

                                         else
                                            Process.sleep config.messageTimeout |> Task.andThen (always Time.now)
                                        )
                                    ]
                                )
                )
                (Dict.get from model.peers)
                (Dict.get to model.peers)
                |> Maybe.withDefault ( model, Cmd.none )

        StateReceive { from, to, state } ->
            Maybe.map
                (\toPeer ->
                    if not toPeer.isOnline then
                        -- can't receive if you are offline
                        ( model, Cmd.none )

                    else
                        update (ChangePeerStatus to from <| Online state) model
                )
                (Dict.get to model.peers)
                |> Maybe.withDefault ( model, Cmd.none )

        MessageLost { from, to } ->
            Maybe.map
                (\toPeer ->
                    if not toPeer.isOnline then
                        -- can't receive if offline
                        ( model, Cmd.none )

                    else
                        update (ChangePeerStatus from to Offline) model
                )
                (Dict.get from model.peers)
                |> Maybe.withDefault ( model, Cmd.none )

        MarkUnknown myAddress otherAddress ->
            Maybe.map2
                (\myPeer otherPeer ->
                    ( { model
                        | peers =
                            Dict.insert myAddress
                                { myPeer
                                    | neighborStates =
                                        Dict.update otherAddress
                                            (Maybe.map
                                                (\neighbor ->
                                                    if neighbor == Offline then
                                                        Unknown

                                                    else
                                                        neighbor
                                                )
                                            )
                                            myPeer.neighborStates
                                }
                                model.peers
                      }
                    , Cmd.none
                    )
                )
                (Dict.get myAddress model.peers)
                (Dict.get otherAddress model.peers)
                |> Maybe.withDefault ( model, Cmd.none )

        PeerMsg address peerMsg ->
            Maybe.map
                (\peer ->
                    let
                        ( newPeer, peerCmd ) =
                            Peer.update peerMsg peer
                    in
                    ( { model | peers = Dict.insert address newPeer model.peers }
                    , Cmd.map (PeerMsg address) peerCmd
                    )
                )
                (Dict.get address model.peers)
                |> Maybe.withDefault ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Element.row
                [ Element.width Element.fill
                , Element.height <| Element.px 50
                ]
                [ let
                    messagesSent =
                        model.peers
                            |> Dict.foldl (\k v a -> v.messagesSent + a) 0
                  in
                  Element.paragraph []
                    [ Element.text "Group size: "
                    , Element.text
                        ((config.groupSize
                            |> String.fromInt
                         )
                            ++ " ("
                            ++ (model.peers
                                    |> Dict.foldl
                                        (\k v a ->
                                            if v.isOnline then
                                                a + 1

                                            else
                                                a
                                        )
                                        0
                                    |> String.fromInt
                               )
                            ++ " online)"
                        )
                    , Element.text " | Members in sync: "
                    , Element.text
                        (model.peers
                            |> Dict.foldl
                                (\k v a ->
                                    if v.vote == model.globalPrefixGoal then
                                        a + 1

                                    else
                                        a
                                )
                                0
                            |> String.fromInt
                        )
                    , Element.text " | Messages sent: "
                    , Element.text
                        (messagesSent
                            |> String.fromInt
                        )
                    , Element.text " | Messages per member: "
                    , Element.text
                        ((toFloat messagesSent / config.groupSize)
                            |> round
                            |> String.fromInt
                        )
                    , Element.text " | "
                    , Input.button
                        [ Border.color (Element.rgb 0.0 0.0 0.0)
                        , Border.width 1
                        , Element.padding 5
                        , Background.color (Element.rgb 0.8 0.8 0.8)
                        , Font.size 12
                        ]
                        { onPress = Just AllOnline
                        , label = Element.text "All Online"
                        }
                    , Input.button
                        [ Border.color (Element.rgb 0.0 0.0 0.0)
                        , Border.width 1
                        , Element.padding 5
                        , Background.color (Element.rgb 0.8 0.8 0.8)
                        , Font.size 12
                        ]
                        { onPress = Just AllOffline
                        , label = Element.text "All Offline"
                        }
                    , Input.button
                        [ Border.color (Element.rgb 0.0 0.0 0.0)
                        , Border.width 1
                        , Element.padding 5
                        , Background.color (Element.rgb 0.8 0.8 0.8)
                        , Font.size 12
                        ]
                        { onPress = Just ResetMessageCount
                        , label = Element.text "Reset Message Count"
                        }
                    ]
                ]
            , Dict.toList model.peers
                |> List.drop (0 * (config.groupSize // 2))
                |> List.take (config.groupSize // 2)
                |> List.map (Tuple.second >> viewPeer model)
                |> Element.row
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
            , Dict.toList model.peers
                |> List.drop (1 * (config.groupSize // 2))
                -- |> List.take (config.groupSize // 2)
                |> List.map (Tuple.second >> viewPeer model)
                |> Element.row
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
            ]


viewPeer : Model -> Peer.Model -> Element Msg
viewPeer model peer =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Border.color <|
            if peer.isOnline then
                if peer.vote == model.globalPrefixGoal then
                    Element.rgb 0.0 1.0 0.0

                else
                    Element.rgb 0.0 0.0 0.0

            else
                Element.rgb 1.0 0.0 0.0
        , Border.width 2
        ]
        ([ Element.el [ Element.centerX ]
            (Element.paragraph
                [ Font.size 12
                , Font.underline
                , Element.padding 5
                ]
                [ if peer.isOnline then
                    Element.text <| "Peer " ++ String.fromInt peer.address

                  else
                    Element.text <| "Peer " ++ String.fromInt peer.address ++ " (offline)"
                ]
            )
         , Input.text [ Element.width <| Element.px 200, Font.size 12 ]
            { text = String.fromList peer.peerState
            , onChange = PeerMsg peer.address << Peer.UserChangePrefix
            , label = Input.labelAbove [] (Element.text <| "Local Prefix")
            , placeholder = Just <| Input.placeholder [] (Element.text "< no state >")
            }
         , Element.paragraph
            [ Font.size 12
            , Element.padding 5
            ]
            [ if peer.isOnline then
                Element.text <| "Vote - " ++ String.fromList peer.vote

              else
                Element.text <| "Vote - (offline)"
            ]
         , Input.button
            [ Border.color (Element.rgb 0.0 0.0 0.0)
            , Border.width 1
            , Element.padding 5
            , Background.color (Element.rgb 0.8 0.8 0.8)
            , Font.size 12
            ]
            { onPress = Just (StartGroupSync peer.address)
            , label = Element.text "Group Sync"
            }
         , Input.button
            [ Border.color (Element.rgb 0.0 0.0 0.0)
            , Border.width 1
            , Element.padding 5
            , Background.color (Element.rgb 0.8 0.8 0.8)
            , Font.size 12
            ]
            { onPress = Just (ToggleOnline peer.address)
            , label =
                Element.text <|
                    if peer.isOnline then
                        "Go Offline"

                    else
                        "Go Online"
            }
         ]
            ++ (peer.peers
                    |> List.map
                        (\neighborAddress ->
                            [ Element.paragraph
                                [ Font.size 12
                                , Element.padding 5
                                ]
                                [ Element.text <| "Peer " ++ String.fromInt neighborAddress ++ " - "
                                , if neighborAddress == peer.address then
                                    Element.el
                                        []
                                        (Element.text <| String.fromList peer.vote)

                                  else
                                    case Dict.get neighborAddress peer.neighborStates of
                                        Just (Online prefix) ->
                                            Element.el
                                                []
                                                (Element.text <| String.fromList prefix)

                                        _ ->
                                            Element.el
                                                []
                                                (Element.text "Unknown")
                                ]
                            ]
                        )
                    |> List.concat
               )
            ++ [ Element.paragraph [ Font.size 12 ]
                    [ Element.text
                        ("Messages sent: " ++ String.fromInt peer.messagesSent)
                    ]
               , Element.paragraph [ Font.size 12 ]
                    [ Element.text
                        ("Messages received: " ++ String.fromInt peer.messagesReceived)
                    ]
               ]
        )


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.element
        { init =
            \flags ->
                List.foldl
                    (\address ( model, cmds ) ->
                        let
                            ( modelOne, cmdOne ) =
                                update (RandomizePrefix address) model

                            -- ( model, Cmd.none )
                        in
                        ( modelOne, cmds ++ [ cmdOne, cmdOne ] )
                    )
                    (Tuple.mapSecond List.singleton initialModel)
                    (List.range 0 config.groupSize)
                    |> Tuple.mapSecond Cmd.batch
        , subscriptions = \model -> Sub.none
        , view = view
        , update = update
        }


type Latency
    = Failed
    | Delayed Float


latencyGen : Generator Latency
latencyGen =
    Random.weighted ( 100.0, False ) [ ( 0.0, True ) ]
        |> Random.andThen
            (\failed ->
                if failed then
                    Random.constant Failed

                else
                    Random.map Delayed (Random.float 10.0 200.0)
            )


prefixGen : Generator (List Char)
prefixGen =
    Random.weighted ( 0.05, [ 'A' ] )
        [ ( 0.95, [ 'A', 'B' ] )
        , ( 9.0, [ 'A', 'B', 'C' ] )
        , ( 30.0, [ 'A', 'B', 'C', 'D' ] )
        , ( 20.0, [ 'A', 'B', 'C', 'D', 'E' ] )
        , ( 40.0, [ 'A', 'B', 'C', 'D', 'E', 'F' ] )
        ]
