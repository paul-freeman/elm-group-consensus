module Main exposing (main)

import Bitwise
import Browser
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
import Process
import Random exposing (Generator, Seed)
import Task
import Time


config =
    { groupSize = 6
    , offlineRecheck = 60000.0
    , messageTimeout = 3000.0
    , syncTimeout = 100.0
    , checkPeersTimeout = 250.0
    , deterministic = False -- no Process.sleep calls
    }


type alias Address =
    Int


type alias Model =
    { peers : Dict Address Peer
    , commonPrefixGoal : List Char
    , seed : Seed
    , neighborhoodSize : Int
    }


type alias Peer =
    { address : Address
    , isOnline : Bool
    , state : State
    , prefix : Maybe (List Char)
    , neighborStates : Dict Address (Maybe Neighbor)
    , messagesSent : Int
    , messagesReceived : Int
    }


type Neighbor
    = Online State
    | Offline


type alias State =
    Dict Address (Maybe (List Char))


initialModel : ( Model, Cmd Msg )
initialModel =
    let
        peers =
            List.range 0 (config.groupSize - 1)
                |> List.map (\n -> ( n, initialPeer n ))
                |> Dict.fromList
    in
    ( { peers = peers
      , commonPrefixGoal = []
      , seed = Random.initialSeed 1001
      , neighborhoodSize = (round (logBase 2 config.groupSize) // 2 + 1) * 2 + 1
      }
    , Cmd.none
    )


initialPeer : Address -> Peer
initialPeer address =
    let
        initState =
            List.range 0 (config.groupSize - 1)
                |> List.map
                    (\groupMemberAddress ->
                        ( groupMemberAddress, Nothing )
                    )
                |> Dict.fromList
                |> Dict.insert address (Just [])
    in
    { address = address
    , isOnline = True
    , state = initState
    , prefix = Nothing
    , neighborStates = Dict.singleton address <| Just <| Online initState
    , messagesSent = 0
    , messagesReceived = 0
    }


type Msg
    = NoOp
    | CalculatePeers Address
    | RandomizePrefix Address
    | UserChangePrefix Address String
    | UpdatePrefix Address (List Char)
    | CalculateGlobalPrefixGoal
    | CalculateLocalPrefixGoal Address
    | CalculateCurrentState Address
    | IncrementMessageSent Address
    | IncrementMessageReceived Address
    | ResetMessageCount
    | ChangePeerStatus Address Address Neighbor
    | ToggleOnline Address
    | AllOnline
    | AllOffline
    | StartGroupSync Address
    | RequestStateDelay
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CalculatePeers address ->
            Maybe.map
                (\peer ->
                    let
                        newPeerDict =
                            List.map
                                (\neighborAddress ->
                                    Dict.get neighborAddress peer.neighborStates
                                        |> Maybe.withDefault Nothing
                                        |> Tuple.pair neighborAddress
                                )
                                (chooseNeighbors peer model)
                                |> Dict.fromList

                        modelOne =
                            { model
                                | peers =
                                    Dict.insert address
                                        { peer | neighborStates = newPeerDict }
                                        model.peers
                            }
                    in
                    ( modelOne, Cmd.none )
                )
                (Dict.get address model.peers)
                |> Maybe.withDefault ( model, Cmd.none )

        RandomizePrefix address ->
            Maybe.map
                (\peer ->
                    let
                        ( newPrefix, newSeed ) =
                            Random.step prefixGen model.seed
                    in
                    update (UpdatePrefix address newPrefix) { model | seed = newSeed }
                )
                (Dict.get address model.peers)
                |> Maybe.withDefault ( model, Cmd.none )

        UserChangePrefix address newString ->
            update (UpdatePrefix address (String.toList newString)) model

        UpdatePrefix address prefix ->
            Maybe.map
                (\peer ->
                    let
                        newState =
                            Dict.insert address (Just prefix) peer.state

                        modelOne =
                            { model
                                | peers =
                                    Dict.insert address
                                        { peer
                                            | state = newState
                                            , neighborStates =
                                                Dict.insert address
                                                    (if peer.isOnline then
                                                        Just (Online newState)

                                                     else
                                                        Just Offline
                                                    )
                                                    peer.neighborStates
                                        }
                                        model.peers
                            }

                        ( modelTwo, cmdTwo ) =
                            update CalculateGlobalPrefixGoal modelOne

                        ( modelThree, cmdThree ) =
                            if not peer.isOnline then
                                ( modelTwo, Cmd.none )

                            else
                                update (CalculateCurrentState address) modelTwo
                    in
                    ( modelThree, Cmd.batch [ cmdTwo, cmdThree ] )
                )
                (Dict.get address model.peers)
                |> Maybe.withDefault ( model, Cmd.none )

        CalculateGlobalPrefixGoal ->
            let
                prefixes =
                    Dict.filter (\_ peer -> peer.isOnline) model.peers
                        |> Dict.toList
                        |> List.map
                            (\( address, peer ) ->
                                case Dict.get address peer.state of
                                    Just (Just prefix) ->
                                        prefix

                                    _ ->
                                        []
                            )

                newGoal =
                    case ( List.head prefixes, List.tail prefixes ) of
                        ( Just head, Just tail ) ->
                            List.foldl longestPrefix head tail

                        ( Just head, Nothing ) ->
                            head

                        _ ->
                            []
            in
            ( { model | commonPrefixGoal = newGoal }, Cmd.none )

        CalculateLocalPrefixGoal address ->
            Maybe.map
                (\peer ->
                    ( { model
                        | peers =
                            Dict.insert address
                                { peer | prefix = computeStatePrefix peer.state }
                                model.peers
                      }
                    , Cmd.none
                    )
                )
                (Dict.get address model.peers)
                |> Maybe.withDefault ( model, Cmd.none )

        CalculateCurrentState thisAddress ->
            Maybe.map
                (\peer ->
                    let
                        oldState =
                            peer.state

                        newState =
                            let
                                ( myStateInNeighborhood, otherStates ) =
                                    peer.neighborStates
                                        |> Dict.partition (\address _ -> address == thisAddress)
                            in
                            case Dict.get thisAddress myStateInNeighborhood |> Maybe.andThen identity of
                                Just (Online myState) ->
                                    Dict.foldl
                                        addNeighborVotes
                                        -- my state with initial votes
                                        (Dict.map (\_ v -> [ ( v, 1 ) ]) myState)
                                        otherStates
                                        |> findWinner model.neighborhoodSize

                                _ ->
                                    peer.state
                    in
                    if oldState == newState then
                        -- nothing to do
                        ( model, Cmd.none )

                    else
                        -- need to update our neighbors
                        let
                            modelOne =
                                { model
                                    | peers =
                                        Dict.insert
                                            peer.address
                                            { peer | state = newState }
                                            model.peers
                                }
                        in
                        update (StartGroupSync peer.address) modelOne
                )
                (Dict.get thisAddress model.peers)
                |> Maybe.withDefault ( model, Cmd.none )

        IncrementMessageSent address ->
            Maybe.map
                (\peer ->
                    ( { model
                        | peers =
                            Dict.insert
                                address
                                { peer | messagesSent = peer.messagesSent + 1 }
                                model.peers
                      }
                    , Cmd.none
                    )
                )
                (Dict.get address model.peers)
                |> Maybe.withDefault ( model, Cmd.none )

        IncrementMessageReceived address ->
            Maybe.map
                (\peer ->
                    ( { model
                        | peers =
                            Dict.insert
                                address
                                { peer | messagesReceived = peer.messagesReceived + 1 }
                                model.peers
                      }
                    , Cmd.none
                    )
                )
                (Dict.get address model.peers)
                |> Maybe.withDefault ( model, Cmd.none )

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
                                |> Maybe.andThen identity

                        newPeerStatus =
                            Just otherPeerStatus

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
                            case ( oldPeerStatus, newPeerStatus ) of
                                ( Just Offline, _ ) ->
                                    update (CalculatePeers peerAddress) modelOne

                                ( _, Just Offline ) ->
                                    update (CalculatePeers peerAddress) modelOne

                                _ ->
                                    ( modelOne, Cmd.none )

                        ( modelThree, cmdThree ) =
                            update (CalculateCurrentState peerAddress) modelTwo
                    in
                    ( modelThree, Cmd.batch [ cmdTwo, cmdThree ] )
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
                                    { peer
                                        | isOnline = False
                                        , neighborStates = Dict.singleton address (Just Offline)
                                    }
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
                                update (CalculatePeers peer.address) modelOne

                            ( modelThree, cmdThree ) =
                                update CalculateGlobalPrefixGoal modelTwo

                            ( modelFour, cmdFour ) =
                                update (CalculateCurrentState peer.address) modelThree
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
                            ( firstModel, firstCmd ) =
                                if not peer.isOnline then
                                    ( model, Cmd.none )

                                else
                                    update (CalculatePeers peerAddress) model

                            ( finalModel, peerCmds ) =
                                peer.neighborStates
                                    |> Dict.keys
                                    -- don't sync with yourself
                                    |> List.filter ((/=) peerAddress)
                                    |> List.foldl
                                        (\neighborAddress ( foldModel, cmds ) ->
                                            Maybe.map
                                                (\neighborPeer ->
                                                    let
                                                        ( latency, newSeed ) =
                                                            Random.step latencyGen foldModel.seed

                                                        ( newModel, newCmd ) =
                                                            update
                                                                (RequestStateDelay
                                                                    -- pull neighbor's state
                                                                    -- XXX this could be replaced with an official request message
                                                                    { to = peerAddress -- to me
                                                                    , from = neighborAddress -- from my neighbor
                                                                    , state = neighborPeer.state
                                                                    }
                                                                    latency
                                                                )
                                                                { foldModel | seed = newSeed }
                                                    in
                                                    ( newModel, cmds ++ [ newCmd ] )
                                                )
                                                (Dict.get neighborAddress model.peers)
                                                |> Maybe.withDefault ( firstModel, [ firstCmd ] )
                                        )
                                        ( firstModel, [ firstCmd ] )
                        in
                        ( finalModel, Cmd.batch peerCmds )
                )
                (Dict.get peerAddress model.peers)
                |> Maybe.withDefault ( model, Cmd.none )

        RequestStateDelay { from, to, state } latency ->
            Maybe.map2
                (\fromPeer toPeer ->
                    if not fromPeer.isOnline then
                        -- they can't send to you if they are offline
                        let
                            ( modelOne, cmdOne ) =
                                update (IncrementMessageSent from) model
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
                                        update (IncrementMessageSent from) model

                                    ( modelTwo, cmdTwo ) =
                                        update (IncrementMessageReceived to) modelOne
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
                                        update (IncrementMessageSent from) model
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

                    else if List.member from (Dict.keys toPeer.neighborStates) then
                        -- if they are one of our neighbors, update their vote
                        update (ChangePeerStatus to from <| Online state) model

                    else
                        -- not our neighbor
                        ( model, Cmd.none )
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
                        update (ChangePeerStatus to from Offline) model
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
                                                    if neighbor == Just Offline then
                                                        Nothing

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
                        (config.groupSize
                            |> String.fromInt
                        )

                    -- , Element.text " | Members in sync: "
                    -- , Element.text
                    --     (model.peers
                    --         |> Dict.foldl
                    --             (\k v a ->
                    --                 if List.map Tuple.first v.neighborhoodVote == model.commonPrefixGoal then
                    --                     a + 1
                    --                 else
                    --                     a
                    --             )
                    --             0
                    --         |> String.fromInt
                    --     )
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
                |> List.drop (0 * (config.groupSize // 3))
                |> List.take (config.groupSize // 3)
                |> List.map (Tuple.second >> viewPeer model)
                |> Element.row
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
            , Dict.toList model.peers
                |> List.drop (1 * (config.groupSize // 3))
                |> List.take (config.groupSize // 3)
                |> List.map (Tuple.second >> viewPeer model)
                |> Element.row
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
            , Dict.toList model.peers
                |> List.drop (2 * (config.groupSize // 3))
                -- |> List.take (config.groupSize // 3)
                |> List.map (Tuple.second >> viewPeer model)
                |> Element.row
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
            ]


viewPeer : Model -> Peer -> Element Msg
viewPeer model peer =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill

        -- , Border.color <|
        --     if peer.isOnline then
        --         if
        --             List.Extra.takeWhile (\( _, c ) -> c == model.neighborhoodSize) peer.neighborhoodVote
        --                 |> List.map Tuple.first
        --                 |> (==) model.commonPrefixGoal
        --         then
        --             Element.rgb 0.0 1.0 0.0
        --         else
        --             Element.rgb 0.0 0.0 0.0
        --     else
        --         Element.rgb 1.0 0.0 0.0
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
            { text =
                peer.prefix
                    |> Maybe.withDefault []
                    |> String.fromList
            , onChange = UserChangePrefix peer.address
            , label = Input.labelAbove [] (Element.text <| "Local Prefix")
            , placeholder = Just <| Input.placeholder [] (Element.text "< no state >")
            }
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
            ++ (Dict.toList peer.neighborStates
                    |> List.map
                        (\( address, status ) ->
                            case status of
                                Just neighbor ->
                                    if List.member address (Dict.keys peer.neighborStates) then
                                        [ Element.paragraph
                                            [ Font.size 12
                                            , Element.padding 5
                                            ]
                                            [ Element.text <| "Peer " ++ String.fromInt address ++ " - "
                                            , case neighbor of
                                                Online state ->
                                                    Element.el
                                                        [ Font.color <| Element.rgb 0.0 0.0 0.0
                                                        , Border.color <| Element.rgb 0.0 1.0 0.0
                                                        , Border.width 1
                                                        ]
                                                        (Element.text <|
                                                            "Online: "
                                                                ++ String.fromList (Maybe.withDefault [] <| computeStatePrefix state)
                                                        )

                                                Offline ->
                                                    Element.el
                                                        [ Font.color <| Element.rgb 1.0 1.0 1.0
                                                        , Background.color <| Element.rgb 1.0 0.0 0.0
                                                        ]
                                                        (Element.text "Offline")
                                            ]
                                        ]

                                    else
                                        []

                                _ ->
                                    []
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


chooseNeighbors : Peer -> Model -> List Address
chooseNeighbors peer model =
    -- includes yourself as first entry
    let
        sortedPeers =
            (config.groupSize - 1)
                |> List.range 0
                |> List.sortBy (Bitwise.xor peer.address)
                |> List.indexedMap Tuple.pair
                |> Dict.fromList

        neighbors =
            config.groupSize
                |> logBase 2
                |> ceiling
                |> List.range 0
                |> List.map (\n -> 2 ^ n - 1)
                |> List.map
                    (\i ->
                        case Dict.get i sortedPeers of
                            Just address ->
                                [ address ]

                            Nothing ->
                                []
                    )
                |> List.concat

        numOfflinePeers =
            neighbors
                |> List.map
                    (\address ->
                        case Dict.get address peer.neighborStates |> Maybe.andThen identity of
                            Just Offline ->
                                1

                            _ ->
                                0
                    )
                |> List.sum

        alternativePeers =
            sortedPeers
                |> Dict.toList
                |> List.filter
                    (\( _, address ) ->
                        let
                            status =
                                Dict.get address peer.neighborStates |> Maybe.andThen identity
                        in
                        not
                            (List.member address neighbors
                                || (status == Just Offline)
                                || (address == peer.address)
                            )
                    )
                |> List.map Tuple.second
    in
    List.append neighbors alternativePeers
        |> List.take model.neighborhoodSize


longestPrefix : List Char -> List Char -> List Char
longestPrefix xs ys =
    List.map2 Tuple.pair xs ys
        |> takeWhile (\( x, y ) -> x == y)
        |> List.map Tuple.first


addNeighborVotes :
    Address
    -> Maybe Neighbor
    -> Dict Address (List ( Maybe (List Char), Int ))
    -> Dict Address (List ( Maybe (List Char), Int ))
addNeighborVotes address theirState totalState =
    case theirState of
        Just (Online onlineState) ->
            -- merge the current state with the total state
            Dict.merge
                addVotesToTotal
                (\_ _ _ r -> r)
                (\_ _ r -> r)
                onlineState
                -- not used
                Dict.empty
                totalState

        _ ->
            totalState


addVotesToTotal :
    Address
    -> Maybe (List Char)
    -> Dict Address (List ( Maybe (List Char), Int ))
    -> Dict Address (List ( Maybe (List Char), Int ))
addVotesToTotal address thisValue totalVotes =
    Dict.update address
        (\maybeVotes ->
            case maybeVotes of
                Just votes ->
                    Just <|
                        (List.foldr
                            (\( mPrefix, voteCount ) ( newVotes, foundIt ) ->
                                if thisValue == mPrefix then
                                    ( ( mPrefix, voteCount + 1 ) :: newVotes, True )

                                else
                                    ( ( mPrefix, voteCount ) :: newVotes, foundIt )
                            )
                            ( [], False )
                            votes
                            |> (\( checkedState, foundIt ) ->
                                    if foundIt then
                                        checkedState

                                    else
                                        ( thisValue, 1 ) :: checkedState
                               )
                        )

                Nothing ->
                    Nothing
        )
        totalVotes


findWinner : Int -> Dict Address (List ( Maybe (List Char), Int )) -> State
findWinner neighborhoodSize totalVotes =
    Dict.map
        (\_ votes ->
            List.filter (\( _, c ) -> c > neighborhoodSize // 2) votes
                |> List.map Tuple.first
                |> List.head
                |> Maybe.withDefault Nothing
        )
        totalVotes


computeStatePrefix : State -> Maybe (List Char)
computeStatePrefix state =
    Dict.foldl
        (\_ mPrefix mAcc ->
            case ( mPrefix, mAcc ) of
                ( Just prefix, Just acc ) ->
                    Just <| longestPrefix prefix acc

                ( Just prefix, Nothing ) ->
                    Just prefix

                ( Nothing, acc ) ->
                    acc
        )
        Nothing
        state
