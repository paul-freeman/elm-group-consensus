port module Main exposing (main)

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
    { groupSize = 16
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
    , neighborhoodSize : Int
    , seed : Seed
    }


initialModel : Model
initialModel =
    let
        neighborhoodSize =
            ceiling (logBase 2 config.groupSize) + 1

        initSeed =
            Random.initialSeed 1001

        ( peerSeeds, modelSeed ) =
            List.foldl
                (\_ ( seeds, currentSeed ) ->
                    let
                        ( peerSeed, newSeed ) =
                            Random.step (Random.int 0 999999999) currentSeed
                    in
                    ( seeds ++ [ peerSeed ], newSeed )
                )
                ( [], initSeed )
                (List.range 0 (config.groupSize - 1))

        peers =
            List.map2
                (\n peerSeed ->
                    ( n
                    , Peer.initialModel
                        { deterministic = config.deterministic
                        , groupSize = config.groupSize
                        , messageTimeout = config.messageTimeout
                        , neighborhoodSize = neighborhoodSize
                        , seed = Random.initialSeed peerSeed
                        , syncTimeout = config.syncTimeout
                        }
                        n
                    )
                )
                (List.range 0 (config.groupSize - 1))
                peerSeeds
                |> Dict.fromList
    in
    { peers = peers
    , globalPrefixGoal = []
    , neighborhoodSize = neighborhoodSize
    , seed = modelSeed
    }


type Msg
    = AllOffline
    | AllOnline
    | CalculateGlobalPrefixGoal
    | PeerMsg Address Peer.Msg
    | RelayMessageIn { from : Address, to : Address, state : String }
    | ResetMessageCount


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        AllOffline ->
            Dict.foldl
                (\address peer ( currentModel, cmds ) ->
                    if peer.isOnline then
                        let
                            ( newModel, newCmd ) =
                                update (PeerMsg address <| Peer.ToggleOnline) currentModel
                        in
                        ( newModel, cmds ++ [ newCmd ] )

                    else
                        ( currentModel, cmds )
                )
                ( model, [] )
                model.peers
                |> Tuple.mapSecond Cmd.batch

        AllOnline ->
            Dict.foldl
                (\address peer ( currentModel, cmds ) ->
                    if not peer.isOnline then
                        let
                            ( newModel, newCmd ) =
                                update (PeerMsg address <| Peer.ToggleOnline) currentModel
                        in
                        ( newModel, cmds ++ [ newCmd ] )

                    else
                        ( currentModel, cmds )
                )
                ( model, [] )
                model.peers
                |> Tuple.mapSecond Cmd.batch

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

        PeerMsg address peerMsg ->
            let
                ( modelOne, cmdOne ) =
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

                ( modelTwo, cmdTwo ) =
                    update CalculateGlobalPrefixGoal modelOne
            in
            ( modelTwo, Cmd.batch [ cmdOne, cmdTwo ] )

        RelayMessageIn { from, to, state } ->
            let
                ( modelOne, cmdOne ) =
                    update (PeerMsg to <| Peer.IncrementMessageReceived) model

                ( modelTwo, cmdTwo ) =
                    update (PeerMsg to <| Peer.StateReceive { from = from, state = String.toList state }) modelOne
            in
            ( modelTwo, Cmd.batch [ cmdOne, cmdTwo ] )

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
            { onPress = Just (PeerMsg peer.address <| Peer.StartGroupSync)
            , label = Element.text "Group Sync"
            }
         , Input.button
            [ Border.color (Element.rgb 0.0 0.0 0.0)
            , Border.width 1
            , Element.padding 5
            , Background.color (Element.rgb 0.8 0.8 0.8)
            , Font.size 12
            ]
            { onPress = Just (PeerMsg peer.address <| Peer.ToggleOnline)
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
                            ( peerSeed, newSeed ) =
                                Random.step (Random.int 0 999999) model.seed
                                    |> Tuple.mapFirst Random.initialSeed

                            ( modelOne, cmdOne ) =
                                update
                                    (PeerMsg address Peer.RandomizePrefix)
                                    model
                        in
                        ( modelOne, cmds ++ [ cmdOne ] )
                    )
                    ( initialModel, [] )
                    (List.range 0 config.groupSize)
                    |> Tuple.mapSecond Cmd.batch
        , subscriptions = \model -> subscriptions
        , view = view
        , update = update
        }


port relayMessageIn :
    ({ from : Address, to : Address, state : String } -> msg)
    -> Sub msg


subscriptions : Sub Msg
subscriptions =
    relayMessageIn RelayMessageIn
