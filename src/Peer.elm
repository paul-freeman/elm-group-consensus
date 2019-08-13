port module Peer exposing
    ( Address
    , Model
    , Msg(..)
    , Neighbor(..)
    , State
    , initialModel
    , longestPrefix
    , update
    )

import Array
import Array.Extra exposing (removeAt)
import Bitwise
import Debouncer.Messages as Debouncer exposing (Debouncer, fromSeconds, provideInput, settleWhenQuietFor, toDebouncer)
import Dict exposing (Dict)
import List.Extra exposing (takeWhile)
import Process
import Random exposing (Generator, Seed)
import Task
import Time


type alias Config =
    { deterministic : Bool
    , groupSize : Int
    , messageTimeout : Float
    , neighborhoodSize : Int
    , seed : Seed
    , syncTimeout : Float
    }


type alias Address =
    Int


type alias Model =
    { address : Address
    , config : Config
    , groupSyncNeeded : Debouncer Msg
    , isOnline : Bool
    , messagesReceived : Int
    , messagesSent : Int
    , neighborStates : Dict Address Neighbor
    , peers : List Address
    , peerState : List Char
    , seed : Seed
    , vote : List Char
    }


initialModel : Config -> Address -> Model
initialModel config address =
    { address = address
    , config = config
    , groupSyncNeeded =
        Debouncer.manual
            |> settleWhenQuietFor
                (Just <|
                    fromSeconds <|
                        (config.syncTimeout / 1000.0)
                )
            |> toDebouncer
    , isOnline = False
    , messagesSent = 0
    , messagesReceived = 0
    , neighborStates = Dict.singleton address Offline
    , peers = [ address ]
    , peerState = []
    , seed = config.seed
    , vote = []
    }


type Neighbor
    = Online State
    | Offline
    | Unknown


type alias State =
    List Char


type Msg
    = NoOp
    | CalculateLocalVote
    | CalculatePeers
    | ChangePeerStatus Address Neighbor
    | IncrementMessageSent
    | IncrementMessageReceived
    | MarkUnknown Address
    | MessageLost
        { to : Address
        }
    | RandomizePrefix
    | ReadyToGroupSync (Debouncer.Msg Msg)
    | RelayMessageOut
        { from : Address
        , to : Address
        , state : String
        }
    | SendStateDelay
        { from : Address
        , to : Address
        , state : State
        }
        Latency
    | StartGroupSync
    | StateReceive { from : Address, state : State }
    | ToggleOnline
    | UpdatePrefix (List Char)
    | UserChangePrefix String


updateDebouncer : Debouncer.UpdateConfig Msg Model
updateDebouncer =
    { mapMsg = ReadyToGroupSync
    , getDebouncer = .groupSyncNeeded
    , setDebouncer = \debouncer model -> { model | groupSyncNeeded = debouncer }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log ("Peer " ++ String.fromInt model.address) msg of
        NoOp ->
            ( model, Cmd.none )

        CalculatePeers ->
            let
                newPeers =
                    chooseNeighbors model

                newPeerDict =
                    List.map
                        (\neighborAddress ->
                            Dict.get neighborAddress model.neighborStates
                                |> Maybe.withDefault Unknown
                                |> Tuple.pair neighborAddress
                        )
                        newPeers
                        |> Dict.fromList

                modelOne =
                    { model
                        | neighborStates = newPeerDict
                        , peers = newPeers
                    }
            in
            ( modelOne, Cmd.none )

        CalculateLocalVote ->
            ( { model
                | vote =
                    Dict.foldl
                        (\_ neighbor acc ->
                            case neighbor of
                                Online prefix ->
                                    longestPrefix prefix acc

                                _ ->
                                    acc
                        )
                        model.peerState
                        model.neighborStates
              }
            , Cmd.none
            )

        ChangePeerStatus otherPeerAddress otherPeerStatus ->
            let
                oldPeerStatus =
                    Dict.get otherPeerAddress model.neighborStates
                        |> Maybe.withDefault Unknown

                newPeerStatus =
                    otherPeerStatus

                modelOne =
                    { model
                        | neighborStates =
                            Dict.insert
                                otherPeerAddress
                                newPeerStatus
                                model.neighborStates
                    }

                ( modelTwo, cmdTwo ) =
                    ( modelOne, Cmd.none )

                ( modelThree, cmdThree ) =
                    update CalculateLocalVote modelTwo

                ( modelFour, cmdFour ) =
                    if oldPeerStatus /= newPeerStatus then
                        -- update StartGroupSync modelThree
                        update
                            (StartGroupSync
                                |> provideInput
                                |> ReadyToGroupSync
                            )
                            modelThree

                    else
                        ( modelThree, Cmd.none )
            in
            ( modelFour, Cmd.batch [ cmdTwo, cmdThree, cmdFour ] )

        IncrementMessageSent ->
            ( { model | messagesSent = model.messagesSent + 1 }, Cmd.none )

        IncrementMessageReceived ->
            ( { model | messagesReceived = model.messagesReceived + 1 }, Cmd.none )

        MarkUnknown peerAddress ->
            ( { model
                | neighborStates =
                    Dict.update peerAddress
                        (Maybe.map
                            (\neighbor ->
                                if neighbor == Offline then
                                    Unknown

                                else
                                    neighbor
                            )
                        )
                        model.neighborStates
              }
            , Cmd.none
            )

        MessageLost { to } ->
            if not model.isOnline then
                ( model, Cmd.none )

            else
                update (ChangePeerStatus to Offline) model

        RandomizePrefix ->
            let
                ( newPrefix, newSeed ) =
                    Random.step prefixGen model.seed
            in
            update (UpdatePrefix newPrefix) { model | seed = newSeed }

        ReadyToGroupSync subMsg ->
            Debouncer.update update updateDebouncer subMsg model

        RelayMessageOut { from, to, state } ->
            ( model, relayMessageOut { from = from, to = to, state = state } )

        SendStateDelay { to, state } latency ->
            if not model.isOnline then
                -- can't send if you are offline
                ( model, Cmd.none )

            else
                case latency of
                    Delayed time ->
                        let
                            ( modelOne, cmdOne ) =
                                update IncrementMessageSent model
                        in
                        ( modelOne
                        , Cmd.batch
                            [ cmdOne
                            , Task.perform
                                (always <|
                                    RelayMessageOut
                                        { from = model.address
                                        , to = to
                                        , state = String.fromList state
                                        }
                                )
                                (if model.config.deterministic then
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
                                update IncrementMessageSent model
                        in
                        ( modelOne
                        , Cmd.batch
                            [ cmdOne
                            , Task.perform
                                (always (MessageLost { to = to }))
                                (if model.config.deterministic then
                                    Time.now

                                 else
                                    Process.sleep model.config.messageTimeout |> Task.andThen (always Time.now)
                                )
                            ]
                        )

        StateReceive { from, state } ->
            if not model.isOnline then
                -- can't receive if you are offline
                ( model, Cmd.none )

            else
                update (ChangePeerStatus from <| Online state) model

        StartGroupSync ->
            if not model.isOnline then
                -- can't sync when offline
                ( model, Cmd.none )

            else
                let
                    ( finalModel, peerCmds ) =
                        model.peers
                            -- don't sync with yourself
                            |> List.filter ((/=) model.address)
                            |> List.foldl
                                (\neighborAddress ( currentModel, cmds ) ->
                                    let
                                        ( latency, newSeed ) =
                                            Random.step latencyGen currentModel.seed

                                        ( newModel, newCmd ) =
                                            update
                                                (SendStateDelay
                                                    { from = model.address
                                                    , to = neighborAddress
                                                    , state = model.vote
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

        ToggleOnline ->
            if model.isOnline then
                ( { model | isOnline = False }
                , Cmd.none
                )

            else
                let
                    modelOne =
                        { model | isOnline = True }

                    ( modelTwo, cmdTwo ) =
                        update CalculatePeers modelOne

                    ( modelThree, cmdThree ) =
                        update CalculateLocalVote modelTwo
                in
                ( modelThree, Cmd.batch [ cmdTwo, cmdThree ] )

        UpdatePrefix prefix ->
            let
                ( modelOne, cmdOne ) =
                    update CalculateLocalVote
                        { model
                            | peerState = prefix
                            , neighborStates =
                                Dict.map
                                    (\k v ->
                                        case v of
                                            Online state ->
                                                if state == model.vote then
                                                    Unknown

                                                else
                                                    Online state

                                            x ->
                                                x
                                    )
                                    model.neighborStates
                            , vote = prefix
                        }

                ( modelTwo, cmdTwo ) =
                    if model.vote /= modelOne.vote then
                        update
                            (StartGroupSync
                                |> provideInput
                                |> ReadyToGroupSync
                            )
                            modelOne

                    else
                        ( modelOne, cmdOne )
            in
            ( modelTwo, Cmd.batch [ cmdOne, cmdTwo ] )

        UserChangePrefix newString ->
            update (UpdatePrefix (String.toList newString)) model


chooseNeighbors : Model -> List Address
chooseNeighbors model =
    -- includes yourself as first entry
    let
        sortedPeers =
            List.range 0 (model.config.groupSize - 1)
                |> List.sortBy (Bitwise.xor model.address)
                |> Array.fromList

        indices =
            logBase 2 (toFloat model.config.groupSize)
                |> ceiling
                |> List.range 0
                |> List.map (\n -> 2 ^ n)
                |> (\list -> List.repeat (model.config.neighborhoodSize // List.length list + 1) list)
                |> List.concat
                |> (::) 0
                |> List.take model.config.neighborhoodSize
                |> List.indexedMap
                    (\n x ->
                        if n == (model.config.groupSize - 1) then
                            0

                        else
                            modBy (model.config.groupSize - 1 - n) (x - n)
                    )

        finalPeers =
            List.foldl
                (\index ( input, output ) ->
                    let
                        element =
                            Array.get index input

                        newInput =
                            removeAt index input
                    in
                    case element of
                        Nothing ->
                            ( input, output )

                        Just e ->
                            ( newInput, Array.push e output )
                )
                ( sortedPeers, Array.empty )
                indices
                |> Tuple.second
                |> Array.toList
    in
    finalPeers


longestPrefix : List Char -> List Char -> List Char
longestPrefix xs ys =
    List.map2 Tuple.pair xs ys
        |> takeWhile (\( x, y ) -> x == y)
        |> List.map Tuple.first


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


port relayMessageOut :
    { from : Address, to : Address, state : String }
    -> Cmd msg
