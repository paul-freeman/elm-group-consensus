port module Peer exposing
    ( Address
    , Model
    , Msg(..)
    , Neighbor(..)
    , State
    , Vote
    , chooseNeighbors
    , initialModel
    , longestPrefix
    , update
    )

import Array exposing (Array)
import Array.Extra exposing (removeAt)
import Bitwise
import Debouncer.Messages as Debouncer exposing (Debouncer, fromSeconds, provideInput, settleWhenQuietFor, toDebouncer)
import Dict exposing (Dict)
import Json.Encode as E
import List.Extra exposing (takeWhile)
import Process
import Random exposing (Generator, Seed)
import Set exposing (Set)
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


type alias Vote =
    ( List Char, Address )


type alias Model =
    { address : Address
    , config : Config
    , groupSyncNeeded : Debouncer Msg
    , isOnline : Bool
    , messagesReceived : Int
    , messagesSent : Int

    -- who do I want votes from?
    , neighbors : Array ( Address, Neighbor )

    -- who wants my vote?
    , neighbees : Set Address
    , peerState : List Char
    , seed : Seed
    , vote : Maybe Vote
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
    , neighbors =
        chooseNeighbors address config.groupSize
            |> Array.map
                (\x ->
                    ( x
                    , if x == address then
                        Offline

                      else
                        Unknown
                    )
                )
    , neighbees = Set.empty
    , peerState = []
    , seed = config.seed
    , vote = Nothing
    }


type Neighbor
    = Online Vote
    | Offline
    | Stale Vote
    | Unknown


type alias State =
    List Char


type Msg
    = NoOp
    | CalculateNeighborhoodVote
    | ChangeNeighborStatus Address Neighbor
    | IncrementMessageSent
    | IncrementMessageReceived
    | MarkNeighborUnknown Address
    | MessageLost { to : Address }
    | RandomizePrefix
    | ReadyToGroupSync (Debouncer.Msg Msg)
    | RegistrationReceive { from : Address }
    | RelayMessageOut
        { from : Address
        , to : Address
        , vote : String
        }
    | RelayRegistrationOut
        { from : Address
        , to : Address
        }
    | SendVoteDelay
        { from : Address
        , to : Address
        , vote : Vote
        }
        Latency
    | StartGroupSync
    | StateReceive { from : Address, vote : Vote }
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

        CalculateNeighborhoodVote ->
            let
                newVote =
                    Array.foldl
                        (\( neighborAddress, neighborStatus ) ( count, ( prefix, address ) ) ->
                            if count >= model.config.neighborhoodSize then
                                ( count, ( prefix, address ) )

                            else
                                case neighborStatus of
                                    Online ( peerPrefix, peerAddress ) ->
                                        ( count + 1
                                        , if longestPrefix prefix peerPrefix == prefix then
                                            ( prefix, address )

                                          else
                                            ( peerPrefix, peerAddress )
                                        )

                                    _ ->
                                        ( count, ( prefix, address ) )
                        )
                        ( 0, ( model.peerState, model.address ) )
                        model.neighbors
                        |> Tuple.second
            in
            ( { model
                | vote = Just newVote
                , neighbors =
                    Array.map
                        (\( address, status ) ->
                            if address == model.address then
                                case status of
                                    Offline ->
                                        Debug.log "modelA" ( model.address, Offline )

                                    _ ->
                                        Debug.log "modelB" ( model.address, Online newVote )

                            else
                                Debug.log "modelC" ( address, status )
                        )
                        (Debug.log "neighbors" model.neighbors)
              }
            , Cmd.none
            )

        ChangeNeighborStatus neighborAddress neighborStatus ->
            {-
               We are inserting a new status for a neighbor, which can be
               Online, Offline, Stale, Unknown.

               The primary goal is to insert the new status into the neighbor
               array under the correct address.

               However, we also want to invalidate any existing votes that are
               stale by marking them as such.
            -}
            update CalculateNeighborhoodVote
                { model
                    | neighbors =
                        Array.map
                            (updateNeighborStatus ( neighborAddress, neighborStatus ))
                            model.neighbors
                }

        IncrementMessageSent ->
            ( { model | messagesSent = model.messagesSent + 1 }, Cmd.none )

        IncrementMessageReceived ->
            ( { model | messagesReceived = model.messagesReceived + 1 }, Cmd.none )

        MarkNeighborUnknown peerAddress ->
            ( { model
                | neighbors =
                    Array.map
                        (\( address, neighbor ) ->
                            if neighbor == Offline then
                                ( address, Unknown )

                            else
                                ( address, neighbor )
                        )
                        model.neighbors
              }
            , Cmd.none
            )

        MessageLost { to } ->
            if not model.isOnline then
                ( model, Cmd.none )

            else
                update (ChangeNeighborStatus to Offline) model

        RandomizePrefix ->
            let
                ( newPrefix, newSeed ) =
                    Random.step prefixGen model.seed
            in
            update (UpdatePrefix newPrefix) { model | seed = newSeed }

        ReadyToGroupSync subMsg ->
            Debouncer.update update updateDebouncer subMsg model

        RegistrationReceive { from } ->
            if not model.isOnline then
                -- can't receive if you are offline
                ( model, Cmd.none )

            else
                case model.vote of
                    Just vote ->
                        let
                            ( latency, newSeed ) =
                                Random.step latencyGen model.seed
                        in
                        update
                            (SendVoteDelay
                                { from = model.address
                                , to = from
                                , vote = vote
                                }
                                latency
                            )
                            { model
                                | seed = newSeed
                                , neighbees = Set.insert from model.neighbees
                            }

                    _ ->
                        ( { model
                            | neighbees = Set.insert from model.neighbees
                          }
                        , Cmd.none
                        )

        RelayMessageOut { from, to, vote } ->
            ( model, relayMessageOut { from = from, to = to, vote = vote } )

        RelayRegistrationOut { from, to } ->
            ( model, relayRegistrationOut { from = from, to = to } )

        SendVoteDelay { to, vote } latency ->
            if not model.isOnline then
                -- can't send if you are offline
                ( model, Cmd.none )

            else
                let
                    ( modelOne, cmdOne ) =
                        -- we don't actually know if it failed
                        update IncrementMessageSent model
                in
                case latency of
                    Delayed time ->
                        ( modelOne
                        , Cmd.batch
                            [ cmdOne
                            , Task.perform
                                (always <|
                                    RelayMessageOut
                                        { from = model.address
                                        , to = to
                                        , vote =
                                            E.encode 0 <|
                                                E.object
                                                    [ ( "first", E.string <| String.fromList <| Tuple.first vote )
                                                    , ( "second", E.int <| Tuple.second vote )
                                                    ]
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

        StateReceive { from, vote } ->
            if not model.isOnline then
                -- can't receive if you are offline
                ( model, Cmd.none )

            else
                let
                    ( modelOne, cmdOne ) =
                        update IncrementMessageReceived model

                    ( modelTwo, cmdTwo ) =
                        update (ChangeNeighborStatus from <| Online vote) modelOne
                in
                ( modelTwo, Cmd.batch [ cmdOne, cmdTwo ] )

        StartGroupSync ->
            if not model.isOnline then
                -- can't sync when offline
                ( model, Cmd.none )

            else
                let
                    ( registrationCount, registrationModel, registrationCmds ) =
                        -- register with your neighbors if they are Unknown
                        model.neighbors
                            -- don't sync with yourself
                            |> Array.filter (\( a, _ ) -> a /= model.address)
                            |> Array.foldl
                                (\( neighborAddress, neighbor ) ( count, currentModel, cmds ) ->
                                    if count >= model.config.neighborhoodSize then
                                        ( count, currentModel, cmds )

                                    else
                                        let
                                            action =
                                                let
                                                    ( latency, newSeed ) =
                                                        Random.step latencyGen currentModel.seed
                                                in
                                                ( count + 1
                                                , { currentModel | seed = newSeed }
                                                , cmds
                                                    ++ [ relayRegistrationOut
                                                            { from = model.address
                                                            , to = neighborAddress
                                                            }
                                                       ]
                                                )
                                        in
                                        case neighbor of
                                            Unknown ->
                                                action

                                            Stale _ ->
                                                action

                                            _ ->
                                                ( count + 1, currentModel, cmds )
                                )
                                ( 0, model, [] )

                    ( finalModel, peerCmds ) =
                        model.neighbees
                            -- don't sync with yourself
                            |> Set.filter ((/=) model.address)
                            |> Set.foldl
                                (\neighborAddress ( currentModel, cmds ) ->
                                    let
                                        ( latency, newSeed ) =
                                            Random.step latencyGen currentModel.seed

                                        ( newModel, newCmd ) =
                                            case model.vote of
                                                Just vote ->
                                                    update
                                                        (SendVoteDelay
                                                            { from = model.address
                                                            , to = neighborAddress
                                                            , vote = vote
                                                            }
                                                            latency
                                                        )
                                                        { currentModel | seed = newSeed }

                                                _ ->
                                                    ( { currentModel | seed = newSeed }, Cmd.none )
                                    in
                                    ( newModel, cmds ++ [ newCmd ] )
                                )
                                ( registrationModel, [] )
                in
                ( finalModel, Cmd.batch (registrationCmds ++ peerCmds) )

        ToggleOnline ->
            if model.isOnline then
                ( { model
                    | isOnline = False
                    , neighbors =
                        Array.map
                            (\( address, status ) ->
                                if address == model.address then
                                    ( model.address, Offline )

                                else
                                    ( address, status )
                            )
                            model.neighbors
                  }
                , Cmd.none
                )

            else
                let
                    ( modelOne, cmdOne ) =
                        update CalculateNeighborhoodVote
                            { model
                                | isOnline = True
                                , neighbors =
                                    Array.map
                                        (\( address, status ) ->
                                            if address == model.address then
                                                ( model.address, Unknown )

                                            else
                                                ( address, status )
                                        )
                                        model.neighbors
                            }

                    ( modelTwo, cmdTwo ) =
                        update
                            (StartGroupSync
                                |> provideInput
                                |> ReadyToGroupSync
                            )
                            modelOne
                in
                ( modelTwo, Cmd.batch [ cmdOne, cmdTwo ] )

        UpdatePrefix prefix ->
            ( { model | peerState = prefix }, Cmd.none )

        UserChangePrefix newString ->
            update (UpdatePrefix (String.toList newString)) model


chooseNeighbors : Address -> Int -> Array Address
chooseNeighbors address groupSize =
    -- includes yourself as first entry
    let
        sortedPeers =
            List.range 0 (groupSize - 1)
                |> List.sortBy (abs << (-) address)
                |> Array.fromList

        indices =
            logBase 2 (toFloat groupSize)
                |> ceiling
                |> List.range 0
                |> List.map (\n -> 2 ^ n)
                |> (\list -> List.repeat (groupSize // List.length list + 1) list)
                |> List.concat
                |> (::) 0
                |> List.take groupSize
                |> List.indexedMap
                    (\n x ->
                        if n == (groupSize - 1) then
                            0

                        else
                            modBy (groupSize - 1 - n) (x - n)
                    )
    in
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


updateNeighborStatus : ( Address, Neighbor ) -> ( Address, Neighbor ) -> ( Address, Neighbor )
updateNeighborStatus ( newAddress, newStatus ) ( currentAddress, currentStatus ) =
    ( currentAddress
    , if newAddress == currentAddress then
        -- update this neighbor to this status (unless it's is a stale status)
        case ( newStatus, currentStatus ) of
            ( Online ( newS, newA ), Online ( currentS, currentA ) ) ->
                if newA /= currentA || longestPrefix newS currentS /= newS then
                    -- it means the new status is a longer sequence or a new
                    -- peer
                    newStatus

                else
                    -- it means the new status is using the same address, but
                    -- they are saying a shorter sequence now. keep the current
                    -- one
                    currentStatus

            _ ->
                -- anything else, just update it
                newStatus

      else
        -- this is not the entry for this neighbor, but if they are using an
        -- old status as their vote, we can probably mark that vote as stale
        case Debug.log "test" ( newStatus, currentStatus ) of
            ( Online ( newS, newA ), Online ( currentS, currentA ) ) ->
                if newA /= currentA || longestPrefix newS currentS == newS then
                    currentStatus

                else
                    Stale ( currentS, currentA )

            _ ->
                currentStatus
    )


port relayMessageOut : { from : Address, to : Address, vote : String } -> Cmd msg


port relayRegistrationOut : { from : Address, to : Address } -> Cmd msg
