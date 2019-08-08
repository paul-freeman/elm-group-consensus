module Peer exposing
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
import Random exposing (Generator, Seed)


type alias Config =
    { syncTimeout : Float
    , groupSize : Int
    , neighborhoodSize : Int
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
    , vote : List Char
    }


initialModel : Config -> Address -> Model
initialModel config address =
    { address = address
    , config = config
    , groupSyncNeeded =
        Debouncer.manual
            |> settleWhenQuietFor (Just <| fromSeconds config.syncTimeout)
            |> toDebouncer
    , isOnline = False
    , messagesSent = 0
    , messagesReceived = 0
    , neighborStates = Dict.singleton address Offline
    , peers = [ address ]
    , peerState = []
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
    | IncrementMessageSent
    | IncrementMessageReceived
    | UpdatePrefix (List Char)
    | UserChangePrefix String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        IncrementMessageSent ->
            ( { model | messagesSent = model.messagesSent + 1 }, Cmd.none )

        IncrementMessageReceived ->
            ( { model | messagesReceived = model.messagesReceived + 1 }, Cmd.none )

        UpdatePrefix prefix ->
            ( { model | peerState = prefix }, Cmd.none )

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
                            Debug.todo "should not happen - but what if it does?"

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
