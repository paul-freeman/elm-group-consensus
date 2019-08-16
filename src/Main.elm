port module Main exposing (main)

import Array
import Bitwise
import Browser
import Browser.Dom exposing (Error, Viewport)
import Browser.Events
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
import Peer exposing (Address, Neighbor(..), State, Vote)
import Process
import Random exposing (Generator, Seed)
import Set
import Svg exposing (Svg, svg)
import Svg.Attributes as Attributes
import Svg.Events as Events
import Task
import Time exposing (Posix)


config =
    { checkPeersTimeout = 250.0
    , deterministic = False -- no Process.sleep calls
    , groupSize = 4
    , groupSyncDebouncing = False
    , messageTimeout = 3000.0
    , offlineRecheck = 60000.0
    , syncTimeout = 100.0
    }


type alias Model =
    { drawLines :
        List
            { from : Address
            , to : Address
            , time : Int
            }
    , globalPrefixGoal : List Char
    , hoverPeer : Maybe Address
    , peers : Dict Address Peer.Model
    , neighborhoodSize : Int
    , seed : Seed
    , svgView : Bool
    , svgViewport : Maybe Viewport
    , time : Int
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
    { drawLines = []
    , globalPrefixGoal = []
    , hoverPeer = Nothing
    , neighborhoodSize = neighborhoodSize
    , peers = peers
    , seed = modelSeed
    , svgView = False
    , svgViewport = Nothing
    , time = 0
    }


type Msg
    = AllOffline
    | AllOnline
    | CalculateGlobalPrefixGoal
    | DrawMessageSent Address Address (Maybe Posix)
    | GetSvgViewport (Result Error Viewport)
    | PeerMsg Address Peer.Msg
    | RelayMessageIn { from : Address, to : Address, vote : String }
    | RelayRegistrationIn { from : Address, to : Address }
    | ResetMessageCount
    | ResizeWindow Int Int
    | SetHoverPeer (Maybe Address)
    | SetTime Posix
    | ToggleSvg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        DrawMessageSent from to mTime ->
            case mTime of
                Nothing ->
                    ( model
                    , Task.perform
                        (DrawMessageSent from to)
                        (Task.map Just Time.now)
                    )

                Just time ->
                    ( { model
                        | drawLines =
                            ({ from = from
                             , to = to
                             , time = Time.posixToMillis time
                             }
                                :: model.drawLines
                            )
                                |> List.take config.groupSize
                      }
                    , Cmd.none
                    )

        GetSvgViewport result ->
            case result of
                Ok viewport ->
                    ( { model | svgViewport = Just viewport }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

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

        RelayMessageIn { from, to, vote } ->
            case
                D.decodeString
                    (D.field "first" D.string
                        |> D.andThen
                            (\first ->
                                D.field "second" D.int
                                    |> D.andThen
                                        (\second ->
                                            D.succeed ( String.toList first, second )
                                        )
                            )
                    )
                    vote
            of
                Ok goodVote ->
                    let
                        ( modelOne, cmdOne ) =
                            update (PeerMsg to <| Peer.StateReceive { from = from, vote = goodVote }) model

                        ( modelTwo, cmdTwo ) =
                            update (DrawMessageSent from to Nothing) modelOne
                    in
                    ( modelTwo, Cmd.batch [ cmdOne, cmdTwo ] )

                _ ->
                    ( model, Cmd.none )

        RelayRegistrationIn { from, to } ->
            let
                ( modelOne, cmdOne ) =
                    update (PeerMsg to <| Peer.RegistrationReceive { from = from }) model

                ( modelTwo, cmdTwo ) =
                    update (DrawMessageSent from to Nothing) modelOne
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

        ResizeWindow _ _ ->
            ( model, Task.attempt GetSvgViewport Browser.Dom.getViewport )

        SetHoverPeer mAddress ->
            ( { model | hoverPeer = mAddress }, Cmd.none )

        SetTime posix ->
            ( { model | time = Time.posixToMillis posix }, Cmd.none )

        ToggleSvg ->
            ( { model | svgView = not model.svgView }, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            ([ Element.row
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
                                    if Maybe.map Tuple.first v.vote == Just model.globalPrefixGoal then
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
                        { onPress = Just ToggleSvg
                        , label = Element.text "Graph View"
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
             ]
                ++ (if model.svgView then
                        [ Element.html <| viewSvg model ]

                    else
                        [ Dict.toList model.peers
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
                   )
            )


viewSvg : Model -> Html Msg
viewSvg model =
    let
        width =
            model.svgViewport
                |> Maybe.map (.viewport >> .width >> round)
                |> Maybe.withDefault 100

        halfWidth =
            width // 2

        height =
            model.svgViewport
                |> Maybe.map (.viewport >> .height >> round)
                |> Maybe.withDefault 100

        halfHeight =
            height // 2 - 25

        radius =
            min width height // 2 - 100
    in
    svg
        [ Attributes.width <| String.fromInt width
        , Attributes.height <| String.fromInt height
        , Attributes.viewBox <| "0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height
        ]
        ([ Svg.circle
            [ Attributes.cx <| String.fromInt halfWidth
            , Attributes.cy <| String.fromInt halfHeight
            , Attributes.r <| String.fromInt radius
            , Attributes.fill "white"
            , Attributes.stroke "black"
            , Attributes.strokeWidth "1"
            ]
            []
         ]
            ++ (model.drawLines
                    |> List.map
                        (drawMessageLine
                            halfWidth
                            halfHeight
                            radius
                            model.time
                        )
               )
            ++ (model.peers
                    |> Dict.map
                        (drawSvgPeer
                            halfWidth
                            halfHeight
                            radius
                            model
                        )
                    |> Dict.values
                    |> List.concat
               )
        )


drawMessageLine : Int -> Int -> Int -> Int -> { from : Address, to : Address, time : Int } -> Svg Msg
drawMessageLine centerX centerY radius now { from, to, time } =
    let
        myX =
            toFloat centerX + toFloat radius * cos (turns (toFloat from / config.groupSize))

        myY =
            toFloat centerY + toFloat radius * sin (turns (toFloat from / config.groupSize))

        theirX =
            toFloat centerX + toFloat radius * cos (turns (toFloat to / config.groupSize))

        theirY =
            toFloat centerY + toFloat radius * sin (turns (toFloat to / config.groupSize))

        delta =
            1 - (toFloat (now - time) / 3000)
    in
    Svg.line
        [ Attributes.x1 <| String.fromFloat myX
        , Attributes.y1 <| String.fromFloat myY
        , Attributes.x2 <| String.fromFloat theirX
        , Attributes.y2 <| String.fromFloat theirY
        , Attributes.stroke "black"
        , Attributes.strokeWidth "1"
        , Attributes.strokeOpacity <| String.fromFloat <| delta
        ]
        []


drawSvgPeer : Int -> Int -> Int -> Model -> Address -> Peer.Model -> List (Svg Msg)
drawSvgPeer centerX centerY radius model address peer =
    let
        myX =
            toFloat centerX + toFloat radius * cos (turns (toFloat address / config.groupSize))

        myY =
            toFloat centerY + toFloat radius * sin (turns (toFloat address / config.groupSize))
    in
    [ Svg.circle
        [ Attributes.cx <| String.fromFloat myX
        , Attributes.cy <| String.fromFloat myY
        , Attributes.r <| String.fromFloat <| 0.6 * pi * toFloat radius / config.groupSize
        , Attributes.fill "white"
        , Attributes.stroke <|
            if peer.isOnline then
                if Maybe.map Tuple.first peer.vote == Just model.globalPrefixGoal then
                    "green"

                else
                    "black"

            else
                "red"
        , Attributes.strokeWidth <|
            if peer.isOnline then
                if Maybe.map Tuple.first peer.vote == Just model.globalPrefixGoal then
                    "3"

                else
                    "black"

            else
                "3"
        , Events.onClick (PeerMsg address <| Peer.StartGroupSync)
        , Events.onMouseOver (SetHoverPeer (Just address))
        , Events.onMouseOut (SetHoverPeer Nothing)
        ]
        []
    ]
        ++ (if Just address == model.hoverPeer then
                peer.neighbors
                    |> Array.map Tuple.first
                    |> Array.map
                        (\neighborAddress ->
                            let
                                theirX =
                                    toFloat centerX + toFloat radius * cos (turns (toFloat neighborAddress / config.groupSize))

                                theirY =
                                    toFloat centerY + toFloat radius * sin (turns (toFloat neighborAddress / config.groupSize))
                            in
                            Svg.line
                                [ Attributes.x1 <| String.fromFloat myX
                                , Attributes.y1 <| String.fromFloat myY
                                , Attributes.x2 <| String.fromFloat theirX
                                , Attributes.y2 <| String.fromFloat theirY
                                , Attributes.stroke "black"
                                , Attributes.strokeWidth "1"
                                ]
                                []
                        )
                    |> Array.toList

            else
                []
           )


viewPeer : Model -> Peer.Model -> Element Msg
viewPeer model peer =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Border.color <|
            if peer.isOnline then
                if Maybe.map Tuple.first peer.vote == Just model.globalPrefixGoal then
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
                Element.text <|
                    "Vote - "
                        ++ (case peer.vote of
                                Just ( prefix, address ) ->
                                    String.fromList prefix
                                        ++ ":"
                                        ++ String.fromInt address

                                Nothing ->
                                    "Nothing"
                           )

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
            ++ [ Element.paragraph [ Font.size 12 ] [ Element.text "Neighbor votes" ] ]
            ++ (peer.neighbors
                    |> Array.foldl
                        (\( address, neighbor ) ( count, list ) ->
                            if count >= model.neighborhoodSize then
                                ( count, list )

                            else
                                case neighbor of
                                    Online _ ->
                                        ( count + 1, list ++ [ ( address, neighbor ) ] )

                                    _ ->
                                        ( count, list ++ [ ( address, neighbor ) ] )
                        )
                        ( 0, [] )
                    |> Tuple.second
                    |> List.map
                        (\( neighborAddress, neighborStatus ) ->
                            [ Element.paragraph
                                [ Font.size 12
                                , Element.padding 5
                                ]
                                [ Element.text <| "Peer " ++ String.fromInt neighborAddress ++ " - "
                                , case neighborStatus of
                                    Online ( prefix, address ) ->
                                        Element.el
                                            []
                                            (Element.text <|
                                                String.fromList prefix
                                                    ++ ":"
                                                    ++ String.fromInt address
                                            )

                                    Stale ( prefix, address ) ->
                                        Element.el
                                            []
                                            (Element.text <|
                                                String.fromList prefix
                                                    ++ ":"
                                                    ++ String.fromInt address
                                                    ++ " (stale)"
                                            )

                                    _ ->
                                        Element.el
                                            []
                                            (Element.text "Unknown")
                                ]
                            ]
                        )
                    |> List.concat
               )
            ++ [ Element.paragraph [ Font.size 12 ] [ Element.text "Registered peers" ] ]
            ++ (peer.neighbees
                    |> Set.toList
                    |> List.map
                        (\neighborAddress ->
                            [ Element.paragraph
                                [ Font.size 12
                                , Element.padding 5
                                ]
                                [ Element.text <|
                                    "Peer "
                                        ++ String.fromInt neighborAddress
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
                    ( initialModel, [ Task.attempt GetSvgViewport Browser.Dom.getViewport ] )
                    (List.range 0 config.groupSize)
                    |> Tuple.mapSecond Cmd.batch
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


port relayMessageIn : ({ from : Address, to : Address, vote : String } -> msg) -> Sub msg


port relayRegistrationIn : ({ from : Address, to : Address } -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        [ relayMessageIn RelayMessageIn
        , relayRegistrationIn RelayRegistrationIn
        , Browser.Events.onResize ResizeWindow
        ]
            ++ (if model.svgView then
                    [ Browser.Events.onAnimationFrame SetTime ]

                else
                    []
               )
