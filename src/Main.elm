module Main exposing (Flags, Model, Msg, main)

import Browser exposing (Document)
import Browser.Events
import Element exposing (..)
import Element.Border as Border
import Game.Crew as Crew exposing (Alignment(..), Crew)
import Game.Data exposing (Action(..), Applyable(..), Card(..), Event(..), Game, Reputation(..))
import Game.Item
import Gui.Color
import Gui.Input as Input
import Json.Decode exposing (Decoder, Value)
import Random exposing (Generator, Seed)
import Random.List
import Set
import Toople exposing (Tuple2(..), Tuple3(..))


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- TYPES ----


type Model
    = Initializing WindowSize
    | Playing Seed WindowSize Game
    | GameOver Seed WindowSize String


type alias WindowSize =
    ( Int, Int )


type alias Flags =
    Value



---- INIT ----


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        windowSize : WindowSize
        windowSize =
            case Json.Decode.decodeValue decodeFlags flags of
                Ok val ->
                    val

                Err _ ->
                    ( 0, 0 )
    in
    ( Initializing windowSize
    , Random.independentSeed
        |> Random.generate Initialize
    )


decodeFlags : Decoder ( Int, Int )
decodeFlags =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "width" Json.Decode.int)
        (Json.Decode.field "height" Json.Decode.int)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> WindowResized ( w, h ))



---- UPDATE ----


type Msg
    = Initialize Seed
    | ExecuteAction (Game -> Generator Game)
    | WindowResized WindowSize


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( WindowResized windowSize, Initializing _ ) ->
            ( Initializing windowSize, Cmd.none )

        ( WindowResized windowSize, Playing seed _ game ) ->
            ( Playing seed windowSize game, Cmd.none )

        ( WindowResized windowSize, GameOver seed _ reason ) ->
            ( GameOver seed windowSize reason, Cmd.none )

        ( Initialize seed, Initializing windowSize ) ->
            let
                ( initialGame, nextSeed ) =
                    Random.step
                        (Random.map
                            (\crew ->
                                { event = Game.Data.firstEvent
                                , resultOfAction = "You board your ship"
                                , crew = crew
                                , rareItems = []
                                , region =
                                    { name = "Federation"
                                    , reputation = Allied
                                    }
                                , availableCards = Set.fromList (List.range 0 (List.length Game.Data.baseEvents - 1))
                                , availableRegionNames = Set.singleton "carl"
                                }
                            )
                            -- 428 is the crew size of the Enterprise
                            (Random.list 428
                                (Crew.random
                                    { moraleMin = 80
                                    , moraleMax = Crew.moraleMaximum
                                    , alignmentsWeighted = initialCrewAlignments
                                    }
                                )
                            )
                        )
                        seed
            in
            ( Playing nextSeed windowSize initialGame
            , Cmd.none
            )

        ( ExecuteAction actionToApply, Playing seed windowSize game ) ->
            let
                ( nextGame, nextSeed ) =
                    Random.step (actionToApply game) seed
            in
            if List.isEmpty nextGame.crew then
                ( GameOver seed windowSize "The crew is all dead, thus ends your journey."
                , Cmd.none
                )

            else
                ( checkMorale nextSeed windowSize nextGame
                , Cmd.none
                )

        _ ->
            ( model, Cmd.none )


checkMorale : Seed -> WindowSize -> Game -> Model
checkMorale seed windowSize game =
    let
        ( mutinousCrew, alliedCrew ) =
            List.partition
                (\crew -> crew.morale < 0)
                game.crew

        crewToKill : Int
        crewToKill =
            List.length mutinousCrew // 4
    in
    if crewToKill * 2 >= List.length alliedCrew then
        GameOver seed windowSize "The crew is abhoard with your actions and mutinies, overthrowing you."

    else if crewToKill == 0 then
        Playing seed windowSize game

    else
        let
            ( nextGame, nextSeed ) =
                Random.step
                    (Random.map
                        (\shuffledCrew ->
                            { game
                                | resultOfAction = game.resultOfAction ++ "\n\nDue to your actions some of the crew attempts to mutiny but fails. You end up losing " ++ String.fromInt crewToKill ++ " allied crew memebrs plus those who mutinied."
                                , crew = List.drop crewToKill shuffledCrew
                            }
                        )
                        (Random.List.shuffle alliedCrew)
                    )
                    seed
        in
        Playing nextSeed windowSize nextGame


initialCrewAlignments : ( ( Float, Alignment ), List ( Float, Alignment ) )
initialCrewAlignments =
    ( ( 20, LawfulGood )
    , [ ( 21, NeutralGood )
      , ( 13, ChaoticGood )
      , ( 13, LawfulNeutral )
      , ( 13, TrueNeutral )
      , ( 8, ChaoticNeutral )
      , ( 6, LawfulEvil )
      , ( 4, NeutralEvil )
      , ( 2, ChaoticEvil )
      ]
    )



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "Galactic"
    , body =
        [ layoutWith
            { options =
                [ focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Just Gui.Color.focused
                    , shadow =
                        Just
                            { offset = ( 0, 0 )
                            , color = Gui.Color.focused
                            , size = 2
                            , blur = 6
                            }
                    }
                ]
            }
            [ width fill, height fill, padding 16 ]
            (viewBody model)
        ]
    }


viewBody : Model -> Element Msg
viewBody model =
    case model of
        Initializing windowSize ->
            validateWindow windowSize (text "Initializing...")

        Playing _ windowSize game ->
            validateWindow windowSize (viewGame game)

        GameOver _ windowSize reason ->
            validateWindow windowSize (text reason)


validateWindow : WindowSize -> Element msg -> Element msg
validateWindow windowSize content =
    let
        ( width, height ) =
            windowSize
    in
    if width < height && width < 1024 then
        paragraph
            []
            [ text "This game is best played on a screen at least 1024px in width. If your're on a phone, maybe try rotating it horizontal" ]

    else
        content


viewGame : Game -> Element Msg
viewGame game =
    row
        [ spacing 16, width fill, height fill ]
        [ viewState game
        , viewEvents game
        ]


viewState : Game -> Element Msg
viewState game =
    column
        [ height fill, spacing 16, width fill ]
        [ viewCrew game ]


viewCrew : Game -> Element Msg
viewCrew state =
    column
        [ spacing 16
        , width fill
        , height fill
        ]
        [ column
            [ width fill ]
            [ el
                [ padding 8
                , width fill
                , Border.solid
                , Border.widthEach
                    { top = 0
                    , bottom = 1
                    , left = 0
                    , right = 0
                    }
                ]
                (text "Inventory")
            , case state.rareItems of
                [] ->
                    el [ padding 8 ] (text "Empty")

                _ ->
                    paragraph
                        [ padding 8 ]
                        [ state.rareItems
                            |> List.map Game.Item.toString
                            |> String.join ", "
                            |> text
                        ]
            ]
        , column
            [ width fill
            , height fill
            , clipY
            ]
            [ el
                [ padding 8
                , width fill
                , Border.solid
                , Border.widthEach
                    { top = 0
                    , bottom = 1
                    , left = 0
                    , right = 0
                    }
                ]
                (text ("Crew: " ++ String.fromInt (List.length state.crew)))
            , state.crew
                |> List.sortBy .name
                |> List.map viewCrewMember
                |> column
                    [ width fill
                    , spacing 8
                    , padding 8
                    , height fill
                    , scrollbarY
                    ]
            ]
        ]


viewCrewMember : Crew -> Element Msg
viewCrewMember crew =
    el
        [ width fill
        , Border.solid
        , Border.color Gui.Color.light
        , Border.rounded 5
        , Border.widthEach
            { top = 2
            , bottom = 0
            , left = 0
            , right = 2
            }
        ]
        (column
            [ spacing 8
            , padding 8
            , width fill
            , Border.solid
            , Border.color Gui.Color.dark
            , Border.rounded 5
            , Border.widthEach
                { top = 0
                , bottom = 2
                , left = 2
                , right = 0
                }
            ]
            [ text crew.name
            , text ("Morale: " ++ moraleToString crew.morale)
            ]
        )


moraleToString : Int -> String
moraleToString morale =
    if morale > 80 then
        "very high"

    else if morale > 50 then
        "high"

    else if morale > 20 then
        "moderate"

    else if morale > 0 then
        "low"

    else
        "mutinous"


viewEvents : Game -> Element Msg
viewEvents game =
    el
        [ width fill, alignTop, height fill, clipY ]
        (column
            [ width fill, alignTop, scrollbarY ]
            [ paragraph
                [ padding 16 ]
                [ text game.resultOfAction ]
            , viewEvent game game.event
            ]
        )


viewEvent : Game -> Event -> Element Msg
viewEvent game (Event event) =
    el
        [ width fill
        , Border.solid
        , Border.color Gui.Color.light
        , Border.rounded 5
        , Border.widthEach
            { top = 2
            , bottom = 0
            , left = 0
            , right = 2
            }
        ]
        (column
            [ spacing 16
            , padding 16
            , Border.solid
            , Border.color Gui.Color.dark
            , Border.rounded 5
            , Border.widthEach
                { top = 0
                , bottom = 2
                , left = 2
                , right = 0
                }
            , width fill
            ]
            [ paragraph
                []
                [ text (event.description game) ]
            , event.actions
                |> List.map viewAction
                |> wrappedRow [ spacing 16 ]
            ]
        )


viewAction : Applyable -> Element Msg
viewAction (Applyable action) =
    Input.button
        []
        { onPress = Just (ExecuteAction action.apply)
        , label = text action.label
        }
