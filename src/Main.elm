module Main exposing (main)

import Browser exposing (Document)
import Element exposing (..)
import Element.Border as Border
import Game.Crew as Crew exposing (Alignment(..), Crew)
import Gui.Color
import Gui.Input as Input
import List.Extra
import Random exposing (Seed)
import Random.List


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- TYPES ----


type Model
    = Initializing
    | Playing Seed Game


type alias Game =
    { deck : List Card
    , discardDeck : List Card
    , resultOfAction : String
    , crew : List Crew
    , rareItems : List Item
    }


type Item
    = BlueCube
    | RedPyramid
    | GreenSphere


type Card
    = Card
        { label : String
        , description : Game -> String
        , actions : List Action
        }


type Action
    = Action
        { label : String
        , apply : Game -> Seed -> ( Game, Seed )
        }



---- INIT ----


init : () -> ( Model, Cmd Msg )
init () =
    ( Initializing
    , Random.independentSeed
        |> Random.generate Initialize
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- UPDATE ----


type Msg
    = Initialize Seed
    | PlayCard Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Initialize seed, Initializing ) ->
            let
                ( ( deck, crew ), nextSeed ) =
                    Random.step
                        (Random.map2 Tuple.pair
                            (Random.List.shuffle defaultDeck)
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
            ( Playing nextSeed
                { deck =
                    firstCard
                        :: deck
                        ++ [ Card
                                { label = "placeholder label"
                                , description = \_ -> "placeholder description"
                                , actions = []
                                }
                           ]
                , discardDeck = []
                , resultOfAction = "You board your ship"
                , crew = crew
                , rareItems = []
                }
            , Cmd.none
            )

        ( PlayCard index, Playing seed game ) ->
            let
                ( nextGame, nextSeed ) =
                    List.head game.deck
                        |> Maybe.andThen (\(Card { actions }) -> List.Extra.getAt index actions)
                        |> Maybe.map (\(Action { apply }) -> apply game seed)
                        |> Maybe.withDefault ( game, seed )
            in
            ( Playing nextSeed nextGame
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


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
        Initializing ->
            text "Initializing..."

        Playing _ game ->
            viewGame game


viewGame : Game -> Element Msg
viewGame game =
    row
        [ spacing 16, width fill, height fill ]
        [ viewState game
        , viewDeck game
        ]


viewState : Game -> Element Msg
viewState game =
    column
        [ height fill, spacing 16, width fill ]
        [ viewCrew game ]


viewCrew : Game -> Element Msg
viewCrew state =
    row
        [ spacing 16
        , width fill
        , height fill
        ]
        [ row
            [ alignTop, spacing 16 ]
            []
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


viewDeck : Game -> Element Msg
viewDeck game =
    column
        [ width fill, alignTop ]
        [ paragraph
            [ padding 16 ]
            [ text game.resultOfAction ]
        , case game.deck of
            [] ->
                text "deck is empty"

            card :: _ ->
                viewCard game card
        ]


viewCard : Game -> Card -> Element Msg
viewCard game (Card card) =
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
                [ text (card.description game) ]
            , card.actions
                |> List.indexedMap viewAction
                |> wrappedRow [ spacing 16 ]
            ]
        )


viewAction : Int -> Action -> Element Msg
viewAction index (Action action) =
    Input.button
        []
        { onPress = Just (PlayCard index)
        , label = text action.label
        }



---- CARDS ----


discard : Game -> Game
discard game =
    { game
        | deck = List.drop 1 game.deck
        , discardDeck = List.take 1 game.deck ++ game.discardDeck
    }


addCard : Card -> Game -> Game
addCard card game =
    { game | deck = card :: game.deck }


firstCard : Card
firstCard =
    Card
        { label = "Launch Ship"
        , description = \_ -> "Begin your voyage"
        , actions =
            [ Action
                { label = "Launch"
                , apply =
                    \game seed ->
                        ( discard { game | resultOfAction = "Your journey into the great beyond beigns" }
                        , seed
                        )
                }
            ]
        }


defaultDeck : List Card
defaultDeck =
    [ Card
        { label = "Viral Outbreak"
        , description =
            \{ crew } ->
                let
                    crewCount =
                        List.length crew
                in
                "A viral outbreak is spreading rapidly through the ship. 30% ("
                    ++ (((crewCount |> toFloat) * 0.3)
                            |> floor
                            |> String.fromInt
                       )
                    ++ " of "
                    ++ String.fromInt crewCount
                    ++ ") of the crew is infected and the virus is killing 25% of those infected."
        , actions =
            [ Action
                { label = "Do Nothing"
                , apply =
                    \game seed ->
                        let
                            crewSize =
                                toFloat (List.length game.crew)

                            ( newCrew, nextSeed ) =
                                killPercentCrew { min = 0.25, max = 0.35 } game.crew seed
                        in
                        ( discard
                            { game
                                | resultOfAction =
                                    "You let the virus run its course and you end up losing "
                                        ++ (100
                                                - (toFloat (List.length newCrew) * 100 / crewSize)
                                                |> floor
                                                |> String.fromInt
                                           )
                                        ++ "% of the crew."
                                , crew = newCrew
                            }
                        , nextSeed
                        )
                }
            , Action
                { label = "Prioritize a Vaccine"
                , apply =
                    \game seed ->
                        let
                            crewSize =
                                toFloat (List.length game.crew)

                            ( newCrew, nextSeed ) =
                                killPercentCrew { min = 0.15, max = 0.25 } game.crew seed
                        in
                        ( discard
                            { game
                                | resultOfAction =
                                    "You emphasize that the doctors work on a cure and end up losing "
                                        ++ (100
                                                - (toFloat (List.length newCrew) * 100 / crewSize)
                                                |> floor
                                                |> String.fromInt
                                           )
                                        ++ "% of the crew. There's a slight boost to morale."
                                , crew = List.map (Crew.modifyMoral 5 NeutralGood) newCrew
                            }
                        , nextSeed
                        )
                }
            , Action
                { label = "Quarantine the Infected"
                , apply =
                    \game seed ->
                        let
                            crewSize =
                                toFloat (List.length game.crew)

                            ( newCrew, nextSeed ) =
                                killPercentCrew { min = 0.5, max = 0.15 } game.crew seed
                        in
                        ( discard
                            { game
                                | resultOfAction =
                                    "You have the sick stictly quarantined, only losing "
                                        ++ ((toFloat (List.length newCrew) * 100 / crewSize)
                                                |> floor
                                                |> String.fromInt
                                           )
                                        ++ "% of the crew but also decreasing morale."
                                , crew = List.map (Crew.modifyMoral -5 LawfulGood) newCrew
                            }
                        , nextSeed
                        )
                }
            ]
        }
    , Card
        { label = "Pirates"
        , description = \_ -> "A ship of pirates appears from behind a moon. Before you can react they've come along side your ship and are preparing to board."
        , actions =
            [ Action
                { label = "Challenge the Pirates"
                , apply =
                    \game seed ->
                        ( discard { game | resultOfAction = "You engage wiht the pirates, but who will come out the victor?" }
                            |> addCard pirateBattle
                        , seed
                        )
                }
            ]
        }
    , Card
        { label = "Strange Planet"
        , description = \_ -> "After traveling for some time you come across a lush, habitible planet."
        , actions =
            [ Action
                { label = "Pass it By"
                , apply =
                    \game seed ->
                        ( discard { game | resultOfAction = "You continue on your journey, ignoring the planet." }
                        , seed
                        )
                }
            , Action
                { label = "Explore The Surface"
                , apply =
                    \game seed ->
                        ( discard { game | resultOfAction = "You send a small group downw to the planet." }
                            |> addCard exploreStrangePlanet
                        , seed
                        )
                }
            ]
        }
    , Card
        { label = "Planet Korgall"
        , description = \_ -> "You make stop at planet Korgall for supplies."
        , actions =
            [ Action
                { label = "Resupply and Continue"
                , apply =
                    \game seed ->
                        let
                            hasGreenSphere =
                                List.Extra.find
                                    (\item ->
                                        case item of
                                            GreenSphere ->
                                                True

                                            _ ->
                                                False
                                    )
                                    game.rareItems
                                    |> Maybe.map (\_ -> True)
                                    |> Maybe.withDefault False
                        in
                        if hasGreenSphere then
                            Random.step
                                (Random.map
                                    (\shuffledCrew ->
                                        discard
                                            { game
                                                | resultOfAction = "While restocking your ship the Korgallians see the green sphere relic and react in rage, attacking your crew. You manage to make it out of there but lose 2 crew members. You later learn that this device was a weapon built to destroy the Korgallians and they thought you were attempting to kill them."
                                                , crew = List.drop 2 shuffledCrew
                                            }
                                    )
                                    (Random.List.shuffle game.crew)
                                )
                                seed

                        else
                            ( discard { game | resultOfAction = "You restock the ship and continue on your journey." }
                            , seed
                            )
                }
            ]
        }
    ]


exploreStrangePlanet : Card
exploreStrangePlanet =
    Card
        { label = "Strange Planet Surface"
        , description = \_ -> "During your exploration you find some ancient ruins."
        , actions =
            [ Action
                { label = "Investigate the Ruins"
                , apply =
                    \game seed ->
                        ( discard { game | resultOfAction = "You delve deeper into the ruins." }
                            |> addCard exploreRuins
                        , seed
                        )
                }
            , Action
                { label = "Leave Planet"
                , apply =
                    \game seed ->
                        ( discard { game | resultOfAction = "You leave the planet and contiue on your journey." }
                        , seed
                        )
                }
            ]
        }


exploreRuins : Card
exploreRuins =
    Card
        { label = "Inside the Ruins"
        , description = \_ -> "You come across a small room with 3 pedestals. On top of each you see 3 ancient relics. The one of the left is pyramid shaped with some red markings. The one in the middle is a sphere with vertical green lines. The last is a solid blue cube."
        , actions =
            [ Action
                { label = "Leave the Relics"
                , apply =
                    \game seed ->
                        ( discard { game | resultOfAction = "You delve deeper into the ruins and the relics." }
                        , seed
                        )
                }
            , Action
                { label = "Take the Red Pyramid"
                , apply =
                    \game seed ->
                        ( discard
                            { game
                                | resultOfAction = "You take the red pyramid back to the ship."
                                , rareItems = RedPyramid :: game.rareItems
                            }
                        , seed
                        )
                }
            , Action
                { label = "Take the Green Sphere"
                , apply =
                    \game seed ->
                        ( discard
                            { game
                                | resultOfAction = "You take the green sphere back to the ship."
                                , rareItems = GreenSphere :: game.rareItems
                            }
                        , seed
                        )
                }
            , Action
                { label = "Take the Blue Cube"
                , apply =
                    \game seed ->
                        ( discard
                            { game
                                | resultOfAction = "You take the blue cube back to the ship."
                                , rareItems = BlueCube :: game.rareItems
                            }
                        , seed
                        )
                }
            ]
        }


pirateBattle : Card
pirateBattle =
    Card
        { label = "Engaged with Pirates"
        , description = \_ -> "The battle with the pirates is rough but you come out on top. You lose some of your crew but the pirate loses are much greater. How will you deal with the captives?"
        , actions =
            [ Action
                { label = "Toss Them Into Space"
                , apply =
                    \game seed ->
                        ( discard { game | resultOfAction = "You jetison the captive pirates." }
                        , seed
                        )
                }
            , Action
                { label = "Have Them Join You"
                , apply =
                    \game seed ->
                        let
                            ( newCrewMembers, nextSeed ) =
                                Random.step
                                    (Random.list 5
                                        (Crew.random
                                            { moraleMin = 30
                                            , moraleMax = 40
                                            , alignmentsWeighted = pirateAlignments
                                            }
                                        )
                                    )
                                    seed
                        in
                        ( discard
                            { game
                                | resultOfAction = "You offer them a chance to join your crew. "
                                , crew = newCrewMembers ++ List.map (Crew.modifyMoral -2 LawfulNeutral) game.crew
                            }
                        , nextSeed
                        )
                }
            ]
        }


pirateAlignments : ( ( Float, Alignment ), List ( Float, Alignment ) )
pirateAlignments =
    ( ( 15, ChaoticGood )
    , [ ( 15, TrueNeutral )
      , ( 10, ChaoticNeutral )
      , ( 20, LawfulEvil )
      , ( 20, NeutralEvil )
      , ( 20, ChaoticEvil )
      ]
    )


killPercentCrew : { min : Float, max : Float } -> List Crew -> Seed -> ( List Crew, Seed )
killPercentCrew amount crew seed =
    let
        crewSize =
            toFloat (List.length crew)
    in
    Random.step
        (Random.map2
            (\percentToKill shuffledCrew ->
                let
                    numberToKill =
                        floor (percentToKill * crewSize)
                in
                List.drop numberToKill shuffledCrew
            )
            (Random.float amount.min amount.max)
            (Random.List.shuffle crew)
        )
        seed
