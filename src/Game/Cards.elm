module Game.Cards exposing
    ( defaultDeck
    , firstCard
    , placeholderCard
    )

import Game.Crew as Crew exposing (Alignment(..))
import Game.Data exposing (Action(..), Card(..))
import List.Extra
import Random
import Random.List
import Game.Item exposing (Item(..))
import Game.Data exposing (Game)
import Random exposing (Seed)
import Game.Crew exposing (Crew)


placeholderCard : Card
placeholderCard =
    Card
        { label = "placeholder label"
        , description = \_ -> "placeholder description"
        , actions = []
        }


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
                    crewCount : Int
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
                            crewSize : Float
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
                            crewSize : Float
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
                            crewSize : Float
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
                            hasGreenSphere : Bool
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
                                                , crew =
                                                    List.drop 2 shuffledCrew
                                                        |> List.map (Crew.modifyMoral -3 ChaoticEvil)
                                                , rareItems =
                                                    List.filter
                                                        (\item ->
                                                            case item of
                                                                GreenSphere ->
                                                                    False

                                                                _ ->
                                                                    True
                                                        )
                                                        game.rareItems
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



---- HELPERS ----


discard : Game -> Game
discard game =
    { game
        | deck = List.drop 1 game.deck
        , discardDeck = List.take 1 game.deck ++ game.discardDeck
    }


addCard : Card -> Game -> Game
addCard card game =
    { game | deck = card :: game.deck }


killPercentCrew : { min : Float, max : Float } -> List Crew -> Seed -> ( List Crew, Seed )
killPercentCrew amount crew seed =
    let
        crewSize : Float
        crewSize =
            toFloat (List.length crew)
    in
    Random.step
        (Random.map2
            (\percentToKill shuffledCrew ->
                let
                    numberToKill : Int
                    numberToKill =
                        floor (percentToKill * crewSize)
                in
                List.drop numberToKill shuffledCrew
            )
            (Random.float amount.min amount.max)
            (Random.List.shuffle crew)
        )
        seed
