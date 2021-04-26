module Game.Cards exposing
    ( defaultDeck
    , firstCard
    , placeholderCard
    )

import Game.Crew as Crew exposing (Alignment(..), Crew)
import Game.Data exposing (Action(..), Card(..), Game)
import Game.Item exposing (Item(..))
import List.Extra
import Random exposing (Seed)
import Random.Bool
import Random.List


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
            , Action
                { label = "Attempt to Flee"
                , apply =
                    \game seed ->
                        Random.step
                            (Random.andThen
                                (\escapeUnharmed ->
                                    if escapeUnharmed then
                                        Random.constant (discard { game | resultOfAction = "You turn and flee. While the ship takes some minor damage the crew remains safe." })

                                    else
                                        Random.map
                                            (\shuffledCrew ->
                                                discard
                                                    { game
                                                        | resultOfAction = "You turn to flee and the pirates give change. You escape but 4 crew die in the battle."
                                                        , crew = List.drop 4 shuffledCrew
                                                    }
                                            )
                                            (Random.List.shuffle game.crew)
                                )
                                (Random.Bool.odds 80 20)
                            )
                            seed
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
    , Card
        { label = "Floporian Nebula"
        , description = \_ -> "You pass near the unchated Floporian Nebula."
        , actions =
            [ Action
                { label = "Explore the Nebula"
                , apply =
                    \game seed ->
                        Random.step
                            (Random.map
                                (\nextCard ->
                                    discard { game | resultOfAction = "You enter into the Floporian Nebula, what will you find?" }
                                        |> addCard nextCard
                                )
                                (Random.weighted
                                    ( 33, nebulaPirate )
                                    [ ( 33, nebulaPlanet )
                                    , ( 33, nebulaAnomaly )
                                    ]
                                )
                            )
                            seed
                }
            , Action
                { label = "Ignore the Nebula"
                , apply =
                    \game seed ->
                        ( discard { game | resultOfAction = "You continue on your way, ignoring the nebula." }
                        , seed
                        )
                }
            ]
        }
    ]


nebulaPirate : Card
nebulaPirate =
    Card
        { label = "Nebula Pirate"
        , description = \_ -> "As soon as you enter the nebula a pirate ship appears behind you."
        , actions =
            [ Action
                { label = "Attempt to Flee"
                , apply =
                    \game seed ->
                        Random.step
                            (Random.map2
                                (\crewKilled shuffledCrew ->
                                    discard
                                        { game
                                            | resultOfAction = "You manage to escpae but suffer casualties. " ++ String.fromInt crewKilled ++ " of your crew memebrs don't make it."
                                            , crew = List.drop crewKilled shuffledCrew
                                        }
                                )
                                (Random.int 7 16)
                                (Random.List.shuffle game.crew)
                            )
                            seed
                }
            , Action
                { label = "Enage the Pirates"
                , apply =
                    \game seed ->
                        Random.step
                            (Random.map2
                                (\crewKilled shuffledCrew ->
                                    discard
                                        { game
                                            | resultOfAction = "You turn and engage the pirates head on. This surprises them and you win the battle. Sadly " ++ String.fromInt crewKilled ++ " of your crew don't survive."
                                            , crew = List.drop crewKilled shuffledCrew
                                        }
                                )
                                (Random.int 3 9)
                                (Random.List.shuffle game.crew)
                            )
                            seed
                }
            ]
        }


nebulaPlanet : Card
nebulaPlanet =
    Card
        { label = "Nebula Planet"
        , description = \_ -> "After traveling through the nebula for what feels like forever you come across a strange looking planet. It seems habitable, but looks nothing like any habitble planet you've ever seen before."
        , actions =
            [ Action
                { label = "Explore the Planet"
                , apply =
                    \game seed ->
                        ( discard
                            { game
                                | resultOfAction = "You take down a small contingent to explore."
                            }
                            |> addCard exploreNebulaPlanet
                        , seed
                        )
                }
            , Action
                { label = "Leave the Planet Be"
                , apply =
                    \game seed ->
                        ( discard
                            { game
                                | resultOfAction = "You decide it's not worth the risk and head back through the nebula. Leaving the planet for someone else to explore."
                            }
                        , seed
                        )
                }
            ]
        }


exploreNebulaPlanet : Card
exploreNebulaPlanet =
    Card
        { label = "Explore Nebula Planet"
        , description = \_ -> "The planet is covered in tree like things that unlike plants seem to move ever so slowly. There's also a plethora of other creatures of various shapes and size, all moving incrediblly slowly."
        , actions =
            [ Action
                { label = "Delve into the Forest"
                , apply =
                    \game seed ->
                        ( discard
                            { game
                                | resultOfAction = "After hours of hiking you come across a small home that all of the creatures seem to move around. Inside is an old woman."
                            }
                            |> addCard oldWomanInNebula
                        , seed
                        )
                }
            , Action
                { label = "Leave the Planet"
                , apply =
                    \game seed ->
                        ( discard
                            { game
                                | resultOfAction = "You find nothing of interest and decide to leave. Returning back through the nebula."
                            }
                        , seed
                        )
                }
            ]
        }


oldWomanInNebula : Card
oldWomanInNebula =
    Card
        { label = "Old Woman in Nebula"
        , description = \_ -> "The old woman invites you in. She says it's been many years since she's had visitors. She asks if you'd like to stay for dinner?"
        , actions =
            [ Action
                { label = "Stay for Dinner"
                , apply =
                    \game seed ->
                        ( discard
                            { game
                                | resultOfAction = "You accept the offer of dinner. After her many stories the old woman gives you a parting gift. A small puzzle box. You take it graciously, then return to your ship and through the nebula."
                                , rareItems = PuzzleBox :: game.rareItems
                            }
                        , seed
                        )
                }
            , Action
                { label = "Decline Dinner"
                , apply =
                    \game seed ->
                        ( discard
                            { game
                                | resultOfAction = "You let her know that you must be on your way, heading back to your ship and out fo the nebula."
                            }
                        , seed
                        )
                }
            ]
        }


nebulaAnomaly : Card
nebulaAnomaly =
    Card
        { label = "Nebula Anomaly"
        , description = \_ -> "As you venture through the nebula, your crew starts to behave strangely. More and more of them start acting like children. Slowly regressing emotionally."
        , actions =
            [ Action
                { label = "Continue Forward"
                , apply =
                    \game seed ->
                        ( discard
                            { game
                                | resultOfAction = "You push forward through the nebula. Attending to the ill as best you can."
                            }
                            |> addCard (pushThroughAnamoly 0)
                        , seed
                        )
                }
            , Action
                { label = "Leave the Nebula"
                , apply =
                    \game seed ->
                        ( discard
                            { game
                                | resultOfAction = "This odd behavior poses too much of a risk. You turn and leave the nebula as fast as you can. As soon as you exit the nebula everyone begins to very quickly return to normal."
                            }
                        , seed
                        )
                }
            ]
        }


pushThroughAnamoly : Int -> Card
pushThroughAnamoly timesPushedThrough =
    if timesPushedThrough == 3 then
        Card
            { label = "Hiiden in the Nebula"
            , description = \_ -> "After what seems like seems like forever you find the ship in a void in the nebula. At the center is an alien looking vessel."
            , actions =
                [ Action
                    { label = "Board the Vessel"
                    , apply =
                        \game seed ->
                            ( discard
                                { game
                                    | resultOfAction = "You approach and board the strange vessel."
                                }
                                |> addCard boardAlienVesselInNebula
                            , seed
                            )
                    }
                , Action
                    { label = "Leave the Nebula"
                    , apply =
                        \game seed ->
                            Random.step
                                (Random.map2
                                    (\crewKilled shuffledCrew ->
                                        discard
                                            { game
                                                | resultOfAction = "You turn and leave the nebula as fast as you can, though your actions may have been too late. Due to the deterioration of the crew a few accidents occur, killing " ++ String.fromInt crewKilled ++ " of the crew. Thankfully when you finally make it out, the rest of the crew returns to normal."
                                                , crew =
                                                    List.drop crewKilled shuffledCrew
                                                        |> List.map (Crew.modifyMoral (-2 * timesPushedThrough) ChaoticNeutral)
                                            }
                                    )
                                    (Random.int timesPushedThrough (timesPushedThrough + 3))
                                    (Random.List.shuffle game.crew)
                                )
                                seed
                    }
                ]
            }

    else
        Card
            { label = "Push Through Nebula Anomaly"
            , description = \_ -> "Pushing thorugh the nebula takes a toll on the crew. Many have now reverted to being infants and are unable to do their duty. You still have no clue as to the cause."
            , actions =
                [ Action
                    { label = "Continue Forward"
                    , apply =
                        \game seed ->
                            ( discard
                                { game
                                    | resultOfAction = "You push forward through the nebula. Attending to the ill as best you can."
                                }
                                |> addCard (pushThroughAnamoly (timesPushedThrough + 1))
                            , seed
                            )
                    }
                , Action
                    { label = "Leave the Nebula"
                    , apply =
                        \game seed ->
                            Random.step
                                (Random.map2
                                    (\crewKilled shuffledCrew ->
                                        discard
                                            { game
                                                | resultOfAction = "You turn and leave the nebula as fast as you can, though your actions may have been too late. Due to the deterioration of the crew a few accidents occur, killing " ++ String.fromInt crewKilled ++ " of the crew. Thankfully when you finally make it out, the rest of the crew returns to normal."
                                                , crew =
                                                    List.drop crewKilled shuffledCrew
                                                        |> List.map (Crew.modifyMoral (-2 * timesPushedThrough) ChaoticNeutral)
                                            }
                                    )
                                    (Random.int timesPushedThrough (timesPushedThrough + 3))
                                    (Random.List.shuffle game.crew)
                                )
                                seed
                    }
                ]
            }


boardAlienVesselInNebula : Card
boardAlienVesselInNebula =
    Card
        { label = "Board Alien Nebula Vessel"
        , description = \_ -> "You take a small group into the alien vessel. After finding their way through various hallways they find a vault of sorts. Inside the valut is a bizzare object that seems to fade in and out of existence, changing shape as it does."
        , actions =
            [ Action
                { label = "Attempt to take the Object"
                , apply =
                    \game seed ->
                        ( discard
                            { game
                                | resultOfAction = "You reach out and grab the object. As you do it seems to solidy in our reality. You then head back to your ship and out of the nebula. You notice a bubble around the ship, the objecy must be protexting the ship from the nebula."
                                , rareItems = TransdimensionalObject :: game.rareItems
                            }
                        , seed
                        )
                }
            , Action
                { label = "Leave the Object and Nebula"
                , apply =
                    \game seed ->
                        ( { game
                            | resultOfAction = "You return to your ship and leave the nebula as fast as you can. The journey into and out of the nebula takes a heavy toll on the crew."
                            , crew =
                                List.map
                                    (Crew.modifyMoral -20 NeutralEvil)
                                    game.crew
                          }
                        , seed
                        )
                }
            ]
        }


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
