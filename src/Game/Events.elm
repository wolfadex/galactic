module Game.Events exposing
    ( firstEvent
    , initialSetOfAvailableEvents
    )

import Game.Crew as Crew exposing (Alignment(..), Crew)
import Game.Data exposing (Applyable(..), Event(..), Game)
import List.NonEmpty
import Random exposing (Generator)
import Random.Bool
import Random.Extra
import Random.List
import Random.List.Extra
import Set exposing (Set)
import String.Extra


baseEvents : List (Game -> Generator ( Event, Game ))
baseEvents =
    [ \gameState ->
        Random.constant
            ( Event
                { id = "Viral Outbreak"
                , description =
                    let
                        crewCount : Int
                        crewCount =
                            List.length gameState.crew
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
                    [ Applyable
                        { label = "Do Nothing"
                        , apply =
                            \game ->
                                let
                                    crewSize : Float
                                    crewSize =
                                        toFloat (List.length game.crew)
                                in
                                Random.map
                                    (\newCrew ->
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
                                    )
                                    (killPercentCrew { min = 0.25, max = 0.35 } game.crew)
                                    |> Random.andThen randomEvent
                        }
                    , Applyable
                        { label = "Prioritize a Vaccine"
                        , apply =
                            \game ->
                                let
                                    crewSize : Float
                                    crewSize =
                                        toFloat (List.length game.crew)
                                in
                                Random.map
                                    (\newCrew ->
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
                                    )
                                    (killPercentCrew { min = 0.15, max = 0.25 } game.crew)
                                    |> Random.andThen randomEvent
                        }
                    , Applyable
                        { label = "Quarantine the Infected"
                        , apply =
                            \game ->
                                let
                                    crewSize : Float
                                    crewSize =
                                        toFloat (List.length game.crew)
                                in
                                Random.map
                                    (\newCrew ->
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
                                    )
                                    (killPercentCrew { min = 0.5, max = 0.15 } game.crew)
                                    |> Random.andThen randomEvent
                        }
                    ]
                }
            , gameState
            )
    , \gameState ->
        Random.constant
            ( Event
                { id = "pirates of " ++ gameState.region.name
                , description = "A ship of " ++ gameState.region.name ++ " pirates appears from behind a moon. Before you can react they've come along side your ship and are preparing to board."
                , actions =
                    [ Applyable
                        { label = "Challenge the Pirates"
                        , apply =
                            \game ->
                                let
                                    ( event, g ) =
                                        pirateBattle
                                            { game
                                                | resultOfAction = "You engage with the pirates, but who will come out the victor?"
                                            }
                                in
                                setEvent event g
                        }
                    , Applyable
                        { label = "Attempt to Flee"
                        , apply =
                            \game ->
                                Random.Bool.odds 80 20
                                    |> Random.andThen
                                        (\escapeUnharmed ->
                                            if escapeUnharmed then
                                                Random.constant { game | resultOfAction = "You turn and flee. While the ship takes some minor damage the crew remains safe." }

                                            else
                                                Random.map
                                                    (\shuffledCrew ->
                                                        { game
                                                            | resultOfAction = "You turn to flee and the pirates give change. You escape but 4 crew die in the battle."
                                                            , crew = List.drop 4 shuffledCrew
                                                        }
                                                    )
                                                    (Random.List.shuffle game.crew)
                                        )
                                    |> Random.andThen randomEvent
                        }
                    ]
                }
            , gameState
            )
    , \gameState ->
        Random.constant
            ( Event
                { id = "Strange Planet"
                , description = "After traveling for some time you come across a lush, habitible planet."
                , actions =
                    [ Applyable
                        { label = "Pass it By"
                        , apply =
                            setResult "You continue on your journey, ignoring the planet." >> randomEvent
                        }
                    , Applyable
                        { label = "Explore The Surface"
                        , apply =
                            setResult "You send a small group downw to the planet." >> setEvent exploreStrangePlanet
                        }
                    ]
                }
            , gameState
            )
    , \gameState ->
        Random.map
            (\( ( name, pluralName ), remainingNames ) ->
                ( Event
                    { id = "Planet " ++ name
                    , description = "You make stop at planet " ++ name ++ " for supplies."
                    , actions =
                        [ Applyable
                            { label = "Resupply and Continue"
                            , apply =
                                \game ->
                                    let
                                        hasGreenSphere : Bool
                                        hasGreenSphere =
                                            List.member "green sphere" game.rareItems
                                    in
                                    if hasGreenSphere then
                                        Random.andThen
                                            (\shuffledCrew ->
                                                randomEvent
                                                    { game
                                                        | resultOfAction = "While restocking your ship the " ++ pluralName ++ " see the Green Sphere relic and react in rage, attacking your crew. You manage to make it out of there but lose 2 crew members. You later learn that this device was a weapon built to destroy the " ++ pluralName ++ " and they thought you were attempting to kill them."
                                                        , crew =
                                                            List.drop 2 shuffledCrew
                                                                |> List.map (Crew.modifyMoral -3 ChaoticEvil)
                                                        , rareItems =
                                                            List.filter ((/=) "green sphere") game.rareItems
                                                    }
                                            )
                                            (Random.List.shuffle game.crew)

                                    else
                                        randomEvent { game | resultOfAction = "You restock the ship and continue on your journey." }
                            }
                        ]
                    }
                , { gameState
                    | availableProperNouns =
                        List.NonEmpty.fromList remainingNames
                            |> Maybe.withDefault Game.Data.properNouns
                  }
                )
            )
            (Random.List.Extra.pick gameState.availableProperNouns)
    , \gameState ->
        Random.map
            (\( ( name, _ ), remainingNames ) ->
                ( Event
                    { id = name ++ " Nebula"
                    , description = "You pass near the unchated " ++ name ++ " Nebula."
                    , actions =
                        [ Applyable
                            { label = "Explore the Nebula"
                            , apply =
                                \game ->
                                    Random.andThen
                                        (\event ->
                                            setEvent event (setResult ("You enter into the " ++ name ++ " Nebula, what will you find?") game)
                                        )
                                        (Random.weighted
                                            ( 34, nebulaPirate )
                                            [ ( 33, nebulaPlanet )
                                            , ( 33, nebulaAnomaly )
                                            ]
                                        )
                            }
                        , Applyable
                            { label = "Ignore the Nebula"
                            , apply =
                                setResult "You continue on your way, ignoring the nebula." >> randomEvent
                            }
                        ]
                    }
                , { gameState
                    | availableProperNouns =
                        List.NonEmpty.fromList remainingNames
                            |> Maybe.withDefault Game.Data.properNouns
                  }
                )
            )
            (Random.List.Extra.pick gameState.availableProperNouns)
    , \gameState ->
        Random.map
            (\( names, remainingNames ) ->
                ( Event
                    { id = "energy being"
                    , description = "Your ship travels through what looks like a gaseous cloud. Suddenly a ball of energy appears on the bridge of the ship."
                    , actions =
                        [ Applyable
                            { label = "Shoot It"
                            , apply =
                                \game ->
                                    Random.andThen
                                        (\shuffledCrew ->
                                            randomEvent
                                                { game
                                                    | resultOfAction = "You pull out your weapon and fire at the ball of energy. Your weapon does noting. The ball of energy just absords the blast. In the blink of an eye it jumps between a few of your crew, zapping and killing them, before disappearing completely."
                                                    , crew = List.drop 3 shuffledCrew
                                                }
                                        )
                                        (Random.List.shuffle game.crew)
                            }
                        , Applyable
                            { label = "Try to Communicate"
                            , apply =
                                setResult "You slowly approach the ball and try to talk with it."
                                    >> setEvent (talkToEnergyAlien names)
                            }
                        ]
                    }
                , { gameState
                    | availableAlienNames =
                        List.NonEmpty.fromList remainingNames
                            |> Maybe.withDefault Game.Data.alienNames
                  }
                )
            )
            (Random.List.Extra.pick gameState.availableAlienNames)
    ]


talkToEnergyAlien : String -> Event
talkToEnergyAlien name =
    Event
        { id = "talk to energy being"
        , description = "After a pause the ball of energy responds in a voice that sounds like crackling air. It introduces itself as " ++ name ++ ". Explaining that they're just here to experiment on you."
        , actions =
            [ Applyable
                { label = "Demand They Leave"
                , apply =
                    setResult ("As quickly as they appeard " ++ name ++ " disappears.")
                        >> randomEvent
                }
            , Applyable
                { label = "Offer to Help"
                , apply =
                    setResult (name ++ " loves your enthusiasm. It turns out that all they wanted was to find out how friendly you were. With this they quickly depart.")
                        >> randomEvent
                }
            ]
        }


nebulaPirate : Event
nebulaPirate =
    Event
        { id = "Nebula Pirate"
        , description = "As soon as you enter the nebula a pirate ship appears behind you."
        , actions =
            [ Applyable
                { label = "Attempt to Flee"
                , apply =
                    \game ->
                        Random.map2
                            (\crewKilled shuffledCrew ->
                                { game
                                    | resultOfAction = "You manage to escpae but suffer casualties. " ++ String.fromInt crewKilled ++ " of your crew memebrs don't make it."
                                    , crew = List.drop crewKilled shuffledCrew
                                }
                            )
                            (Random.int 7 16)
                            (Random.List.shuffle game.crew)
                            |> Random.andThen randomEvent
                }
            , Applyable
                { label = "Enage the Pirates"
                , apply =
                    \game ->
                        Random.map2
                            (\crewKilled shuffledCrew ->
                                { game
                                    | resultOfAction = "You turn and engage the pirates head on. This surprises them and you win the battle. Sadly " ++ String.fromInt crewKilled ++ " of your crew don't survive."
                                    , crew = List.drop crewKilled shuffledCrew
                                }
                            )
                            (Random.int 3 9)
                            (Random.List.shuffle game.crew)
                            |> Random.andThen randomEvent
                }
            ]
        }


nebulaPlanet : Event
nebulaPlanet =
    Event
        { id = "Nebula Planet"
        , description = "After traveling through the nebula for what feels like forever you come across a strange looking planet. It seems habitable, but looks nothing like any habitble planet you've ever seen before."
        , actions =
            [ Applyable
                { label = "Explore the Planet"
                , apply =
                    setResult "You take down a small contingent to explore."
                        >> setEvent exploreNebulaPlanet
                }
            , Applyable
                { label = "Leave the Planet Be"
                , apply =
                    setResult "You decide it's not worth the risk and head back through the nebula. Leaving the planet for someone else to explore."
                        >> randomEvent
                }
            ]
        }


exploreNebulaPlanet : Event
exploreNebulaPlanet =
    Event
        { id = "Explore Nebula Planet"
        , description = "The planet is covered in tree like things that unlike plants seem to move ever so slowly. There's also a plethora of other creatures of various shapes and size, all moving incrediblly slowly."
        , actions =
            [ Applyable
                { label = "Delve into the Forest"
                , apply =
                    \game ->
                        Random.andThen
                            (\( name, remainingNames ) ->
                                setEvent (oldWomanInNebula name)
                                    { game
                                        | resultOfAction = "After hours of hiking you come across a small home that all of the creatures seem to move around. Inside is an old woman."
                                        , availableAlienNames =
                                            List.NonEmpty.fromList remainingNames
                                                |> Maybe.withDefault Game.Data.alienNames
                                    }
                            )
                            (Random.List.Extra.pick game.availableAlienNames)
                }
            , Applyable
                { label = "Leave the Planet"
                , apply =
                    setResult "You find nothing of interest and decide to leave. Returning back through the nebula."
                        >> randomEvent
                }
            ]
        }


oldWomanInNebula : String -> Event
oldWomanInNebula name =
    Event
        { id = "Old Woman in Nebula"
        , description = "The old woman invites you in, intorducing herself as " ++ name ++ ". She says it's been many years since she's had visitors. She asks if you'd like to stay for dinner?"
        , actions =
            [ Applyable
                { label = "Stay for Dinner"
                , apply =
                    \game ->
                        randomEvent
                            { game
                                | resultOfAction = "You accept the offer of dinner. After her many stories, " ++ name ++ " gives you a parting gift. A small puzzle box. You take it graciously, then return to your ship and through the nebula."
                                , rareItems = "puzzle box" :: game.rareItems
                            }
                }
            , Applyable
                { label = "Decline Dinner"
                , apply =
                    setResult
                        ("You let "
                            ++ name
                            ++ " know that you must be on your way, heading back to your ship and out of the nebula."
                        )
                        >> randomEvent
                }
            ]
        }


nebulaAnomaly : Event
nebulaAnomaly =
    Event
        { id = "Nebula Anomaly"
        , description = "As you venture through the nebula, your crew starts to behave strangely. More and more of them start acting like children. Slowly regressing emotionally."
        , actions =
            [ Applyable
                { label = "Continue Forward"
                , apply =
                    setResult "You push forward through the nebula. Attending to the ill as best you can."
                        >> setEvent (pushThroughAnamoly 0)
                }
            , Applyable
                { label = "Leave the Nebula"
                , apply =
                    setResult "This odd behavior poses too much of a risk. You turn and leave the nebula as fast as you can. As soon as you exit the nebula everyone begins to very quickly return to normal."
                        >> randomEvent
                }
            ]
        }


pushThroughAnamoly : Int -> Event
pushThroughAnamoly timesPushedThrough =
    if timesPushedThrough == 3 then
        Event
            { id = "Hiiden in the Nebula"
            , description = "After what seems like seems like forever you find the ship in a void in the nebula. At the center is an alien looking vessel."
            , actions =
                [ Applyable
                    { label = "Board the Vessel"
                    , apply =
                        setResult "You approach and board the strange vessel."
                            >> setEvent boardAlienVesselInNebula
                    }
                , Applyable
                    { label = "Leave the Nebula"
                    , apply =
                        \game ->
                            Random.map2
                                (\crewKilled shuffledCrew ->
                                    { game
                                        | resultOfAction = "You turn and leave the nebula as fast as you can, though your actions may have been too late. Due to the deterioration of the crew a few accidents occur, killing " ++ String.fromInt crewKilled ++ " of the crew. Thankfully when you finally make it out, the rest of the crew returns to normal."
                                        , crew =
                                            List.drop crewKilled shuffledCrew
                                                |> List.map (Crew.modifyMoral (-2 * timesPushedThrough) ChaoticNeutral)
                                    }
                                )
                                (Random.int timesPushedThrough (timesPushedThrough + 3))
                                (Random.List.shuffle game.crew)
                                |> Random.andThen randomEvent
                    }
                ]
            }

    else
        Event
            { id = "Push Through Nebula Anomaly"
            , description =
                if timesPushedThrough == 0 then
                    "Pushing thorugh the nebula takes a toll on the crew. Many have now reverted to being infants and are unable to do their duty. You still have no clue as to the cause."

                else if timesPushedThrough == 1 then
                    "You continue onwards, straining the limits of the crew. Even more have now reverted to being infants and are unable to do their duty. You still have no clue as to the cause."

                else if timesPushedThrough == 2 then
                    "The crew is starting to get unruly from the strain. Very many are now unable to do their duty. You still have no clue as to the cause."

                else
                    "You're not sure if the vrew can go any furthur, do you dare go on? You still have no clue as to the cause."
            , actions =
                [ Applyable
                    { label = "Continue Forward"
                    , apply =
                        setResult "You push forward through the nebula. Attending to the ill as best you can."
                            >> setEvent (pushThroughAnamoly (timesPushedThrough + 1))
                    }
                , Applyable
                    { label = "Leave the Nebula"
                    , apply =
                        \game ->
                            Random.map2
                                (\crewKilled shuffledCrew ->
                                    { game
                                        | resultOfAction = "You turn and leave the nebula as fast as you can, though your actions may have been too late. Due to the deterioration of the crew a few accidents occur, killing " ++ String.fromInt crewKilled ++ " of the crew. Thankfully when you finally make it out, the rest of the crew returns to normal."
                                        , crew =
                                            List.drop crewKilled shuffledCrew
                                                |> List.map (Crew.modifyMoral (-2 * timesPushedThrough) ChaoticNeutral)
                                    }
                                )
                                (Random.int timesPushedThrough (timesPushedThrough + 3))
                                (Random.List.shuffle game.crew)
                                |> Random.andThen randomEvent
                    }
                ]
            }


boardAlienVesselInNebula : Event
boardAlienVesselInNebula =
    Event
        { id = "Board Alien Nebula Vessel"
        , description = "You take a small group into the alien vessel. After finding their way through various hallways they find a vault of sorts. Inside the valut is a bizzare object that seems to fade in and out of existence, changing shape as it does."
        , actions =
            [ Applyable
                { label = "Attempt to take the Object"
                , apply =
                    \game ->
                        randomEvent
                            { game
                                | resultOfAction = "You reach out and grab the object. As you do it seems to solidy in our reality. You then head back to your ship and out of the nebula. You notice a bubble around the ship, the objecy must be protexting the ship from the nebula."
                                , rareItems = "transdimensional object" :: game.rareItems
                            }
                }
            , Applyable
                { label = "Leave the Object and Nebula"
                , apply =
                    \game ->
                        randomEvent
                            { game
                                | resultOfAction = "You return to your ship and leave the nebula as fast as you can. The journey into and out of the nebula takes a heavy toll on the crew."
                                , crew = List.map (Crew.modifyMoral -20 NeutralEvil) game.crew
                            }
                }
            ]
        }


exploreStrangePlanet : Event
exploreStrangePlanet =
    Event
        { id = "Strange Planet Surface"
        , description = "During your exploration you find some ancient ruins."
        , actions =
            [ Applyable
                { label = "Investigate the Ruins"
                , apply =
                    \game ->
                        Random.andThen
                            (\event ->
                                setResult "You delve deeper into the ruins." game
                                    |> setEvent event
                            )
                            exploreRuins
                }
            , Applyable
                { label = "Leave Planet"
                , apply = setResult "You leave the planet and contiue on your journey." >> randomEvent
                }
            ]
        }


exploreRuins : Generator Event
exploreRuins =
    Random.map3
        (\relic1 relic2 relic3 ->
            Event
                { id = "Inside the Ruins"
                , description =
                    "You come across a small room with 3 pedestals. On top of each you see 3 ancient relics. The one of the left is "
                        ++ relic1
                        ++ ". The one in the middle is a "
                        ++ relic2
                        ++ ". The last is a "
                        ++ relic3
                        ++ "."
                , actions =
                    [ Applyable
                        { label = "Leave the Relics"
                        , apply = setResult "You delve deeper into the ruins and the relics." >> randomEvent
                        }
                    , Applyable
                        { label = "Take the " ++ String.Extra.toTitleCase relic1
                        , apply =
                            \game ->
                                randomEvent
                                    { game
                                        | resultOfAction = "You take the " ++ relic1 ++ " back to the ship."
                                        , rareItems = relic1 :: game.rareItems
                                    }
                        }
                    , Applyable
                        { label = "Take the " ++ String.Extra.toTitleCase relic2
                        , apply =
                            \game ->
                                randomEvent
                                    { game
                                        | resultOfAction = "You take the " ++ relic2 ++ " back to the ship."
                                        , rareItems = relic2 :: game.rareItems
                                    }
                        }
                    , Applyable
                        { label = "Take the " ++ String.Extra.toTitleCase relic3
                        , apply =
                            \game ->
                                randomEvent
                                    { game
                                        | resultOfAction = "You take the " ++ relic3 ++ " back to the ship."
                                        , rareItems = relic3 :: game.rareItems
                                    }
                        }
                    ]
                }
        )
        coloredShape3d
        coloredShape3d
        coloredShape3d


coloredShape3d : Generator String
coloredShape3d =
    Random.map2
        (\( color, _ ) ( shape, _ ) ->
            color ++ " " ++ shape
        )
        (Random.List.Extra.pick Game.Data.colors)
        (Random.List.Extra.pick Game.Data.shapes3d)


pirateBattle : Game -> ( Event, Game )
pirateBattle =
    \gameState ->
        ( Event
            { id = "Engaged with Pirates"
            , description = "The battle with the pirates is rough but you come out on top. You lose some of your crew but the pirate loses are much greater. How will you deal with the captives?"
            , actions =
                [ Applyable
                    { label = "Toss Them Into Space"
                    , apply =
                        \game ->
                            randomEvent { game | resultOfAction = "You jetison the captive pirates." }
                    }
                , Applyable
                    { label = "Have Them Join You"
                    , apply =
                        \game ->
                            Random.list 5
                                (Crew.random
                                    { moraleMin = 30
                                    , moraleMax = 40
                                    , alignmentsWeighted = pirateAlignments
                                    }
                                )
                                |> Random.map
                                    (\newCrewMembers ->
                                        { game
                                            | resultOfAction = "You offer them a chance to join your crew. "
                                            , crew = newCrewMembers ++ List.map (Crew.modifyMoral -2 LawfulNeutral) game.crew
                                        }
                                    )
                                |> Random.andThen randomEvent
                    }
                ]
            }
        , gameState
        )


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


setEvent : Event -> Game -> Generator Game
setEvent event game =
    Random.constant { game | event = event }


setResult : String -> Game -> Game
setResult result game =
    { game | resultOfAction = result }


killPercentCrew : { min : Float, max : Float } -> List Crew -> Generator (List Crew)
killPercentCrew amount crew =
    let
        crewSize : Float
        crewSize =
            toFloat (List.length crew)
    in
    Random.map2
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


randomEvent : Game -> Generator Game
randomEvent game =
    Random.List.choose (Set.toList game.availableCards)
        |> Random.andThen
            (\( toPick, remaining ) ->
                let
                    remainingAvailableEvents : Set Int
                    remainingAvailableEvents =
                        Set.fromList remaining

                    actualRemaining : Set Int
                    actualRemaining =
                        if Set.isEmpty remainingAvailableEvents then
                            initialSetOfAvailableEvents

                        else
                            remainingAvailableEvents
                in
                Random.andThen
                    (\travelToNewRegion ->
                        if travelToNewRegion then
                            Random.map2
                                (\( name, remainingNames ) reputation ->
                                    ( toPick
                                    , { game
                                        | region = { name = name, reputation = reputation }
                                        , availableRegionNames =
                                            List.NonEmpty.fromList remainingNames
                                                |> Maybe.withDefault Game.Data.regionsNames
                                        , availableCards = actualRemaining
                                      }
                                    )
                                )
                                (Random.List.Extra.pick game.availableRegionNames)
                                (Random.List.Extra.weighted Game.Data.reputationsWeighted)

                        else
                            Random.constant ( toPick, { game | availableCards = actualRemaining } )
                    )
                    (Random.Extra.oneIn (List.length remaining * 2))
            )
        |> Random.andThen
            (\( toPick, g ) ->
                toPick
                    |> Maybe.andThen (\i -> getAt i baseEvents)
                    |> Maybe.map (\event -> event g)
                    |> Maybe.withDefault (Random.constant ( emptyEvent, g ))
            )
        |> Random.map (\( event, g ) -> { g | event = event })


getAt : Int -> List a -> Maybe a
getAt index xs =
    if index < 0 then
        Nothing

    else
        case xs of
            [] ->
                Nothing

            first :: rest ->
                if index == 0 then
                    Just first

                else
                    getAt (index - 1) rest


emptyEvent : Event
emptyEvent =
    Event
        { id = ""
        , description = "hodor"
        , actions = []
        }


firstEvent : Event
firstEvent =
    Event
        { id = "Launch Ship"
        , description = "Begin your voyage"
        , actions =
            [ Applyable
                { label = "Launch"
                , apply =
                    \game ->
                        randomEvent { game | resultOfAction = "Your journey into the great beyond beigns" }
                }
            ]
        }


initialSetOfAvailableEvents : Set Int
initialSetOfAvailableEvents =
    Set.fromList (List.range 0 (List.length baseEvents - 1))
