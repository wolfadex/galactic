module Game.Data exposing
    ( Action(..)
    , Applyable(..)
    , Card(..)
    , Deck
    , Event(..)
    , Game
    , Reputation(..)
    , baseEvents
    , emptyEvent
    , nextEvent
    )

import Game.Crew as Crew exposing (Alignment(..), Crew)
import Game.Item exposing (Item)
import List.NonEmpty exposing (NonEmptyList)
import Random exposing (Generator, Seed)
import Random.Bool
import Random.List
import Set exposing (Set)


type alias Game =
    { resultOfAction : String
    , event : Event
    , crew : List Crew
    , rareItems : List Item
    , region : Region
    , availableCards : Set Int
    , availableRegionNames : Set String
    }


type alias Deck =
    List Card


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


type Applyable
    = Applyable
        { label : String
        , apply : Game -> Generator Game
        }


type Event
    = Event
        { id : String
        , description : Game -> String
        , actions : List Applyable
        }


emptyEvent : Event
emptyEvent =
    Event
        { id = ""
        , description = \_ -> ""
        , actions = []
        }


type alias Region =
    { name : String
    , reputation : Reputation
    }


type Reputation
    = Allied
    | Friendly
    | Cordial
    | Neutral
    | Unfriendly
    | Hostile


reputationsWeighted : ( ( Float, Reputation ), List ( Float, Reputation ) )
reputationsWeighted =
    ( ( 10, Allied )
    , [ ( 15, Friendly )
      , ( 20, Cordial )
      , ( 30, Neutral )
      , ( 15, Unfriendly )
      , ( 10, Hostile )
      ]
    )


nextEvent : Game -> Generator Game
nextEvent game =
    Random.List.choose (Set.toList game.availableCards)
        |> Random.andThen
            (\( toPick, remaining ) ->
                let
                    g =
                        { game | availableCards = Set.fromList remaining }
                in
                toPick
                    |> Maybe.andThen (\i -> getAt i baseEvents)
                    |> Maybe.map (\event -> event g)
                    |> Maybe.withDefault (Random.constant ( emptyEvent, g ))
            )
        |> Random.map (\( event, g ) -> { g | event = event })


baseEvents : List (Game -> Generator ( Event, Game ))
baseEvents =
    [ \gameState ->
        Random.map
            (\uniqueName ->
                ( Event
                    { id = "the " ++ uniqueName ++ " pirates of " ++ gameState.region.name
                    , description = \_ -> "A ship of " ++ uniqueName ++ " pirates appears from behind a moon. Before you can react they've come along side your ship and are preparing to board."
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
                                        |> Random.andThen nextEvent
                            }
                        ]
                    }
                , gameState
                )
            )
            (Random.constant "carl")
    ]


pirateBattle : Game -> ( Event, Game )
pirateBattle =
    \gameState ->
        ( Event
            { id = "Engaged with Pirates"
            , description = \_ -> "The battle with the pirates is rough but you come out on top. You lose some of your crew but the pirate loses are much greater. How will you deal with the captives?"
            , actions =
                [ Applyable
                    { label = "Toss Them Into Space"
                    , apply =
                        \game ->
                            nextEvent { game | resultOfAction = "You jetison the captive pirates." }
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
                                |> Random.andThen nextEvent
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



---- DATA ----


regionsNames : NonEmptyList String
regionsNames =
    ( "Carl"
    , [ "Carla"
      , "Carly"
      ]
    )


thingNames : NonEmptyList String
thingNames =
    ( "Florrpion"
    , [ "Genojian"
      , "Kalespiel"
      ]
    )



---- HELPERS ----


setEvent : Event -> Game -> Generator Game
setEvent event game =
    Random.constant { game | event = event }


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
