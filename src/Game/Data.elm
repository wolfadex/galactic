module Game.Data exposing
    ( Applyable(..)
    , Event(..)
    , Game
    , Reputation(..)
    , regionsNames
    , reputationsWeighted
    , thingNames
    )

import Game.Crew exposing (Alignment(..), Crew)
import Game.Item exposing (Item)
import List.NonEmpty exposing (NonEmptyList)
import Random exposing (Generator)
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
