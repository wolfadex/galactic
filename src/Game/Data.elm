module Game.Data exposing
    ( Applyable(..)
    , Event(..)
    , Game
    , Region
    , Reputation(..)
    , colors
    , regionsNames
    , reputationsWeighted
    , shapes3d
    , thingNames
    )

import Game.Crew exposing (Crew)
import List.NonEmpty exposing (NonEmptyList)
import Random exposing (Generator)
import Set exposing (Set)


type alias Game =
    { resultOfAction : String
    , event : Event
    , crew : List Crew
    , rareItems : List String
    , region : Region
    , availableCards : Set Int
    , availableRegionNames : NonEmptyList String
    , availableThingNames : NonEmptyList ( String, String )
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
    ( "Sector 4"
    , [ "Parhelion"
      , "Pantalaimon"
      , "Giskard"
      ]
    )


thingNames : NonEmptyList ( String, String )
thingNames =
    ( ( "Floporian", "Floporians" )
    , [ ( "Genojian", "Genojians" )
      , ( "Kalespiel", "Kalespielians" )
      , ( "Korgall", "Korgallians" )
      ]
    )


colors : NonEmptyList String
colors =
    ( "red"
    , [ "green"
      , "blue"
      , "orange"
      , "magenta"
      , "magenta"
      , "pink"
      , "gray"
      , "black"
      , "white"
      , "Fuchsia"
      ]
    )


shapes3d : NonEmptyList String
shapes3d =
    ( "sphere"
    , [ "cube"
      , "pyramid"
      , "capsule"
      , "cone"
      , "icosahedron"
      , "cylinder"
      , "tube"
      , "torus"
      ]
    )



-- shapes2d : NonEmptyList String
-- shapes2d =
--     ( "circle"
--     , [ "dot"
--       , "line"
--       , "triangle"
--       , "square"
--       , "hexagon"
--       ]
--     )
-- pattern : NonEmptyList String
-- pattern =
--     ( "solid"
--     , [ "striped"
--       , "dotted"
--       , "splotchy"
--       , "hatched"
--       ]
--     )
