module Game.Data exposing
    ( Action(..)
    , Card(..)
    , Deck
    , Event(..)
    , Game
    , names
    )

import Game.Crew exposing (Crew)
import Game.Item exposing (Item)
import Random exposing (Seed)


type alias Game =
    { deck : Deck
    , discardDeck : Deck
    , resultOfAction : String
    , crew : List Crew
    , rareItems : List Item
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


type Event
    = Event
        { id : String
        , description : Game -> String
        , actions : List Action
        }


names : ( String, List String )
names =
    ( "Florrpion"
    , [ "Genojian"
      , "Kalespiel"
      ]
    )
