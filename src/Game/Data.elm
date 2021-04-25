module Game.Data exposing
    ( Action(..)
    , Card(..)
    , Game
    )

import Game.Crew exposing (Crew)
import Game.Item exposing (Item)
import Random exposing (Seed)


type alias Game =
    { deck : List Card
    , discardDeck : List Card
    , resultOfAction : String
    , crew : List Crew
    , rareItems : List Item
    }


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
