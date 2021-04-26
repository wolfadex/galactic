module Game.Region exposing
    ( Region
    , Reputation(..)
    , baseEvents
    , buildUniverse
    )

import Game.Data exposing (Event(..))
import Random exposing (Generator)
import Random.List.Extra


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


buildUniverse : Generator (List Event)
buildUniverse =
    buildRegions regionsNames []


buildRegions : ( String, List String ) -> List (List Event) -> Generator (List Event)
buildRegions possibleNames regions =
    let
        ( firstReputation, restReputations ) =
            reputationsWeighted
    in
    Random.map2
        (\reputation ( name, remainingNames ) ->
            ( { name = name
              , reputation = reputation
              }
            , remainingNames
            )
        )
        (Random.weighted firstReputation restReputations)
        (Random.List.Extra.pick possibleNames)
        |> Random.andThen
            (\( region, remainingNames ) ->
                Random.map
                    (\events ->
                        ( events
                        , remainingNames
                        )
                    )
                    (buildEvents region baseEvents Game.Data.names [])
            )
        |> Random.andThen
            (\( region, remainingNames ) ->
                case remainingNames of
                    [] ->
                        Random.constant (List.concat (region :: regions))

                    firstName :: restNames ->
                        Random.lazy (\() -> buildRegions ( firstName, restNames ) (region :: regions))
            )


buildEvents : Region -> PossibleEvents -> PickableNames -> List Event -> Generator (List Event)
buildEvents region possibleEvents possibleNames events =
    randomEvent region possibleEvents possibleNames
        |> Random.andThen
            (\( event, remainingEvents, remainingNames ) ->
                case ( remainingEvents, remainingNames ) of
                    ( [], _ ) ->
                        Random.constant (event :: events)

                    ( _, [] ) ->
                        Random.constant (event :: events)

                    ( firstEvent :: restEvents, firstName :: restNames ) ->
                        Random.lazy
                            (\() ->
                                buildEvents region
                                    ( firstEvent, restEvents )
                                    ( firstName, restNames )
                                    (event :: events)
                            )
            )


randomEvent : Region -> PossibleEvents -> PickableNames -> Generator ( Event, List EventBuilder, List String )
randomEvent region possibleEvents possibleNames =
    Random.map2
        (\( event, remainingEvents ) ( name, remainingNames ) ->
            ( event region name
            , remainingEvents
            , remainingNames
            )
        )
        (Random.List.Extra.pick possibleEvents)
        (Random.List.Extra.pick possibleNames)



---- DATA ----


baseEvents : PossibleEvents
baseEvents =
    ( \region uniqueName ->
        Event
            { id = uniqueName ++ "__nebula__in__the__" ++ region.name ++ "region"
            , description = \_ -> ""
            , actions = []
            }
    , [ \region uniqueName ->
            Event
                { id = uniqueName ++ "__nebula__in__the__" ++ region.name ++ "region"
                , description = \_ -> ""
                , actions = []
                }
      ]
    )


regionsNames : PickableNames
regionsNames =
    ( "Carl"
    , [ "Carla"
      , "Carly"
      ]
    )



---- SHORTER TYPE NAMES ----


type alias PossibleEvents =
    ( EventBuilder, List EventBuilder )


type alias EventBuilder =
    Region -> String -> Event


type alias PickableNames =
    ( String, List String )
