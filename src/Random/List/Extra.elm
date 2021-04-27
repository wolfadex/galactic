module Random.List.Extra exposing (pick)

import List.NonEmpty exposing (NonEmptyList)
import Random exposing (Generator)


pick : NonEmptyList a -> Generator (NonEmptyList a)
pick ( first, rest ) =
    Random.map
        (\index ->
            if index == 0 then
                ( first, rest )

            else
                ( Maybe.withDefault first (getAt (index - 1) rest)
                , List.take (index - 1) rest ++ List.drop index rest
                )
        )
        (Random.int 0 (List.length rest))


getAt : Int -> List a -> Maybe a
getAt index list =
    list
        |> List.drop index
        |> List.head
