module Set.NonEmpty exposing
    ( NonEmptySet
    , fromNonEmptyList
    , singleton
    , toNonEmptyList
    )

import List.NonEmpty exposing (NonEmptyList)
import Set exposing (Set)


type alias NonEmptySet comparable =
    ( comparable, Set comparable )


singleton : comparable -> NonEmptySet comparable
singleton a =
    ( a, Set.empty )


toNonEmptyList : NonEmptySet comparable -> NonEmptyList comparable
toNonEmptyList ( first, rest ) =
    ( first, Set.toList rest )


fromNonEmptyList : NonEmptyList comparable -> Maybe (NonEmptySet comparable)
fromNonEmptyList ( first, rest ) =
    let
        restSet =
            Set.fromList rest
    in
    if Set.member first restSet then
        case Set.toList restSet of
            [] ->
                Nothing

            finalFirst :: finalRest ->
                Just ( finalFirst, Set.fromList finalRest )

    else
        Just ( first, restSet )
