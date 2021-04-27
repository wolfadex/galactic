module List.NonEmpty exposing
    ( NonEmptyList
    , fromList
    )


type alias NonEmptyList a =
    ( a, List a )


fromList : List a -> Maybe (NonEmptyList a)
fromList xs =
    case xs of
        [] ->
            Nothing

        first :: rest ->
            Just ( first, rest )
