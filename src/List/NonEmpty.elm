module List.NonEmpty exposing
    ( NonEmptyList
    , fromList
    , getAt
    , length
    , singleton
    , toList
    )


type alias NonEmptyList a =
    ( a, List a )


singleton : a -> NonEmptyList a
singleton a =
    ( a, [] )


toList : NonEmptyList a -> List a
toList ( first, rest ) =
    first :: rest


fromList : List a -> Maybe (NonEmptyList a)
fromList xs =
    case xs of
        [] ->
            Nothing

        first :: rest ->
            Just ( first, rest )


length : NonEmptyList a -> Int
length ( _, rest ) =
    List.length rest + 1


getAt : Int -> NonEmptyList a -> Maybe a
getAt index ( first, rest ) =
    if index == 0 then
        Just first

    else
        getAtHelper (index - 1) rest


getAtHelper : Int -> List a -> Maybe a
getAtHelper index xs =
    case xs of
        [] ->
            Nothing

        first :: rest ->
            if index == 0 then
                Just first

            else
                getAtHelper (index - 1) rest
