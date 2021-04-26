module Random.Bool exposing (odds)

import Random exposing (Generator)


odds : Float -> Float -> Generator Bool
odds oddsTrue oddsFalse =
    Random.weighted ( oddsTrue, True ) [ ( oddsFalse, False ) ]
