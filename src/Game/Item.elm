module Game.Item exposing
    ( Item(..)
    , toString
    )


type Item
    = BlueCube
    | RedPyramid
    | GreenSphere
    | TransdimensionalObject
    | PuzzleBox


toString : Item -> String
toString item =
    case item of
        BlueCube ->
            "Blue Cube"

        GreenSphere ->
            "Green Sphere"

        RedPyramid ->
            "Red Pyramid"

        TransdimensionalObject ->
            "Transdimensional Object"

        PuzzleBox ->
            "Puzzle Box"
