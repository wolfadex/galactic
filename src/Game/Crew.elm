module Game.Crew exposing
    ( Alignment(..)
    , Crew
    , alignmentToString
    , modifyMoral
    , moraleMaximum
    , moraleMinimum
    , random
    )

import Random


type alias Crew =
    { name : String
    , morale : Int
    , alignment : Alignment
    }


type Alignment
    = LawfulGood
    | NeutralGood
    | ChaoticGood
    | LawfulNeutral
    | TrueNeutral
    | ChaoticNeutral
    | LawfulEvil
    | NeutralEvil
    | ChaoticEvil


alignmentToString : Alignment -> String
alignmentToString alignment =
    case alignment of
        LawfulGood ->
            "LawfulGood"

        NeutralGood ->
            "NeutralGood"

        ChaoticGood ->
            "ChaoticGood"

        LawfulNeutral ->
            "LawfulNeutral"

        TrueNeutral ->
            "TrueNeutral"

        ChaoticNeutral ->
            "ChaoticNeutral"

        LawfulEvil ->
            "LawfulEvil"

        NeutralEvil ->
            "NeutralEvil"

        ChaoticEvil ->
            "ChaoticEvil"


moraleMinimum : Int
moraleMinimum =
    0


moraleMaximum : Int
moraleMaximum =
    100


random : { moraleMin : Int, moraleMax : Int, alignmentsWeighted : ( ( Float, Alignment ), List ( Float, Alignment ) ) } -> Random.Generator Crew
random { moraleMin, moraleMax, alignmentsWeighted } =
    let
        ( firstName, restNames ) =
            personNames

        ( firstAlignment, restAlignments ) =
            alignmentsWeighted
    in
    Random.map3
        (\name morale alignment ->
            { name = name
            , morale = morale
            , alignment = alignment
            }
        )
        (Random.weighted ( 5, 1 ) [ ( 25, 2 ), ( 40, 3 ), ( 25, 4 ), ( 5, 5 ) ]
            |> Random.andThen (\numNames -> Random.list numNames (Random.uniform firstName restNames))
            |> Random.map (String.join " ")
        )
        (Random.int moraleMin moraleMax)
        (Random.weighted firstAlignment restAlignments)


modifyMoral : Int -> Alignment -> Crew -> Crew
modifyMoral amount actionAlignment crew =
    let
        modifier =
            alignmentModifier actionAlignment crew.alignment
    in
    { crew | morale = min 100 (floor (modifier * toFloat amount) + crew.morale) }


alignmentModifier : Alignment -> Alignment -> Float
alignmentModifier alignment1 alignment2 =
    case ( alignment1, alignment2 ) of
        -- LawfulGood
        ( LawfulGood, LawfulGood ) ->
            2

        ( LawfulGood, NeutralGood ) ->
            1.5

        ( LawfulGood, ChaoticGood ) ->
            1

        ( LawfulGood, LawfulNeutral ) ->
            1

        ( LawfulGood, TrueNeutral ) ->
            0.75

        ( LawfulGood, ChaoticNeutral ) ->
            0.75

        ( LawfulGood, LawfulEvil ) ->
            -0.25

        ( LawfulGood, NeutralEvil ) ->
            -0.25

        ( LawfulGood, ChaoticEvil ) ->
            -0.25

        -- NeutralGood
        ( NeutralGood, LawfulGood ) ->
            1.75

        ( NeutralGood, NeutralGood ) ->
            2

        ( NeutralGood, ChaoticGood ) ->
            1.75

        ( NeutralGood, LawfulNeutral ) ->
            1.5

        ( NeutralGood, TrueNeutral ) ->
            1

        ( NeutralGood, ChaoticNeutral ) ->
            1.5

        ( NeutralGood, LawfulEvil ) ->
            0.5

        ( NeutralGood, NeutralEvil ) ->
            0.75

        ( NeutralGood, ChaoticEvil ) ->
            0.5

        -- ChaoticGood
        ( ChaoticGood, LawfulGood ) ->
            1.5

        ( ChaoticGood, NeutralGood ) ->
            1.75

        ( ChaoticGood, ChaoticGood ) ->
            2

        ( ChaoticGood, LawfulNeutral ) ->
            1.5

        ( ChaoticGood, TrueNeutral ) ->
            1.25

        ( ChaoticGood, ChaoticNeutral ) ->
            1.5

        ( ChaoticGood, LawfulEvil ) ->
            -0.25

        ( ChaoticGood, NeutralEvil ) ->
            0

        ( ChaoticGood, ChaoticEvil ) ->
            0.25

        -- LawfulNeutral
        ( LawfulNeutral, LawfulGood ) ->
            1.5

        ( LawfulNeutral, NeutralGood ) ->
            1.25

        ( LawfulNeutral, ChaoticGood ) ->
            1

        ( LawfulNeutral, LawfulNeutral ) ->
            2

        ( LawfulNeutral, TrueNeutral ) ->
            1.75

        ( LawfulNeutral, ChaoticNeutral ) ->
            1.25

        ( LawfulNeutral, LawfulEvil ) ->
            0.25

        ( LawfulNeutral, NeutralEvil ) ->
            0

        ( LawfulNeutral, ChaoticEvil ) ->
            -0.25

        -- TrueNeutral
        ( TrueNeutral, LawfulGood ) ->
            1

        ( TrueNeutral, NeutralGood ) ->
            1.25

        ( TrueNeutral, ChaoticGood ) ->
            1

        ( TrueNeutral, LawfulNeutral ) ->
            1.25

        ( TrueNeutral, TrueNeutral ) ->
            1.5

        ( TrueNeutral, ChaoticNeutral ) ->
            1.25

        ( TrueNeutral, LawfulEvil ) ->
            1

        ( TrueNeutral, NeutralEvil ) ->
            1.25

        ( TrueNeutral, ChaoticEvil ) ->
            1

        -- ChaoticNeutral
        ( ChaoticNeutral, LawfulGood ) ->
            0.75

        ( ChaoticNeutral, NeutralGood ) ->
            1

        ( ChaoticNeutral, ChaoticGood ) ->
            1.25

        ( ChaoticNeutral, LawfulNeutral ) ->
            1

        ( ChaoticNeutral, TrueNeutral ) ->
            1.25

        ( ChaoticNeutral, ChaoticNeutral ) ->
            1.5

        ( ChaoticNeutral, LawfulEvil ) ->
            0.75

        ( ChaoticNeutral, NeutralEvil ) ->
            1

        ( ChaoticNeutral, ChaoticEvil ) ->
            1.25

        -- LawfulEvil
        ( LawfulEvil, LawfulGood ) ->
            0.5

        ( LawfulEvil, NeutralGood ) ->
            0

        ( LawfulEvil, ChaoticGood ) ->
            -0.25

        ( LawfulEvil, LawfulNeutral ) ->
            0.5

        ( LawfulEvil, TrueNeutral ) ->
            0.25

        ( LawfulEvil, ChaoticNeutral ) ->
            0

        ( LawfulEvil, LawfulEvil ) ->
            2

        ( LawfulEvil, NeutralEvil ) ->
            1.75

        ( LawfulEvil, ChaoticEvil ) ->
            1.25

        -- NeutralEvil
        ( NeutralEvil, LawfulGood ) ->
            0.25

        ( NeutralEvil, NeutralGood ) ->
            0

        ( NeutralEvil, ChaoticGood ) ->
            0.25

        ( NeutralEvil, LawfulNeutral ) ->
            0.5

        ( NeutralEvil, TrueNeutral ) ->
            0.75

        ( NeutralEvil, ChaoticNeutral ) ->
            0.5

        ( NeutralEvil, LawfulEvil ) ->
            1.5

        ( NeutralEvil, NeutralEvil ) ->
            2

        ( NeutralEvil, ChaoticEvil ) ->
            1.5

        -- ChaoticEvil
        ( ChaoticEvil, LawfulGood ) ->
            -0.25

        ( ChaoticEvil, NeutralGood ) ->
            0

        ( ChaoticEvil, ChaoticGood ) ->
            0.25

        ( ChaoticEvil, LawfulNeutral ) ->
            0.25

        ( ChaoticEvil, TrueNeutral ) ->
            0.75

        ( ChaoticEvil, ChaoticNeutral ) ->
            1

        ( ChaoticEvil, LawfulEvil ) ->
            1.25

        ( ChaoticEvil, NeutralEvil ) ->
            1.5

        ( ChaoticEvil, ChaoticEvil ) ->
            2



---- DATA ----


personNames : ( String, List String )
personNames =
    ( "Carl"
    , [ "Bekah"
      , "Sam"
      , "Carey"
      , "Laura"
      , "Tim"
      , "Tom"
      , "Alma"
      , "Flynn"
      , "Ash"
      , "Sean"
      , "Lance"
      , "Samantha"
      , "Raechel"
      , "Lisa"
      , "Kelby"
      , "Vaughn"
      , "Clarence"
      , "Joanna"
      , "Dorothy"
      , "Isaiah"
      , "Ibram"
      , "Elizabeth"
      , "Liz"
      , "Jeroen"
      , "Richard"
      , "Robert"
      , "Thomas"
      , "William"
      , "Sharon"
      , "Julian"
      , "James"
      , "Megan"
      , "Alex"
      , "Alexander"
      , "Shane"
      , "Ashley"
      ]
    )
