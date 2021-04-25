module Gui.Input exposing (button)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (shadow)
import Element.Input as Input
import Gui.Color


button : List (Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
button additionalAttributes config =
    Input.button
        ([ Border.solid
         , Border.color Gui.Color.light
         , Border.widthEach
            { top = 3
            , bottom = 0
            , left = 0
            , right = 3
            }
         , mouseOver
            [ Background.color Gui.Color.focused
            , shadow
                { offset = ( 0, 0 )
                , color = Gui.Color.focused
                , size = 2
                , blur = 6
                }
            ]
         ]
            ++ additionalAttributes
        )
        { label =
            el
                [ paddingXY 16 8
                , Border.solid
                , Border.color Gui.Color.dark
                , Border.widthEach
                    { top = 0
                    , bottom = 3
                    , left = 3
                    , right = 0
                    }
                ]
                config.label
        , onPress = config.onPress
        }
