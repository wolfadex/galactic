module Gui.Input exposing (..)

import Element exposing (..)
import Element.Border as Border
import Element.Input as Input


button : List (Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
button additionalAttributes config =
    Input.button
        ([ paddingXY 16 8
         , Border.solid
         , Border.width 3
         ]
            ++ additionalAttributes
        )
        config
