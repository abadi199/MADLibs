module Button exposing (..)

import Element as E
import Element.Border as Border
import Element.Input as Input


button : String -> msg -> E.Element msg
button label msg =
    Input.button
        [ Border.rounded 5
        , Border.solid
        , Border.width 2
        , Border.color (E.rgba 0 0 0 0.5)
        , E.paddingXY 10 5
        ]
        { onPress = Just msg
        , label = E.text label
        }
