module ParsedVec exposing (..)

import Element exposing (Attribute, Element, column, rgb, text, width)
import Element.Font as Font
import Element.Input as Input
import Html.Events.Extra exposing (onChange)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type RevisionedValue a
    = Actual a
    | Old a


reviseValue : Maybe a -> RevisionedValue a -> RevisionedValue a
reviseValue v rev =
    case v of
        Just new ->
            Actual new

        Nothing ->
            rev


unwrapResisionedValue : RevisionedValue a -> a
unwrapResisionedValue rev =
    case rev of
        Actual v ->
            v

        Old v ->
            v


type alias ParsedFloat =
    { value : RevisionedValue Float
    , fallback : String
    }


fromFloat : Float -> ParsedFloat
fromFloat x =
    { value = Actual x
    , fallback = String.fromFloat x
    }


lastActualParsedFloat : ParsedFloat -> Float
lastActualParsedFloat p =
    unwrapResisionedValue p.value


parse : ParsedFloat -> String -> ParsedFloat
parse old x =
    { value = reviseValue (String.toFloat x) old.value
    , fallback = x
    }


viewParsedFloat :
    { label : Input.Label msg
    , value : ParsedFloat
    , placeholder : Maybe (Input.Placeholder msg)
    , onChange : ParsedFloat -> msg
    }
    -> Element msg
viewParsedFloat { label, value, placeholder, onChange } =
    Input.text
        [ width
            (Element.px 100
            )
        , Font.color
            (case value.value of
                Actual _ ->
                    rgb 0.0 0.0 0.0

                Old _ ->
                    rgb 1.0 0.4 0.4
            )
        ]
        { label = label
        , text = value.fallback
        , placeholder = placeholder
        , onChange = parse value >> onChange
        }


type alias ParsedVec3 =
    { x : ParsedFloat
    , y : ParsedFloat
    , z : ParsedFloat
    }


fromVec3 : Vec3 -> ParsedVec3
fromVec3 v =
    { x = fromFloat (Vec3.getX v)
    , y = fromFloat (Vec3.getY v)
    , z = fromFloat (Vec3.getZ v)
    }


lastActualParsedVec3 : ParsedVec3 -> Vec3
lastActualParsedVec3 p =
    vec3 (lastActualParsedFloat p.x)
        (lastActualParsedFloat p.y)
        (lastActualParsedFloat p.z)


viewParsedVec3 :
    List (Attribute msg)
    ->
        { label : Element msg
        , value : ParsedVec3
        , onChange : ParsedVec3 -> msg
        }
    -> Element msg
viewParsedVec3 attrs { label, value, onChange } =
    column attrs
        [ label
        , viewParsedFloat
            { label = Input.labelLeft [] (text "x")
            , value = value.x
            , placeholder = Just (Input.placeholder [] (text "0.0"))
            , onChange = \e -> onChange { value | x = e }
            }
        , viewParsedFloat
            { label = Input.labelLeft [] (text "y")
            , value = value.y
            , placeholder = Just (Input.placeholder [] (text "0.0"))
            , onChange = \e -> onChange { value | y = e }
            }
        , viewParsedFloat
            { label = Input.labelLeft [] (text "z")
            , value = value.z
            , placeholder = Just (Input.placeholder [] (text "0.0"))
            , onChange = \e -> onChange { value | z = e }
            }
        ]
