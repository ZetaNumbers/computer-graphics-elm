module Camera.Controller exposing (Controller, Delta, Msg, anyKey, asDelta, init, onMouseDown, subscriptions, update)

import Browser.Events exposing (onKeyDown, onKeyUp, onMouseUp)
import Json.Decode as D
import Json.Decode.Extra as DE
import Math.Vector3 exposing (Vec3, vec3)
import Utils exposing (boolAsInt)


type alias Controller =
    { mouseButton : Bool
    , wKey : Bool
    , aKey : Bool
    , sKey : Bool
    , dKey : Bool
    , rKey : Bool
    , fKey : Bool
    , plusKey : Bool
    , minusKey : Bool
    }


init : Controller
init =
    { mouseButton = False
    , wKey = False
    , aKey = False
    , sKey = False
    , dKey = False
    , rKey = False
    , fKey = False
    , plusKey = False
    , minusKey = False
    }


anyKey : Controller -> Bool
anyKey c =
    c.wKey || c.aKey || c.sKey || c.dKey || c.rKey || c.fKey || c.plusKey || c.minusKey


type Msg
    = MouseButton Bool
    | WKey Bool
    | AKey Bool
    | SKey Bool
    | DKey Bool
    | RKey Bool
    | FKey Bool
    | PlusKey Bool
    | MinusKey Bool
    | Other String Bool


msgFromKeyString : String -> Bool -> Msg
msgFromKeyString key =
    case key of
        "w" ->
            WKey

        "a" ->
            AKey

        "s" ->
            SKey

        "d" ->
            DKey

        "r" ->
            RKey

        "f" ->
            FKey

        "+" ->
            PlusKey

        "-" ->
            MinusKey

        other ->
            Other other


update : Msg -> Controller -> Controller
update msg c =
    case msg of
        MouseButton s ->
            { c | mouseButton = s }

        WKey s ->
            { c | wKey = s }

        AKey s ->
            { c | aKey = s }

        SKey s ->
            { c | sKey = s }

        DKey s ->
            { c | dKey = s }

        RKey s ->
            { c | rKey = s }

        FKey s ->
            { c | fKey = s }

        PlusKey s ->
            { c | plusKey = s }

        MinusKey s ->
            { c | minusKey = s }

        Other _ _ ->
            c


type alias Delta =
    { eRadius : Float, dCenter : Vec3 }


asDelta : Controller -> Delta
asDelta c =
    { eRadius = floatFromTwoBools c.plusKey c.minusKey
    , dCenter =
        vec3
            (floatFromTwoBools c.aKey c.dKey)
            (floatFromTwoBools c.fKey c.rKey)
            (floatFromTwoBools c.wKey c.sKey)
    }


floatFromTwoBools : Bool -> Bool -> Float
floatFromTwoBools plus minus =
    toFloat (boolAsInt plus - boolAsInt minus)


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ onKeyDown (DE.andMap (D.succeed True) keyDecoder)
        , onKeyUp (DE.andMap (D.succeed False) keyDecoder)
        , onMouseUp (D.succeed (MouseButton False))
        ]


onMouseDown : Msg
onMouseDown =
    MouseButton True


keyDecoder : D.Decoder (Bool -> Msg)
keyDecoder =
    D.map msgFromKeyString
        (D.field "key" D.string)
