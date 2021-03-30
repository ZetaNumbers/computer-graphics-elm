module Camera exposing (Camera, Msg(..), asPerspective, init, subscriptions, update)

import Browser.Events exposing (onAnimationFrameDelta, onMouseMove)
import Camera.Controller as Controller exposing (Controller)
import Json.Decode as D
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type alias Camera =
    { radius : Float
    , eyeAngle : Vec2
    , center : Vec3
    , controller : Controller
    }


init : Camera
init =
    { radius = 4.0
    , eyeAngle = vec2 0.0 0.0
    , center = vec3 0.0 0.0 0.0
    , controller = Controller.init
    }


applyDelta : Float -> Controller.Delta -> Camera -> Camera
applyDelta dt d camera =
    { camera
        | radius = camera.radius * (e ^ (-dt / 1000.0 * d.eRadius))
        , center =
            Vec3.add camera.center
                (Vec3.scale (dt / 1000.0 * camera.radius) d.dCenter
                    |> Mat4.transform (Mat4.makeRotate -(Vec2.getX camera.eyeAngle) Vec3.j)
                )
    }


type Msg
    = ControllerMsg Controller.Msg
    | Tick Float
    | RotateEye Vec2


update : Msg -> Camera -> Camera
update message camera =
    case message of
        RotateEye duv ->
            { camera | eyeAngle = Vec2.add camera.eyeAngle (Vec2.scale 0.01 duv) }

        Tick dt ->
            applyDelta dt (Controller.asDelta camera.controller) camera

        ControllerMsg msg ->
            { camera | controller = Controller.update msg camera.controller }


asPerspective : Camera -> Mat4
asPerspective { radius, eyeAngle, center } =
    Mat4.makePerspective 45 1 0.01 100
        |> Mat4.translate3 0.0 0.0 -radius
        |> Mat4.rotate (Vec2.getY eyeAngle) Vec3.i
        |> Mat4.rotate (Vec2.getX eyeAngle) Vec3.j
        |> Mat4.translate center


subscriptions : Camera -> Sub Msg
subscriptions camera =
    Sub.batch
        [ if camera.controller.mouseButton then
            onMouseMove
                (D.map RotateEye decodeMovement)

          else
            Sub.none
        , if Controller.anyKey camera.controller then
            onAnimationFrameDelta Tick

          else
            Sub.none
        , Controller.subscriptions
            |> Sub.map ControllerMsg
        ]


decodeMovement : D.Decoder Vec2
decodeMovement =
    D.map2 vec2
        (D.field "movementX" D.float)
        (D.field "movementY" D.float)
