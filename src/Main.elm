module Main exposing (main)

import Browser
import Camera exposing (Camera)
import Canvas exposing (Canvas)
import Element exposing (Element, column, paddingXY, row, spacing)
import MeshMask exposing (MeshMask)



---- MODEL ----


type alias Model =
    { canvas : Canvas
    , mask : MeshMask
    , camera : Camera
    }


init : ( Model, Cmd Msg )
init =
    ( { canvas = Tuple.first Canvas.init
      , mask = MeshMask.init
      , camera = Camera.init
      }
    , Tuple.second Canvas.init
        |> Cmd.map CanvasMsg
    )



---- UPDATE ----


type Msg
    = CanvasMsg Canvas.Msg
    | MaskMsg MeshMask.Msg
    | CameraMsg Camera.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        CameraMsg msg ->
            ( { model | camera = Camera.update msg model.camera }, Cmd.none )

        CanvasMsg msg ->
            let
                newMesh =
                    Canvas.update msg model.canvas
            in
            ( { model | canvas = Tuple.first newMesh }, Tuple.second newMesh |> Cmd.map CanvasMsg )

        MaskMsg msg ->
            ( { model | mask = MeshMask.update msg model.mask }, Cmd.none )



---- VIEW ----


view : Model -> Element Msg
view { camera, mask, canvas } =
    row []
        [ column [ paddingXY 20 10, spacing 10 ]
            [ Canvas.viewOptions canvas
                |> Element.map CanvasMsg
            , MeshMask.view mask
                |> Element.map MaskMsg
            ]
        , Canvas.view camera mask canvas
            |> Element.map Camera.ControllerMsg
            |> Element.map CameraMsg
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Camera.subscriptions model.camera
        |> Sub.map CameraMsg



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> Element.layout []
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
