module Canvas exposing (Canvas, Msg, init, update, view, viewOptions)

import Array
import Camera exposing (Camera)
import Camera.Controller as Controller
import Canvas.MeshOptions exposing (MeshOption, optionList, toOption)
import Element exposing (Element, text)
import Element.Events exposing (onMouseDown)
import Element.Input as Input
import Html.Attributes exposing (height, style, width)
import Http
import Length
import Maybe.Extra as MaybeE
import MeshMask exposing (MeshMask)
import Obj.Decode
import Result.Extra as ResultE
import Shaders exposing (difFragmentShader, vertexShader, whiteFragmentShader)
import TriangularMesh exposing (TriangularMesh, vertices)
import Utils
import Vertex exposing (VertexGl, VertexObj)
import WebGL exposing (entityWith)
import WebGL.Settings exposing (cullFace)
import WebGL.Settings.DepthTest


type alias Canvas =
    { option : MeshOption
    , inner : Result String SubMeshes
    }


init : ( Canvas, Cmd Msg )
init =
    ( { inner = Err "Uninitialized"
      , option = Canvas.MeshOptions.init
      }
    , fetch Canvas.MeshOptions.init
    )


type Msg
    = GotMesh Canvas
    | FetchMesh MeshOption


update : Msg -> Canvas -> ( Canvas, Cmd Msg )
update msg mesh =
    case msg of
        FetchMesh opt ->
            ( mesh, fetch opt )

        GotMesh newMesh ->
            ( newMesh, Cmd.none )


fetch : MeshOption -> Cmd Msg
fetch opt =
    Http.get
        { url = Canvas.MeshOptions.toPath opt
        , expect =
            Obj.Decode.expectObj
                (ResultE.mapBoth Utils.httpErrorToString toGL
                    >> Canvas opt
                    >> GotMesh
                )
                Length.meters
                Obj.Decode.faces
        }


type alias MeshObj =
    TriangularMesh VertexObj


type alias SubMeshes =
    { faces : WebGL.Mesh VertexGl
    , edges : WebGL.Mesh VertexGl
    , vertices : WebGL.Mesh VertexGl
    }


toGL : MeshObj -> SubMeshes
toGL mesh =
    let
        vertices =
            TriangularMesh.vertices mesh
                |> Array.map Vertex.toGL
                |> Array.toList
    in
    { faces =
        WebGL.indexedTriangles
            vertices
            (TriangularMesh.faceIndices mesh)
    , edges =
        WebGL.lines
            (TriangularMesh.edgeVertices mesh
                |> List.map (Tuple.mapBoth Vertex.toGL Vertex.toGL)
            )
    , vertices =
        WebGL.points
            vertices
    }


view : Camera -> MeshMask -> Canvas -> Element Controller.Msg
view camera mask mesh =
    let
        entity fragmentShader submesh () =
            WebGL.entity
                vertexShader
                fragmentShader
                submesh
                { perspective = Camera.asPerspective camera }

        entityWithCulling fragmentShader submesh () =
            WebGL.entityWith
                [ cullFace WebGL.Settings.back
                , WebGL.Settings.DepthTest.default
                ]
                vertexShader
                fragmentShader
                submesh
                { perspective = Camera.asPerspective camera }
    in
    case mesh.inner of
        Err error ->
            text error

        Ok inner ->
            WebGL.toHtmlWith
                [ WebGL.clearColor 0.0 0.0 0.0 1.0
                , WebGL.depth 1.0
                ]
                [ width 600
                , height 600
                , style "display" "block"
                ]
                (MaybeE.values
                    [ Utils.boolThen mask.faces
                        (entityWithCulling difFragmentShader inner.faces)
                    , Utils.boolThen mask.edges
                        (entity whiteFragmentShader inner.edges)
                    , Utils.boolThen mask.vertices
                        (entity whiteFragmentShader inner.vertices)
                    ]
                )
                |> Element.html
                |> Element.el [ onMouseDown Controller.onMouseDown ]


viewOptions : Canvas -> Element Msg
viewOptions canvas =
    Input.radio []
        { selected = Just canvas.option
        , label = Input.labelAbove [] (text "Mesh")
        , options =
            optionList
                |> List.map toOption
        , onChange = FetchMesh
        }
