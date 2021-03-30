module MeshMask exposing (MeshMask, Msg, init, update, view)

import Element exposing (Element, column, text)
import Element.Input as Input


type alias MeshMask =
    { faces : Bool
    , edges : Bool
    , vertices : Bool
    }


init : MeshMask
init =
    { faces = True
    , edges = False
    , vertices = False
    }


type Msg
    = SetFaces Bool
    | SetEdges Bool
    | SetVertices Bool


update : Msg -> MeshMask -> MeshMask
update msg mask =
    case msg of
        SetFaces o ->
            { mask | faces = o }

        SetEdges o ->
            { mask | edges = o }

        SetVertices o ->
            { mask | vertices = o }


view : MeshMask -> Element Msg
view mask =
    let
        checkbox : (Bool -> Msg) -> String -> Bool -> Element Msg
        checkbox onChange subMeshName checked =
            Input.checkbox []
                { onChange = onChange
                , icon = Input.defaultCheckbox
                , checked = checked
                , label = Input.labelRight [] (text subMeshName)
                }
    in
    column []
        [ text "Show"
        , checkbox SetFaces "Faces" mask.faces
        , checkbox SetEdges "Edges" mask.edges
        , checkbox SetVertices "Vertices" mask.vertices
        ]
