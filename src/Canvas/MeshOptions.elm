module Canvas.MeshOptions exposing (MeshOption(..), init, optionList, toOption, toPath)

import Element exposing (text)
import Element.Input as Input


type MeshOption
    = Cube
    | Sphere
    | Cylinder
    | Cone
    | Torus
    | Snowman


optionList : List MeshOption
optionList =
    [ Cube, Sphere, Cylinder, Cone, Torus, Snowman ]


init : MeshOption
init =
    Cube


toOption : MeshOption -> Input.Option MeshOption msg
toOption opt =
    Input.option opt (text (toName opt))


toName : MeshOption -> String
toName opt =
    case opt of
        Cube ->
            "Cube"

        Sphere ->
            "Sphere"

        Cylinder ->
            "Cylinder"

        Cone ->
            "Cone"

        Torus ->
            "Torus"

        Snowman ->
            "Snowman"


toPath : MeshOption -> String
toPath opt =
    "./" ++ toName opt ++ ".obj"
