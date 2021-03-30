module Vertex exposing (..)

import Geometry.Interop.LinearAlgebra.Point3d as Point3dInterop
import Geometry.Interop.LinearAlgebra.Vector3d as Vector3dInterop
import Length
import Math.Vector3 exposing (Vec3)
import Obj.Decode exposing (ObjCoordinates)
import Point3d exposing (Point3d)
import Quantity
import Vector3d exposing (Vector3d)


type alias VertexGl =
    { position : Vec3
    , normal : Vec3
    }


type alias VertexObj =
    { position : Point3d Length.Meters ObjCoordinates
    , normal : Vector3d Quantity.Unitless ObjCoordinates
    }


toGL : VertexObj -> VertexGl
toGL { position, normal } =
    { position = Point3dInterop.toVec3 position
    , normal = Vector3dInterop.toVec3 normal
    }
