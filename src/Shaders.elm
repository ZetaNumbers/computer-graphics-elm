module Shaders exposing (Uniforms, whiteFragmentShader, difFragmentShader, vertexShader)

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import Vertex exposing (VertexGl)
import WebGL exposing (Shader)


type alias Uniforms =
    { perspective : Mat4 }


vertexShader : Shader VertexGl Uniforms { vnormal : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;

        uniform mat4 perspective;

        varying vec3 vnormal;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vnormal = normal;
        }
    |]


difFragmentShader : Shader {} Uniforms { vnormal : Vec3 }
difFragmentShader =
    [glsl|
        precision mediump float;

        varying vec3 vnormal;
        
        const vec3 ambientColor = vec3(0.2, 0.2, 0.2);
        const vec3 diffuseColor = vec3(0.8, 0.8, 0.8);
        const vec3 difLightDir = normalize(-vec3(1.0, 2.0, 3.0));

        void main () {
            gl_FragColor = vec4(diffuseColor * max(dot(-difLightDir, vnormal), 0.0) + ambientColor, 1.0);
        }
    |]


whiteFragmentShader : Shader {} Uniforms { vnormal : Vec3 }
whiteFragmentShader =
    [glsl|
        precision mediump float;
        
        varying vec3 vnormal;
        
        void main () {
            gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
        }
    |]
