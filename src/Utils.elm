module Utils exposing (..)

import Http


boolOr : Bool -> a -> Maybe a
boolOr mask v =
    if mask then
        Just v

    else
        Nothing


boolThen : Bool -> (() -> a) -> Maybe a
boolThen mask f =
    if mask then
        Just (f ())

    else
        Nothing


boolAsInt : Bool -> Int
boolAsInt b =
    if b then
        1

    else
        0


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadStatus status ->
            "Got BadStatus: " ++ String.fromInt status

        Http.BadUrl url ->
            "Got BadUrl: " ++ url

        Http.Timeout ->
            "Got Timeout!"

        Http.NetworkError ->
            "Got NetworkError"

        Http.BadBody body ->
            "Got BadBody: " ++ body
