module Web exposing (..)

import Array
import Html exposing (Html, div)
import Html.App as App
import Html.Attributes
import String
import Svg exposing (..)
import Svg.Attributes
import Bitmap exposing (Bitmap)


black =
    Bitmap.Pixel 0 0 0 1


cyan =
    Bitmap.Pixel 200 255 255 1


main =
    App.beginnerProgram
        { model = init
        , view = view
        , update = update
        }


type alias Model =
    { bitmap : Bitmap.Bitmap
    }


init : Model
init =
    { bitmap =
        Bitmap.create 64 cyan
            |> Bitmap.circle black ( 43, 40 ) 3
            |> Bitmap.circle black ( 20, 40 ) 3
            |> Bitmap.circle black ( 31, 31 ) 25
            |> Bitmap.curve black [ ( 15, 25 ), ( 31.5, 10 ), ( 48, 25 ) ]
    }


type Msg
    = Noop


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style
            [ ( "margin", "0 auto" )
            , ( "width", "400px" )
            ]
        ]
        [ drawBitmap model.bitmap
        ]


pixelToAttribute : Bitmap.Pixel -> String
pixelToAttribute pixel =
    let
        (Bitmap.Pixel r g b a) =
            pixel

        values =
            [ r, g, b ]
                |> List.map toString
                |> String.join ","
    in
        "rgba(" ++ values ++ "," ++ (toString a) ++ ")"


drawPixel rowIndex colIndex pixel =
    let
        x =
            (toFloat colIndex)

        y =
            (toFloat (63 - rowIndex))
    in
        rect
            [ Svg.Attributes.x (toString x)
            , Svg.Attributes.y (toString y)
            , Svg.Attributes.width "1"
            , Svg.Attributes.height "1"
            , Svg.Attributes.fill (pixelToAttribute pixel)
            ]
            []


drawBitmap : Bitmap -> Html Msg
drawBitmap bitmap =
    let
        drawRow index row =
            Array.indexedMap (drawPixel index) row
                |> Array.toList
    in
        Array.indexedMap drawRow bitmap
            |> Array.foldl (++) []
            |> svg
                [ Svg.Attributes.width "400"
                , Svg.Attributes.height "400"
                , Svg.Attributes.viewBox "0 0 64 64"
                ]
