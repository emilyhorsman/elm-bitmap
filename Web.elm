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


type Instruction
    = Circle Bitmap.Pixel Bitmap.Point Int
    | Curve Bitmap.Pixel (List Bitmap.FloatPoint)
    | Line Bitmap.Pixel Bitmap.Point Bitmap.Point


type alias Model =
    { bitmap : Bitmap.Bitmap
    , instructions : List Instruction
    }


init : Model
init =
    { bitmap = Bitmap.create 64 cyan
    , instructions =
        [ Circle black ( 43, 40 ) 3
        , Circle black ( 20, 40 ) 3
        , Circle black ( 31, 31 ) 25
        , Curve black [ ( 15, 25 ), ( 31.5, 10 ), ( 48, 25 ) ]
        , Line black ( 0, 0 ) ( 63, 63 )
        ]
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
        [ model.bitmap
            |> withInstructions model.instructions
            |> drawBitmap
        ]


drawInstruction : Instruction -> Bitmap -> Bitmap
drawInstruction instruction bitmap =
    case instruction of
        Circle pixel point radius ->
            Bitmap.circle pixel point radius bitmap

        Curve pixel points ->
            Bitmap.curve pixel points bitmap

        Line pixel p0 p1 ->
            Bitmap.line pixel p0 p1 bitmap


withInstructions : List Instruction -> Bitmap -> Bitmap
withInstructions instructions bitmap =
    List.foldl drawInstruction bitmap instructions


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
