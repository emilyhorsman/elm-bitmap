module Web exposing (..)

import Array exposing (Array)
import Html exposing (Html, div, button, text)
import Html.App as App
import Html.Attributes
import Html.Events exposing (onClick)
import String
import Svg
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


type alias Instructions =
    Array Instruction


type alias Model =
    { bitmap : Bitmap.Bitmap
    , instructions : Instructions
    }


init : Model
init =
    { bitmap = Bitmap.create 64 cyan
    , instructions =
        Array.fromList
            [ Circle black ( 43, 40 ) 3
            , Circle black ( 20, 40 ) 3
            , Circle black ( 31, 31 ) 25
            , Curve black [ ( 15, 25 ), ( 31.5, 10 ), ( 48, 25 ) ]
            , Line black ( 0, 0 ) ( 63, 63 )
            ]
    }


type Msg
    = Remove Int
    | Add Instruction


removeInstruction : Int -> Instructions -> Instructions
removeInstruction index instructions =
    let
        a =
            Array.slice 0 index instructions

        b =
            Array.slice (index + 1) (Array.length instructions) instructions
    in
        Array.append a b


update : Msg -> Model -> Model
update msg model =
    case msg of
        Remove index ->
            { model | instructions = removeInstruction index model.instructions }

        Add instruction ->
            { model | instructions = Array.push instruction model.instructions }


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
        , model.instructions
            |> drawUserInstructions
        , drawInstructionPalette
        ]


drawInstructionPalette : Html Msg
drawInstructionPalette =
    let
        commands =
            [ ( "Circle", Circle black ( 31, 31 ) 5 )
            , ( "Curve", Curve black [ ( 0, 63 ), ( 31.5, 53 ), ( 63, 63 ) ] )
            , ( "Line", Line black ( 0, 0 ) ( 63, 0 ) )
            ]

        drawButton ( label, command ) =
            button
                [ onClick (Add command) ]
                [ text label ]
    in
        div
            []
            (List.map drawButton commands)


drawUserInstruction : Int -> Instruction -> Html Msg
drawUserInstruction index instruction =
    div
        []
        [ text (toString instruction)
        , button [ onClick (Remove index) ] [ text "Remove" ]
        ]


drawUserInstructions : Instructions -> Html Msg
drawUserInstructions instructions =
    Array.indexedMap drawUserInstruction instructions
        |> Array.toList
        |> div []


drawInstruction : Instruction -> Bitmap -> Bitmap
drawInstruction instruction bitmap =
    case instruction of
        Circle pixel point radius ->
            Bitmap.circle pixel point radius bitmap

        Curve pixel points ->
            Bitmap.curve pixel points bitmap

        Line pixel p0 p1 ->
            Bitmap.line pixel p0 p1 bitmap


withInstructions : Instructions -> Bitmap -> Bitmap
withInstructions instructions bitmap =
    Array.foldl drawInstruction bitmap instructions


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
        Svg.rect
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
            |> Svg.svg
                [ Svg.Attributes.width "400"
                , Svg.Attributes.height "400"
                , Svg.Attributes.viewBox "0 0 64 64"
                ]
