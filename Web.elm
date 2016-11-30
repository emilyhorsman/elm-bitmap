module Web exposing (..)

import Array exposing (Array)
import Char
import Html exposing (Html, div, button, text, input, span)
import Html.App as App
import Html.Attributes exposing (type', value)
import Html.Events exposing (onClick, onInput)
import String
import Svg
import Svg.Attributes
import Bitmap exposing (Bitmap)
import Debug exposing (log)


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
            [ Circle (Bitmap.Pixel 51 153 255 1) ( 43, 40 ) 3
            , Circle black ( 20, 40 ) 3
            , Circle black ( 31, 31 ) 25
            , Curve black [ ( 15, 25 ), ( 31.5, 10 ), ( 48, 25 ) ]
            ]
    }


type Msg
    = Remove Int
    | Add Instruction
    | ChangePixel Int String
    | ChangeRadius Int String
    | ChangePoint Int Int Int String


removeInstruction : Int -> Instructions -> Instructions
removeInstruction index instructions =
    let
        a =
            Array.slice 0 index instructions

        b =
            Array.slice (index + 1) (Array.length instructions) instructions
    in
        Array.append a b


changeInstructions : Int -> (Instruction -> Instruction) -> Instructions -> Instructions
changeInstructions index transform instructions =
    let
        instruction =
            Array.get index instructions

        nextInstruction =
            Maybe.map transform instruction
    in
        case nextInstruction of
            Just instr ->
                Array.set index instr instructions

            Nothing ->
                instructions


changePixel : Int -> String -> Instructions -> Instructions
changePixel index value instructions =
    let
        newPixel =
            hexToPixel value

        transform instruction =
            case instruction of
                Circle _ point radius ->
                    Circle newPixel point radius

                Curve _ points ->
                    Curve newPixel points

                Line _ p0 p1 ->
                    Line newPixel p0 p1
    in
        changeInstructions index transform instructions


changeRadius : Int -> String -> Instructions -> Instructions
changeRadius index value instructions =
    let
        newValue =
            String.toInt value

        transform instruction =
            case ( newValue, instruction ) of
                ( Ok v, Circle pixel point _ ) ->
                    Circle pixel point v

                _ ->
                    instruction
    in
        changeInstructions index transform instructions


changePoint : Int -> Int -> Int -> String -> Instructions -> Instructions
changePoint index pointIndex component value instructions =
    let
        newValue =
            String.toInt value

        transform instruction =
            case ( newValue, pointIndex, component, instruction ) of
                ( Ok x, 0, 0, Line pixel ( _, y) p1 ) ->
                    Line pixel ( x, y ) p1

                ( Ok y, 0, 1, Line pixel ( x, _ ) p1 ) ->
                    Line pixel ( x, y ) p1

                ( Ok x, 1, 0, Line pixel p0 ( _, y ) ) ->
                    Line pixel p0 ( x, y )

                ( Ok y, 1, 1, Line pixel p0 ( x, _ ) ) ->
                    Line pixel p0 ( x, y )

                _ ->
                    instruction

    in
        changeInstructions index transform instructions


update : Msg -> Model -> Model
update msg model =
    case msg of
        Remove index ->
            { model | instructions = removeInstruction index model.instructions }

        Add instruction ->
            { model | instructions = Array.push instruction model.instructions }

        ChangePixel index value ->
            { model | instructions = changePixel index value model.instructions }

        ChangeRadius index value ->
            { model | instructions = changeRadius index value model.instructions }

        ChangePoint index pointIndex component value ->
            { model | instructions = changePoint index pointIndex component value model.instructions }


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


colourInput : String -> (String -> Msg) -> Html Msg
colourInput current msg =
    input
        [ type' "color"
        , value current
        , onInput msg
        ]
        []


numberInput : number -> (String -> Msg) -> Html Msg
numberInput current msg =
    input
        [ type' "number"
        , value (toString current)
        , onInput msg
        , Html.Attributes.min "0"
        ]
        []


pointInput : ( number, number ) -> (Int -> String -> Msg) -> Html Msg
pointInput ( x, y ) msg =
    span
        []
        [ numberInput x (msg 0)
        , numberInput y (msg 1)
        ]


drawInstructionCommands : Int -> Instruction -> Html Msg
drawInstructionCommands index instruction =
    case instruction of
        Circle pixel ( x, y ) radius ->
            span []
                [ text "Circle"
                , colourInput (pixelToHex pixel) (ChangePixel index)
                , numberInput radius (ChangeRadius index)
                ]

        Curve pixel points ->
            span []
                [ text "Curve"
                , colourInput (pixelToHex pixel) (ChangePixel index)
                ]

        Line pixel p0 p1 ->
            span []
                [ text "Line"
                , colourInput (pixelToHex pixel) (ChangePixel index)
                , pointInput p0 (ChangePoint index 0)
                , pointInput p1 (ChangePoint index 1)
                ]


drawUserInstruction : Int -> Instruction -> Html Msg
drawUserInstruction index instruction =
    div
        []
        [ drawInstructionCommands index instruction
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


intToHex : Int -> String
intToHex value =
    let
        values =
            [ value // 16, value % 16 ]

        toHex int =
            if int <= 9 then
                Char.fromCode (int + 48)
            else
                Char.fromCode (int + 87)
    in
        values |> List.map toHex |> String.fromList


pixelToHex : Bitmap.Pixel -> String
pixelToHex (Bitmap.Pixel r g b _) =
    "#" ++ (intToHex r) ++ (intToHex g) ++ (intToHex b)


hexToInt : Char -> Int
hexToInt char =
    let
        code =
            Char.toCode char
    in
        -- ['0', '9'] => [48, 57]
        -- ['a', 'f'] => [97, 102]
        if code <= 57 then
            code - 48
        else
            code - 87


hexToPixel : String -> Bitmap.Pixel
hexToPixel hex =
    case String.toList hex of
        '#' :: r16 :: r1 :: g16 :: g1 :: b16 :: b1 :: [] ->
            let
                r =
                    (hexToInt r16) * 16 + hexToInt r1

                g =
                    (hexToInt g16) * 16 + hexToInt g1

                b =
                    (hexToInt b16) * 16 + hexToInt b1
            in
                Bitmap.Pixel r g b 1

        _ ->
            Bitmap.Pixel 0 0 0 1


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
