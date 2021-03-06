module Web exposing (..)

import Array exposing (Array)
import Char
import Html exposing (Html, div, button, text, input, span, table, tr, td, a)
import Html.App as App
import Html.Attributes exposing (type', value, href, target, style)
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
            [ Circle black ( 43, 40 ) 3
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
    | ChangeFloatPoint Int Int Int String
    | AddFloatPoint Int
    | RemoveFloatPoint Int Int


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


changePoints : Int -> Int -> Float -> List Bitmap.FloatPoint -> List Bitmap.FloatPoint
changePoints pointIndex component value points =
    let
        change index point =
            if index == pointIndex then
                case ( component, point ) of
                    ( 0, ( _, y ) ) ->
                        ( value, y )

                    ( 1, ( x, _ ) ) ->
                        ( x, value )

                    _ ->
                        point
            else
                point
    in
        List.indexedMap change points


changeFloatPoint : Int -> Int -> Int -> String -> Instructions -> Instructions
changeFloatPoint index pointIndex component value instructions =
    let
        newValue =
            String.toFloat value

        transform instruction =
            case ( newValue, instruction ) of
                ( Ok v, Curve pixel points ) ->
                    Curve pixel (changePoints pointIndex component v points)

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
                ( Ok x, 0, 0, Line pixel ( _, y ) p1 ) ->
                    Line pixel ( x, y ) p1

                ( Ok y, 0, 1, Line pixel ( x, _ ) p1 ) ->
                    Line pixel ( x, y ) p1

                ( Ok x, 1, 0, Line pixel p0 ( _, y ) ) ->
                    Line pixel p0 ( x, y )

                ( Ok y, 1, 1, Line pixel p0 ( x, _ ) ) ->
                    Line pixel p0 ( x, y )

                ( Ok x, _, 0, Circle pixel ( _, y ) radius ) ->
                    Circle pixel ( x, y ) radius

                ( Ok y, _, 1, Circle pixel ( x, _ ) radius ) ->
                    Circle pixel ( x, y ) radius

                _ ->
                    instruction
    in
        changeInstructions index transform instructions


addFloatPoint : Int -> Instructions -> Instructions
addFloatPoint index instructions =
    let
        transform instruction =
            case instruction of
                Curve pixel points ->
                    Curve pixel (points ++ [ ( 0, 0 ) ])

                _ ->
                    instruction
    in
        changeInstructions index transform instructions


removeFromList : Int -> List a -> List a
removeFromList index list =
    let
        drop item ( items, i, hasDropped ) =
            if not hasDropped && i == index then
                ( items, i + 1, True )
            else
                ( item :: items, i + 1, False )

        ( nextList, _, _ ) =
            List.foldl drop ( [], 0, False ) list
    in
        nextList


removeFloatPoint : Int -> Int -> Instructions -> Instructions
removeFloatPoint index pointIndex instructions =
    let
        transform instruction =
            case instruction of
                Curve pixel points ->
                    Curve pixel (removeFromList pointIndex points)

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

        ChangeFloatPoint index pointIndex component value ->
            { model | instructions = changeFloatPoint index pointIndex component value model.instructions }

        RemoveFloatPoint index pointIndex ->
            { model | instructions = removeFloatPoint index pointIndex model.instructions }

        AddFloatPoint index ->
            { model | instructions = addFloatPoint index model.instructions }


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "margin", "0 auto" )
            , ( "width", "400px" )
            ]
        ]
        [ model.bitmap
            |> withInstructions model.instructions
            |> drawBitmap
        , drawUserInstructions model.instructions
        , drawInstructionPalette
        , drawInstructionCode model.instructions
        , a
            [ href "https://github.com/emilyhorsman/elm-bitmap/"
            , target "_blank"
            , style [ ( "margin", "1em 0" ), ( "display", "block" ) ]
            ]
            [ text "https://github.com/emilyhorsman/elm-bitmap/" ]
        ]


drawInstructionPalette : Html Msg
drawInstructionPalette =
    let
        commands =
            [ ( "Add Circle", Circle black ( 31, 31 ) 5 )
            , ( "Add Curve", Curve black [ ( 0, 63 ), ( 31.5, 53 ), ( 63, 63 ) ] )
            , ( "Add Line", Line black ( 0, 0 ) ( 63, 0 ) )
            ]

        drawButton ( label, command ) =
            button
                [ onClick (Add command)
                , style [ ( "margin", "1em" ) ]
                ]
                [ text label ]
    in
        div
            [ style [ ( "text-align", "center" ) ] ]
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
        , Html.Attributes.max "127"
        , style [ ( "margin", "0.25em" ) ]
        ]
        []


pointInput : ( number, number ) -> (Int -> String -> Msg) -> Html Msg
pointInput ( x, y ) msg =
    span
        []
        [ numberInput x (msg 0)
        , numberInput y (msg 1)
        ]


drawInstructionPoint : Int -> Int -> Bitmap.FloatPoint -> Html Msg
drawInstructionPoint index pointIndex point =
    div
        []
        [ pointInput point (ChangeFloatPoint index pointIndex)
        , button [ onClick (RemoveFloatPoint index pointIndex) ] [ text "Remove" ]
        ]


drawInstructionCommands : Int -> Instruction -> List (Html Msg)
drawInstructionCommands index instruction =
    case instruction of
        Circle pixel point radius ->
            [ td [] [ text "Circle" ]
            , td [] [ colourInput (pixelToHex pixel) (ChangePixel index) ]
            , td []
                [ numberInput radius (ChangeRadius index)
                , pointInput point (ChangePoint index 0)
                ]
            ]

        Curve pixel points ->
            [ td [] [ text "Curve" ]
            , td [] [ colourInput (pixelToHex pixel) (ChangePixel index) ]
            , td []
                (List.indexedMap (drawInstructionPoint index) points
                    ++ [ button [ onClick (AddFloatPoint index) ] [ text "Add Point" ] ]
                )
            ]

        Line pixel p0 p1 ->
            [ td [] [ text "Line" ]
            , td [] [ colourInput (pixelToHex pixel) (ChangePixel index) ]
            , td []
                [ div [] [ pointInput p0 (ChangePoint index 0) ]
                , div [] [ pointInput p1 (ChangePoint index 1) ]
                ]
            ]


drawUserInstruction : Int -> Instruction -> Html Msg
drawUserInstruction index instruction =
    tr
        [ style [ ( "vertical-align", "top" ) ] ]
        (drawInstructionCommands index instruction
            ++ [ td [] [ button [ onClick (Remove index) ] [ text "Remove" ] ] ]
        )


drawUserInstructions : Instructions -> Html Msg
drawUserInstructions instructions =
    Array.indexedMap drawUserInstruction instructions
        |> Array.toList
        |> table [ style [ ( "width", "100%" ) ] ]


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


drawInstructionCode : Instructions -> Html Msg
drawInstructionCode instructions =
    let
        writtenInstructions =
            instructions
                |> Array.map write
                |> Array.map prefix
                |> Array.toList

        prefix str =
            "    |> " ++ str

        serializePixel (Bitmap.Pixel r g b a) =
            "(Bitmap.Pixel " ++ (toString r) ++ " " ++ (toString g) ++ " " ++ (toString b) ++ " " ++ (toString a) ++ ")"

        write instruction =
            let
                symbols =
                    case instruction of
                        Line pixel p0 p1 ->
                            [ "Bitmap.line"
                            , serializePixel pixel
                            , toString p0
                            , toString p1
                            ]

                        Circle pixel p0 radius ->
                            [ "Bitmap.circle"
                            , serializePixel pixel
                            , toString p0
                            , toString radius
                            ]

                        Curve pixel points ->
                            [ "Bitmap.curve"
                            , serializePixel pixel
                            , toString points
                            ]
            in
                String.join " " symbols

        lines =
            "Bitmap.create 64 (Bitmap.Pixel 200 255 255 1)" :: writtenInstructions
    in
        div
            [ style [ ( "font-family", "monospace" ), ( "white-space", "pre" ) ] ]
            [ String.join "\n" lines |> text ]
