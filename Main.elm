module Main exposing (..)

import Bitmap
import GraphicSVG exposing (..)
import List
import Array
import Debug exposing (log)


drawPixel rowIndex colIndex pixel =
    let
        (Bitmap.Pixel r g b a) =
            pixel

        x =
            (toFloat colIndex) * cellSize

        y =
            (toFloat rowIndex) * cellSize
    in
        square cellSize
            |> filled (rgba (toFloat r) (toFloat g) (toFloat b) a)
            |> move ( x, y )
            |> notifyTap (Toggle rowIndex colIndex)


drawBitmap bitmap =
    let
        drawRow index row =
            Array.indexedMap (drawPixel index) row
                |> Array.toList
                |> group
    in
        Array.indexedMap drawRow bitmap
            |> Array.toList
            |> group


white =
    Bitmap.Pixel 200 255 255 1


black =
    Bitmap.Pixel 0 0 0 1


type Message
    = GameTick Float GetKeyState
    | Toggle Int Int


cellSize =
    6


main =
    gameApp GameTick
        { model = init
        , view = view
        , update = update
        }


init =
    { t = 0
    , bitmap =
        Bitmap.create 64 white
            |> Bitmap.line black ( 0, 0 ) ( 63, 63 )
            |> Bitmap.line black ( 63, 63 ) ( 0, 0 )
            |> Bitmap.line black ( 0, 63 ) ( 63, 0 )
            |> Bitmap.line black ( 63, 0 ) ( 0, 63 )
            |> Bitmap.line black ( 0, 16 ) ( 63, 48 )
            |> Bitmap.line black ( 63, 48 ) ( 0, 16 )
            |> Bitmap.line black ( 0, 48 ) ( 63, 16 )
            |> Bitmap.line black ( 63, 16 ) ( 0, 48 )
            |> Bitmap.line black ( 16, 63 ) ( 48, 0 )
            |> Bitmap.line black ( 48, 0 ) ( 16, 63 )
            |> Bitmap.line black ( 48, 63 ) ( 16, 0 )
            |> Bitmap.line black ( 16, 0 ) ( 48, 63 )
            |> Bitmap.line black ( 0, 32 ) ( 63, 32 )
            |> Bitmap.line black ( 63, 31 ) ( 0, 31 )
            |> Bitmap.line black ( 31, 63 ) ( 31, 0 )
            |> Bitmap.line black ( 32, 0 ) ( 32, 63 )
    }


view model =
    let
        center =
            move ( cellSize * -32, cellSize * -32 )
    in
        collage 600
            600
            [ drawBitmap model.bitmap |> center
            ]


update message model =
    case message of
        Toggle row col ->
            { model
                | bitmap = Bitmap.toggle black white row col model.bitmap
            }

        _ ->
            model
