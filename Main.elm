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
            |> Bitmap.line black ( 31, 32 ) ( 63, 0 )
            |> Bitmap.line black ( 31, 32 ) ( 63, 16 )
            |> Bitmap.line black ( 31, 31 ) ( 48, 0 )
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
