module Bitmap exposing (Bitmap, Pixel(..), create, set, toggle, line)

import Array exposing (Array)
import Color


type alias Red =
    Int


type alias Green =
    Int


type alias Blue =
    Int


type alias Alpha =
    Float


type alias Point =
    ( Int, Int )


type Pixel
    = Pixel Red Green Blue Alpha


type alias Bitmap =
    Array (Array Pixel)


create : Int -> Pixel -> Bitmap
create length defaultFill =
    let
        row =
            Array.repeat length defaultFill
    in
        Array.repeat length row


set : Pixel -> Int -> Int -> Bitmap -> Bitmap
set newFill rowIndex colIndex bitmap =
    let
        set arr =
            Array.set colIndex newFill arr

        newRow =
            Maybe.map set (Array.get rowIndex bitmap)
    in
        case newRow of
            Just row ->
                Array.set rowIndex row bitmap

            Nothing ->
                bitmap


get : Int -> Int -> Bitmap -> Maybe Pixel
get rowIndex colIndex bitmap =
    Maybe.andThen (Array.get rowIndex bitmap) (Array.get colIndex)


toggle : Pixel -> Pixel -> Int -> Int -> Bitmap -> Bitmap
toggle aPixel bPixel rowIndex colIndex bitmap =
    let
        curPixel =
            get rowIndex colIndex bitmap

        alternate pixel =
            if pixel == aPixel then
                bPixel
            else if pixel == bPixel then
                aPixel
            else
                pixel
    in
        case curPixel of
            Just pixel ->
                set (alternate pixel) rowIndex colIndex bitmap

            Nothing ->
                bitmap


line : Pixel -> Point -> Point -> Bitmap -> Bitmap
line pixel origin endpoint bitmap =
    let
        ( x1, y1 ) =
            origin

        ( x2, y2 ) =
            endpoint

        -- 0 <= dx/dy <= 1
        dx =
            x2 - x1

        dy =
            y1 - y2

        m =
            (toFloat dy) / (toFloat dx)

        xs =
            Array.initialize (dx + 1) identity |> Array.map ((+) x1)

        draw x ( bitmap, error, y ) =
            let
                nextBitmap =
                    set pixel y x bitmap

                shouldIncrementY =
                    error + m >= 0.5

                nextError =
                    if shouldIncrementY then
                        error + m - 1
                    else
                        error + m

                nextY =
                    if shouldIncrementY then
                        y - 1
                    else
                        y
            in
                ( nextBitmap, nextError, nextY )

        ( newBitmap, _, _ ) =
            Array.foldl draw ( bitmap, 0, y1 ) xs
    in
        newBitmap
