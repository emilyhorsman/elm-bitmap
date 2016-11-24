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


closedRange : Int -> Int -> Array Int
closedRange from to =
    let
        -- Add one for closed interval.
        quantity =
            abs (to - from) + 1

        sign =
            if from <= to then
                (+)
            else
                (-)
    in
        Array.initialize quantity identity |> Array.map (sign from)


line : Pixel -> Point -> Point -> Bitmap -> Bitmap
line pixel origin endpoint bitmap =
    let
        ( x1, y1 ) =
            origin

        ( x2, y2 ) =
            endpoint

        dx =
            x2 - x1

        dy =
            y2 - y1

        m =
            (toFloat dy) / (toFloat dx)

        -- We cannot use the traditional slope (y per x) if we plot through
        -- ys instead of xs. We would need to use the reciprocal slope, the
        -- units x per unit y.
        rM =
            (toFloat dx) / (toFloat dy)

        xs =
            closedRange x1 x2

        ys =
            closedRange y1 y2

        -- 0 <= dx/dy <= 1
        plotFirstOctant x ( bitmap, error, y ) =
            let
                nextBitmap =
                    set pixel y x bitmap

                shouldIncrementY =
                    error + m >= 0.5

                ( nextError, nextY ) =
                    if shouldIncrementY then
                        ( error + m - 1, y + 1 )
                    else
                        ( error + m, y )
            in
                ( nextBitmap, nextError, nextY )

        -- 0 > dx/dy >= -1
        plotEighthOctant x ( bitmap, error, y ) =
            let
                nextBitmap =
                    set pixel y x bitmap

                shouldDecrementY =
                    error + m <= -0.5

                ( nextError, nextY ) =
                    if shouldDecrementY then
                        ( error + m + 1, y - 1 )
                    else
                        ( error + m, y )
            in
                ( nextBitmap, nextError, nextY )

        -- 1 < dx/dy < inf
        plotSecondOctant y ( bitmap, error, x ) =
            let
                nextBitmap =
                    set pixel y x bitmap

                shouldIncrementX =
                    error + rM >= 0.5

                ( nextError, nextX ) =
                    if shouldIncrementX then
                        ( error + rM - 1, x + 1 )
                    else
                        ( error + rM, x )
            in
                ( nextBitmap, nextError, nextX )

        plotSeventhOctant y ( bitmap, error, x ) =
            let
                nextBitmap =
                    set pixel y x bitmap

                shouldIncrementX =
                    error + rM <= -0.5

                ( nextError, nextX ) =
                    if shouldIncrementX then
                        ( error + rM + 1, x + 1 )
                    else
                        ( error + rM, x )
            in
                ( nextBitmap, nextError, nextX )

        ( plotFunc, interval, start ) =
            if m >= 0 && m <= 1 then
                ( plotFirstOctant, closedRange x1 x2, y1 )
            else if m > 1 then
                ( plotSecondOctant, closedRange y1 y2, x1 )
            else if m < 0 && m >= -1 then
                ( plotEighthOctant, closedRange x1 x2, y1 )
            else if m < -1 && y2 < y1 then
                ( plotSeventhOctant, closedRange y1 y2, x1 )
            else
                ( plotFirstOctant, closedRange x1 x2, y1 )

        ( newBitmap, _, _ ) =
            Array.foldl plotFunc ( bitmap, 0, start ) interval
    in
        newBitmap
