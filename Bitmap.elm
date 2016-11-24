module Bitmap exposing (Bitmap, Pixel(..), create, set, toggle)

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


set : Bitmap -> Pixel -> Int -> Int -> Bitmap
set bitmap newFill rowIndex colIndex =
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


get : Bitmap -> Int -> Int -> Maybe Pixel
get bitmap rowIndex colIndex =
    Maybe.andThen (Array.get rowIndex bitmap) (Array.get colIndex)


toggle : Bitmap -> Pixel -> Pixel -> Int -> Int -> Bitmap
toggle bitmap aPixel bPixel rowIndex colIndex =
    let
        curPixel =
            get bitmap rowIndex colIndex

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
                set bitmap (alternate pixel) rowIndex colIndex

            Nothing ->
                bitmap
