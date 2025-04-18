module Data exposing (mags)

import Data.Blobs.MAGs exposing (magsBlob)
import LoadData exposing (loadData)
import Csv.Decode as Decode
import DataModel exposing (MAG)

mags : List MAG
mags = case loadData magsBlob of
    Ok r ->
        r
    Err err ->
        []

