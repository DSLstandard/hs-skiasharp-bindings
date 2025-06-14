module Skia.SKEncodedOrigin where

import Linear
import Skia.Internal.Prelude

{- |  Given an encoded origin and the width and height (of any numeric type) of
the source data, returns a matrix that transforms the source rectangle with
upper left corner at [0, 0] and origin to a correctly oriented destination
rectangle of [0, 0, w, h].
-}
toMatrix ::
    (Num a) =>
    SKEncodedOrigin ->
    -- | Width
    a ->
    -- | Height
    a ->
    M33 a
toMatrix origin w h = do
    -- See Google Skia's include/codec/SkEncodedOrigin.h
    case origin of
        SKEncodedOrigin'TopLeft ->
            identity
        SKEncodedOrigin'TopRight ->
            V3
                (V3 (-1) 0 w)
                (V3 0 1 0)
                (V3 0 0 1)
        SKEncodedOrigin'BottomRight ->
            V3
                (V3 (-1) 0 w)
                (V3 0 (-1) h)
                (V3 0 0 1)
        SKEncodedOrigin'BottomLeft ->
            V3
                (V3 1 0 0)
                (V3 0 (-1) h)
                (V3 0 0 1)
        SKEncodedOrigin'LeftTop ->
            V3
                (V3 0 1 0)
                (V3 1 0 0)
                (V3 0 0 1)
        SKEncodedOrigin'RightTop ->
            V3
                (V3 0 (-1) w)
                (V3 1 0 0)
                (V3 0 0 1)
        SKEncodedOrigin'RightBottom ->
            V3
                (V3 0 (-1) w)
                (V3 (-1) 0 h)
                (V3 0 0 1)
        SKEncodedOrigin'LeftBottom ->
            V3
                (V3 0 1 0)
                (V3 (-1) 0 h)
                (V3 0 0 1)

{- | Return true if the encoded origin includes a 90 degree rotation, in which
case the width and height of the source data are swapped relative to a correctly
oriented destination.
-}
swapsWidthHeight :: SKEncodedOrigin -> Bool
swapsWidthHeight = \case
    -- See Google Skia's include/codec/SkEncodedOrigin.h
    SKEncodedOrigin'TopLeft -> False
    SKEncodedOrigin'TopRight -> False
    SKEncodedOrigin'BottomRight -> False
    SKEncodedOrigin'BottomLeft -> False
    SKEncodedOrigin'LeftTop -> True
    SKEncodedOrigin'RightTop -> True
    SKEncodedOrigin'RightBottom -> True
    SKEncodedOrigin'LeftBottom -> True
