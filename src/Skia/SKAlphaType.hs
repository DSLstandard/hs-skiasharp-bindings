module Skia.SKAlphaType where

import Skia.Internal.Prelude

{- | Returns true if 'SKAlphaType' equals 'SKAlphaType'Opaque'.

'SKAlphaType'Opaque' is a hint that the 'SKColorType' is opaque, or that all
alpha values are set to their 1.0 equivalent. If 'SKAlphaType' is
'SKAlphaType'Opaque', and 'SKColorType' is not opaque, then the result of
drawing any pixel with a alpha value less than 1.0 is undefined.
-}
isOpaque :: SKAlphaType -> Bool
isOpaque = \case
    -- See include/core/SkAlphaType.h
    SKAlphaType'Opaque -> True
    _ -> False
