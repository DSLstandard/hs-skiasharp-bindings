module Skia.SKImageInfo (
    module Skia.SKImageInfo,
    SKImageInfo (..), -- from Skia.Types.Extra,
)
where

import Skia.SKColorType qualified as SKColorType
import Skia.Types.Extra

bytesPerPixel :: SKImageInfo -> Int
bytesPerPixel info = SKColorType.bytesPerPixel info.colorType

{- | Returns minimum bytes per row, computed from pixel 'width' and
'SKColorType', which specifies 'bytesPerPixel'.

Note that 'SKBitmap' maximum value for row bytes must fit in 31 bits, even if
this function returns a larger value.
-}
minRowBytes :: SKImageInfo -> Int
minRowBytes info = info.width * SKColorType.bytesPerPixel info.colorType
