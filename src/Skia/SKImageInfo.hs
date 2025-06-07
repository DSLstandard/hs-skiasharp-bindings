module Skia.SKImageInfo (
    module Skia.SKImageInfo,
    SKImageInfo (..), -- from Skia.Types.Extra,
)
where

import Skia.SKColorType qualified as SKColorType
import Skia.Types.Extra

bytesPerPixel :: SKImageInfo -> Int
bytesPerPixel info = SKColorType.bytesPerPixel info.colorType
