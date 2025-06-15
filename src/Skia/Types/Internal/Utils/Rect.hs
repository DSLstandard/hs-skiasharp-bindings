module Skia.Types.Internal.Utils.Rect where

import Data.Coerce
import Foreign.C.Types
import Skia.Bindings
import Skia.Types.Rect (Rect (..))
import Skia.Types.Rect qualified as Rect

toSKIRect :: Rect Int -> Sk_irect
toSKIRect (fmap fromIntegral -> Rect{..}) = Sk_irect{..}

fromSKIRect :: Sk_irect -> Rect Int
fromSKIRect Sk_irect{..} = fmap fromIntegral Rect{..}

toSKRect :: Rect Float -> Sk_rect
toSKRect (coerce -> Rect{..}) = Sk_rect{..}

fromSKRect :: Sk_rect -> Rect Float
fromSKRect Sk_rect{..} = coerce Rect{..}
