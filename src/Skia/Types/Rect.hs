module Skia.Types.Rect where

import Data.Coerce
import Foreign.C.Types
import Skia.Bindings

data Rect a = Rect
    { left :: a
    , top :: a
    , right :: a
    , bottom :: a
    }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

width :: (Num a) => Rect a -> a
width Rect{..} = right - left

height :: (Num a) => Rect a -> a
height Rect{..} = bottom - top

-- NOTE: It looks strange, but this definition is from SkiaSharp C#.
empty :: (Num a) => Rect a
empty = Rect 0 0 0 0

-- NOTE: It looks strange, but this definition is from SkiaSharp C#.
isEmpty :: (Num a, Eq a) => Rect a -> Bool
isEmpty rect = rect == empty

toSKIRect :: Rect Int -> Sk_irect
toSKIRect (fmap fromIntegral -> Rect{..}) = Sk_irect{..}

fromSKIRect :: Sk_irect -> Rect Int
fromSKIRect Sk_irect{..} = fmap fromIntegral Rect{..}

toSKRect :: Rect Float -> Sk_rect
toSKRect (coerce -> Rect{..}) = Sk_rect{..}

fromSKRect :: Sk_rect -> Rect Float
fromSKRect Sk_rect{..} = coerce Rect{..}
