module Skia.Internal.Utils where

import Control.Monad.IO.Class
import Data.Bits
import Data.Coerce
import Data.Foldable
import Foreign
import Linear

applyV2 :: V2 a -> (a -> a -> r) -> r
applyV2 (V2 x y) f = f x y
{-# INLINE applyV2 #-}

apply :: a -> (a -> r) -> r
apply x f = f x
{-# INLINE apply #-}

peekWith :: (Storable a, MonadIO m) => (a -> b) -> Ptr a -> m b
peekWith mapping ptr = do
    r <- liftIO $ peek ptr
    pure $ mapping r
{-# INLINE peekWith #-}

withArrayLen' :: (Storable s) => [s] -> ((Ptr s, Int) -> IO r) -> IO r
withArrayLen' array f = withArrayLen array (flip $ curry f)
{-# INLINE withArrayLen' #-}

bitOrs :: (Bits a) => [a] -> a
bitOrs = foldl' (.|.) zeroBits
{-# INLINE bitOrs #-}

{- | Composes a bit flag.

Example: @0b00010010 == makeBitFlags @Word8 [(0, False), (1, True), (4, True)]@
-}
makeBitFlags ::
    forall a.
    (Bits a) =>
    -- | A list of (0-indexed bit position, is bit set?)
    [(Int, Bool)] ->
    a
makeBitFlags fields = do
    foldl'
        ( \wholeFlag (bitPos, bitIsSet) ->
            if bitIsSet
                then setBit wholeFlag bitPos
                else wholeFlag
        )
        zeroBits
        fields

convert4Word8ToWord32 :: (Word8, Word8, Word8, Word8) -> Word32
convert4Word8ToWord32 (c1, c2, c3, c4) = do
    -- FIXME: Is there already a builtin function that does this?
    (c1' `shiftL` 24) .|. (c2' `shiftL` 16) .|. (c3' `shiftL` 8) .|. c4'
  where
    c1' = fromIntegral c1
    c2' = fromIntegral c2
    c3' = fromIntegral c3
    c4' = fromIntegral c4

convertWord32To4Word8 :: Word32 -> (Word8, Word8, Word8, Word8)
convertWord32To4Word8 word32 = do
    -- FIXME: Is there already a builtin function that does this?
    (octet 3, octet 2, octet 1, octet 0)
  where
    octet :: Int -> Word8
    octet ix = fromIntegral $ word32 `shiftR` (8 * ix)
    {-# INLINE octet #-}

{- | Like 'castPtr' but imposes a 'Coercible' constraint. This function can be
used to catch type errors.
-}
coercePtr :: (Coercible a b) => Ptr a -> Ptr b
coercePtr = castPtr

{- | Like 'castForeignPtr' but imposes a 'Coercible' constraint. This function can be
used to catch type errors.
-}
coerceForeignPtr :: (Coercible a b) => ForeignPtr a -> ForeignPtr b
coerceForeignPtr = castForeignPtr
