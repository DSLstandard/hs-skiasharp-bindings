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

{- | Like 'withArrayLen' but the inner function is uncurried.

The purpose of this function is to allow one to write:

@
(ptr, len) <- ContT $ withArrayLen' myHaskellArray
@
-}
withArrayLen' :: (Storable s) => [s] -> ((Ptr s, Int) -> IO r) -> IO r
withArrayLen' array f = withArrayLen array (flip $ curry f)
{-# INLINE withArrayLen' #-}

bitOrs :: (Bits a) => [a] -> a
bitOrs = foldl' (.|.) zeroBits
{-# INLINE bitOrs #-}

makeBitFlags ::
    forall a.
    (Bits a) =>
    -- | A list of (Flag values, True if flag is set)
    [(Bool, a)] ->
    a
makeBitFlags flags = do
    foldl'
        ( \flag (flagIsSet, flagValue) ->
            if flagIsSet
                then flag .|. flagValue
                else flag
        )
        zeroBits
        flags

hasFlag ::
    (Bits a) =>
    -- | Query flag value - the flag value with only one bit set
    a ->
    -- | Flag value input to be tested
    a ->
    Bool
hasFlag query flag = (flag .&. query) /= zeroBits

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
