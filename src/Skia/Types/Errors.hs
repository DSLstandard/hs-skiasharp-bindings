module Skia.Types.Errors where

import Control.Exception

-- NOTE: The errors in this module do not correspond to any Google Skia-defined
-- exceptions. They are for this Haskell library.

newtype UnmarshalSKEnumError = UnmarshalSKEnumError String
    deriving (Show)

instance Exception UnmarshalSKEnumError

-- | An internal error of *THIS* Haskell Skia binding library.
newtype InternalError = InternalError String
    deriving (Show)

instance Exception InternalError

newtype BadArgumentError = BadArgumentError String
    deriving (Show)

instance Exception BadArgumentError

newtype SkiaAssertionError = SkiaAssertionError String
    deriving (Show)

instance Exception SkiaAssertionError

-- | General errors intended for Haskell library users to look at.
data SkiaError = SkiaError String
    deriving (Show)

instance Exception SkiaError
