{- | Type level boolean utility

Reference: https://hackage.haskell.org/package/type-level-0.3.0/docs/Data-TypeLevel-Bool.html
-}
module Skia.Internal.Bool where

data True
data False

type family Or a b where
    Or False False = False
    Or True False = True
    Or False True = True
    Or True True = True

type family If cond onTrue onFalse where
    If False onTrue onFalse = onFalse
    If True onTrue onFalse = onTrue
