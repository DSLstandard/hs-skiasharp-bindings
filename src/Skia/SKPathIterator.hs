module Skia.SKPathIterator where

import Skia.Internal.Prelude

destroy :: (MonadIO m) => SKPathIterator -> m ()
destroy iter = evalContIO do
    iter' <- useObj iter
    liftIO $ sk_path_iter_destroy iter'

create ::
    (MonadIO m) =>
    SKPath ->
    -- | Force close?
    Bool ->
    m SKPathIterator
create path forceClose = evalContIO do
    path' <- useObj path
    iter' <- liftIO $ sk_path_create_iter path' (fromBool forceClose)
    toObject iter'

{- | Returns next SkPath::Verb in verb array, and advances SkPath::Iter. When
verb array is exhausted, returns kDone_Verb.

Zero to four SkPoint are stored in pts, depending on the returned SkPath::Verb.
-}
next ::
    (MonadIO m) =>
    SKPathIterator ->
    -- | pts. Should be an array of at least 4 points.
    Ptr Sk_point ->
    m SKPathVerb
next iter pts = evalContIO do
    iter' <- useObj iter
    verb <- liftIO $ sk_path_iter_next iter' pts
    unmarshalSKEnumOrDie verb

{- | Returns conic weight if next() returned kConic_Verb.

If next() has not been called, or next() did not return kConic_Verb, result is
undefined.

@return  conic weight for conic SkPoint returned by next()
-}
getConicWeight :: (MonadIO m) => SKPathIterator -> m Float
getConicWeight iter = evalContIO do
    iter' <- useObj iter
    liftIO $ coerce <$> sk_path_iter_conic_weight iter'

isCloseLine :: (MonadIO m) => SKPathIterator -> m Bool
isCloseLine iter = evalContIO do
    iter' <- useObj iter
    liftIO $ toBool <$> sk_path_iter_is_close_line iter'

isClosedContour :: (MonadIO m) => SKPathIterator -> m Bool
isClosedContour iter = evalContIO do
    iter' <- useObj iter
    liftIO $ toBool <$> sk_path_iter_is_closed_contour iter'
