module Skia.SKTextBlobBuilder where

import Skia.Internal.Prelude

-- | Deletes data allocated internally by SkTextBlobBuilder.
destroy :: (MonadIO m) => SKTextBlobBuilder -> m ()
destroy builder = evalContIO do
    builder' <- useObj builder
    liftIO $ sk_textblob_builder_delete builder'

{- | Constructs empty 'SKTextBlobBuilder'. By default, 'SKTextBlobBuilder' has
runs.

MEMORY MANAGEMENT: You must manually free the 'SKTextBlobBuilder' with 'destroy'
when done using it.
-}
create :: (MonadIO m) => m SKTextBlobBuilder
create = liftIO do
    builder' <- sk_textblob_builder_new
    toObject builder'

{- | Returns 'SKTextBlob' built from runs of glyphs added by builder. Returned
'SKTextBlob' is immutable; it may be copied, but its contents may not be
altered. Returns 'Nothing' if no runs of glyphs were added by builder.

Resets 'SKTextBlobBuilder' to its initial empty state, allowing it to be reused
to build a new set of runs.
-}
make :: (MonadIO m) => SKTextBlobBuilder -> m (Maybe SKTextBlob)
make builder = evalContIO do
    builder' <- useObj builder
    blob' <- liftIO $ sk_textblob_builder_make builder'
    if blob' == nullPtr
        then pure Nothing
        else Just <$> toObjectFin sk_textblob_unref blob'
