module Skia.SKDocument where

import Data.Acquire qualified as Acquire
import Data.Text qualified as T
import Data.Time qualified as Time
import Skia.Internal.Prelude
import Skia.SKString qualified as SKString

{- | Google's Skia's default for PDF raster DPI.

@
defaultRasterDpi == 72.0
@
-}
defaultRasterDpi :: Float
defaultRasterDpi = 72.0

data EncodingQuality
    = -- | Held value should be @<= 100@
      EncodingQuality'Lossy Int
    | EncodingQuality'Lossless
    deriving (Show, Eq, Ord)

-- See Google Skia's include/docs/SkPDFDocument.h::(struct Metadata).
data SKPdfDocumentMetadata = SKPdfDocumentMetadata
    { title :: T.Text
    -- ^ The document's title.
    , author :: T.Text
    -- ^ The name of the person who created the document.
    , subject :: T.Text
    -- ^ The subject of the document.
    , keywords :: T.Text
    -- ^ Keywords associated with the document.
    --
    -- Commas may be used to delineate
    -- keywords within the string.
    , creator :: T.Text
    -- ^ If the document was converted to PDF from another format,
    -- the name of the conforming product that created the
    -- original document from which it was converted.
    , producer :: T.Text
    -- ^ The product that is converting this document to PDF.
    --
    -- For reference, in Google's Skia, the default to @""Skia/PDF m" SKPDF_STRING(SK_MILESTONE)"@
    , creation :: Maybe Time.ZonedTime
    -- ^ The date and time the document was created.
    -- 'Nothing' represents an unknown/unset time.
    , modified :: Maybe Time.ZonedTime
    -- ^ The date and time the document was most recently modified.
    -- 'Nothing' represents an unknown/unset time.
    , rasterDpi :: Float
    -- ^ The DPI (pixels-per-inch) at which features without native PDF support
    -- will be rasterized (e.g. draw image with perspective, draw text with
    -- perspective, ...)
    --
    -- A larger DPI would create a PDF that reflects the original intent with
    -- better fidelity, but it can make for larger PDF files too, which would
    -- use more memory while rendering, and it would be slower to be processed
    -- or sent online or to printer.
    , pdfa :: Bool
    -- ^ If true, include XMP metadata, a document UUID, and sRGB output intent
    -- information.  This adds length to the document and makes it
    -- non-reproducable, but are necessary features for PDF/A-2b conformance
    , encodingQuality :: EncodingQuality
    -- ^ Encoding quality controls the trade-off between size and quality.
    --
    -- As a default, this can be set to 'EncodingQuality'Lossless' percent,
    -- which corresponds to lossless encoding.
    --
    -- If this value is set to 'EncodingQuality'Lossy' with value <= 100, and
    -- the image is opaque, it will be encoded (using JPEG) with that quality
    -- setting.
    }
    deriving (Show)

usePdfDocumentMetadata :: SKPdfDocumentMetadata -> ContT r IO (Ptr Sk_document_pdf_metadata)
usePdfDocumentMetadata input = evalContIO do
    (ptr -> fTitle) <- useAcquire $ SKString.createFromText input.title
    (ptr -> fAuthor) <- useAcquire $ SKString.createFromText input.author
    (ptr -> fSubject) <- useAcquire $ SKString.createFromText input.subject
    (ptr -> fKeywords) <- useAcquire $ SKString.createFromText input.keywords
    (ptr -> fCreator) <- useAcquire $ SKString.createFromText input.creator
    (ptr -> fProducer) <- useAcquire $ SKString.createFromText input.producer
    fCreation <- useStorable $ maybe emptyPdfDateTime marshalZonedTime input.creation
    fModified <- useStorable $ maybe emptyPdfDateTime marshalZonedTime input.modified
    let fRasterDPI = coerce input.rasterDpi
    let fPDFA = fromBool input.pdfa
    let fEncodingQuality = case input.encodingQuality of
            EncodingQuality'Lossless -> 101
            EncodingQuality'Lossy value -> fromIntegral value
    useStorable Sk_document_pdf_metadata{..}
  where
    -- Google's Skia: all zero = unknown/unset time
    emptyPdfDateTime :: Sk_document_pdf_datetime
    emptyPdfDateTime = Sk_document_pdf_datetime 0 0 0 0 0 0 0 0

    marshalZonedTime :: Time.ZonedTime -> Sk_document_pdf_datetime
    marshalZonedTime (Time.ZonedTime localTime timeZone) = do
        let Time.TimeZone{timeZoneMinutes} = timeZone
        let Time.LocalTime{localDay, localTimeOfDay} = localTime
        let Time.TimeOfDay{todHour, todMin, todSec} = localTimeOfDay
        let
        {- Google Skia:
            struct DateTime {
                int16_t  fTimeZoneMinutes;  // The number of minutes that this
                                            // is ahead of or behind UTC.
                uint16_t fYear;          //!< e.g. 2005
                uint8_t  fMonth;         //!< 1..12
                uint8_t  fDayOfWeek;     //!< 0..6, 0==Sunday
                uint8_t  fDay;           //!< 1..31
                uint8_t  fHour;          //!< 0..23
                uint8_t  fMinute;        //!< 0..59
                uint8_t  fSecond;        //!< 0..59

                void toISO8601(SkString* dst) const;
            };
        -}
        let Time.YearMonthDay year monthOfYear dayOfMonth = localDay
        Sk_document_pdf_datetime
            { fTimeZoneMinutes =
                fromIntegral timeZoneMinutes
            , fYear =
                fromIntegral year
            , fMonth =
                fromIntegral monthOfYear
            , fDay =
                fromIntegral dayOfMonth
            , fDayOfWeek =
                case Time.dayOfWeek localDay of
                    Time.Sunday -> 0
                    Time.Monday -> 1
                    Time.Tuesday -> 2
                    Time.Wednesday -> 3
                    Time.Thursday -> 4
                    Time.Friday -> 5
                    Time.Saturday -> 6
            , fHour =
                fromIntegral todHour
            , fMinute =
                fromIntegral todMin
            , fSecond =
                -- FIXME: Haskell package 'time' says the range of todSec is 0
                -- <= todSec < 61, but Google's Skia lists fSecond [0..59]...
                -- what to do?
                --
                -- TODO: Use floor or round or ceil?
                floor todSec
            }

-- | Raises a 'SkiaError' if operation fails.
createPdfFromStream ::
    (IsSKWStream stream) =>
    stream ->
    Maybe SKPdfDocumentMetadata ->
    Acquire SKDocument
createPdfFromStream (toA SKWStream -> stream) metadata =
    mkSKObjectAcquire
        ( evalContIO do
            case metadata of
                Nothing -> do
                    liftIO $ sk_document_create_pdf_from_stream (ptr stream)
                Just metadata -> do
                    metadata' <- usePdfDocumentMetadata metadata
                    liftIO $ sk_document_create_pdf_from_stream_with_metadata (ptr stream) metadata'
        )
        sk_document_unref

-- | Raises a 'SkiaError' if operation fails.
createXpsFromStream ::
    (IsSKWStream stream) =>
    stream ->
    -- | DPI. Consider picking 'defaultRasterDpi'.
    Float ->
    -- | Returns 'Nothing' if there is an error.
    Acquire SKDocument
createXpsFromStream (toA SKWStream -> stream) dpi =
    mkSKObjectAcquire
        (sk_document_create_xps_from_stream (ptr stream) (coerce dpi))
        sk_document_unref

{- | Begin a new page for the document, returning the canvas that will draw into
the page. The document owns this canvas, and it will go out of scope when
'endPage' or 'close' is called, or the document is deleted.

NOTE: Because the 'SKDocument' should outlive the returned 'SKCanvas', this
'Acquire' performs 'holdReferenceCount' on the input 'SKDocument' to extend its
lifetime.
-}
beginPage ::
    (MonadIO m) =>
    SKDocument ->
    -- | Width
    Float ->
    -- | Height
    Float ->
    -- | Content rect
    Rect Float ->
    Acquire SKCanvas
beginPage document width height content = evalContIO do
    content' <- useStorable $ toSKRect content
    canvas' <-
        liftIO $
            sk_document_begin_page
                (ptr document)
                (coerce width)
                (coerce height)
                content'
    toObject canvas'

{- | Call 'endPage' when the content for the current page has been drawn (into
the canvas returned by 'beginPage'). After this call the canvas returned by
'beginPage' will be out-of-scope.
-}
endPage ::
    (MonadIO m) =>
    SKDocument ->
    m ()
endPage document = evalContIO do
    document' <- useObj document
    liftIO $ sk_document_end_page document'

{- | Call 'close' when all pages have been drawn. This will close the file or
stream holding the document's contents. After 'close' the document can no longer
add new pages. Deleting the document will automatically call 'close' if need be.
-}
close :: (MonadIO m) => SKDocument -> m ()
close document = evalContIO do
    document' <- useObj document
    liftIO $ sk_document_close document'

{- | Call 'abort' to stop producing the document immediately. The stream output
must be ignored, and should not be trusted.
-}
abort :: (MonadIO m) => SKDocument -> m ()
abort document = evalContIO do
    document' <- useObj document
    liftIO $ sk_document_abort document'
