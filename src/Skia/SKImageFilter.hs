module Skia.SKImageFilter where

import Control.Exception qualified
import Data.Vector.Storable qualified as VS
import Linear
import Skia.Internal.Prelude
import Control.Monad.Trans.Resource

{- | Create a filter that implements a custom blend mode. Each output pixel is the result of
combining the corresponding background and foreground pixels using the 4 coefficients:

@
   k1 * foreground * background + k2 * foreground + k3 * background + k4
@
-}
createArithmetic ::
    (MonadResource m) =>
    -- | (k1, k2, k3, k4). The four coefficients used to combine the foreground
    -- and background.
    (Float, Float, Float, Float) ->
    -- | \"enforcePMColor\". If true, the RGB channels will be clamped to the
    -- calculated alpha.
    Bool ->
    -- | The background content, using the source bitmap when this is 'Nothing'.
    Maybe SKImageFilter ->
    -- | The foreground content, using the source bitmap when this is 'Nothing'.
    Maybe SKImageFilter ->
    -- | Optional rectangle that crops the inputs and output.
    Maybe (Rect Float) ->
    m (ReleaseKey, SKImageFilter)
createArithmetic (k1, k2, k3, k4) enforcePMColor background foreground cropRect =
    allocateSKObject
        ( evalContIO do
            cropRect' <- useNullIfNothing useStorable $ fmap toSKRect $ cropRect
            liftIO $
                sk_imagefilter_new_arithmetic
                    (coerce k1)
                    (coerce k2)
                    (coerce k3)
                    (coerce k4)
                    (fromBool enforcePMColor)
                    (ptrOrNull background)
                    (ptrOrNull foreground)
                    cropRect'
        )
        sk_imagefilter_unref

{- | This filter takes an 'SKBlendMode' and uses it to composite the two filters
together.
-}
createBlend ::
    (MonadIO m) =>
    -- | The blend mode that defines the compositing operation
    SKBlendMode ->
    -- | \"background\". The Dst pixels used in blending, if 'Nothing' the source bitmap is used.
    Maybe SKImageFilter ->
    -- | \"foreground\". The Dst pixels used in blending, if 'Nothing' the source bitmap is used.
    Maybe SKImageFilter ->
    -- | Optional rectangle that crops the inputs and output.
    Maybe (Rect Float) ->
    m SKImageFilter
createBlend blendMode background foreground cropRect = evalContIO do
    background' <- useNullIfNothing useObj background
    foreground' <- useNullIfNothing useObj foreground
    cropRect' <- useNullIfNothing useStorable $ fmap toSKRect $ cropRect
    result' <- liftIO $ sk_imagefilter_new_blend (marshalSKEnum blendMode) background' foreground' cropRect'
    toObjectFin sk_imagefilter_unref result'

-- | This filter takes an 'SKBlender' and uses it to composite the two filters together.
createBlender ::
    (MonadIO m) =>
    SKBlender ->
    -- | \"background\". The Dst pixels used in blending, if 'Nothing' the source bitmap is used.
    Maybe SKImageFilter ->
    -- | \"foreground\". The Dst pixels used in blending, if 'Nothing' the source bitmap is used.
    Maybe SKImageFilter ->
    -- | Optional rectangle that crops the inputs and output.
    Maybe (Rect Float) ->
    m SKImageFilter
createBlender blender background foreground cropRect = evalContIO do
    blender' <- useObj blender
    background' <- useNullIfNothing useObj background
    foreground' <- useNullIfNothing useObj foreground
    cropRect' <- useNullIfNothing useStorable $ fmap toSKRect $ cropRect
    result' <- liftIO $ sk_imagefilter_new_blender blender' background' foreground' cropRect'
    toObjectFin sk_imagefilter_unref result'

createBlur ::
    (MonadIO m) =>
    -- | The Gaussian sigma values for blurring along the X and Y axes
    -- respectively.
    V2 Float ->
    -- | The tile mode applied at edges.
    SKShaderTileMode ->
    -- | The input filter that is blurred, uses source bitmap if this is
    -- 'Nothing'.
    Maybe SKImageFilter ->
    -- | Optional rectangle that crops the inputs and output.
    Maybe (Rect Float) ->
    m SKImageFilter
createBlur (V2 sigmaX sigmaY) tileMode input cropRect = evalContIO do
    input' <- useNullIfNothing useObj input
    cropRect' <- useNullIfNothing useStorable $ fmap toSKRect $ cropRect
    result' <- liftIO $ sk_imagefilter_new_blur (coerce sigmaX) (coerce sigmaY) (marshalSKEnum tileMode) input' cropRect'
    toObjectFin sk_imagefilter_unref result'

-- | Create a filter that applies the color filter to the input filter results.
createColorFilter ::
    (MonadIO m) =>
    -- | The color filter that transforms the input image.
    SKColorFilter ->
    -- | The input filter, or uses the source bitmap if this is 'Nothing'.
    -- | Optional rectangle that crops the inputs and output.
    Maybe SKImageFilter ->
    -- | Optional rectangle that crops the inputs and output.
    Maybe (Rect Float) ->
    m SKImageFilter
createColorFilter cfilter input cropRect = evalContIO do
    cfilter' <- useObj cfilter
    input' <- useNullIfNothing useObj input
    cropRect' <- useNullIfNothing useStorable $ fmap toSKRect $ cropRect
    result' <- liftIO $ sk_imagefilter_new_color_filter cfilter' input' cropRect'
    toObjectFin sk_imagefilter_unref result'

{- | Create a filter that composes \"inner\" with \"outer\", such that the
results of \"inner\" are treated as the source bitmap passed to \"outer\",
i.e.

@
result = outer(inner(source)).
@
-}
createCompose ::
    (MonadIO m) =>
    -- | \"outer\": The outer filter that evaluates the results of inner.
    SKImageFilter ->
    -- | \"inner\": The inner filter that produces the input to outer.
    SKImageFilter ->
    m SKImageFilter
createCompose outer inner = evalContIO do
    outer' <- useObj outer
    inner' <- useObj inner
    result' <- liftIO $ sk_imagefilter_new_compose outer' inner'
    toObjectFin sk_imagefilter_unref result'

{- | Create a filter that moves each pixel in its color input based on an (x,y)
 vector encoded in its displacement input filter. Two color components of the
 displacement image are mapped into a vector as:

@ scale * (color[xChannel], color[yChannel]) @

...where the channel selectors are one of R, G, B, or A.
-}
createDisplacementMapEffect ::
    (MonadIO m) =>
    -- | \"xChannelSelector\". RGBA channel that encodes the x displacement per pixel.
    SKColorChannel ->
    -- | \"yChannelSelector\". RGBA channel that encodes the y displacement per pixel.
    SKColorChannel ->
    -- | \"scale\". Scale applied to displacement extracted from image.
    Float ->
    -- | \"displacement\". The filter defining the displacement image, or null to use source.
    Maybe SKImageFilter ->
    -- | \"color\". The filter providing the color pixels to be displaced. If 'Nothing',
    -- it will use the source.
    Maybe SKImageFilter ->
    -- | \"cropRect\". Optional rectangle that crops the color input and output.
    Maybe (Rect Float) ->
    m SKImageFilter
createDisplacementMapEffect xChannelSelector yChannelSelector scale displacement color cropRect = evalContIO do
    displacement' <- useNullIfNothing useObj displacement
    color' <- useNullIfNothing useObj color
    cropRect' <- useNullIfNothing useStorable $ fmap toSKRect cropRect
    result' <-
        liftIO $
            sk_imagefilter_new_displacement_map_effect
                (marshalSKEnum xChannelSelector)
                (marshalSKEnum yChannelSelector)
                (coerce scale)
                displacement'
                color'
                cropRect'
    toObjectFin sk_imagefilter_unref result'

{- | Create a filter that draws a drop shadow under the input content. This
 filter produces an image that includes the inputs' content.
-}
createDropShadow ::
    (MonadIO m) =>
    -- | The X and Y offset of the shadow.
    V2 Float ->
    -- | The blur radii for the shadow, along the X and Y axis respectively.
    V2 Float ->
    -- | \"color\". The color of the drop shadow.
    SKColor ->
    -- | \"input\". The input filter, or will use the source bitmap if this is 'Nothing'.
    Maybe SKImageFilter ->
    -- | \"cropRect\". Optional rectangle that crops the input and output.
    Maybe (Rect Float) ->
    m SKImageFilter
createDropShadow (V2 dx dy) (V2 sigmaX sigmaY) color input cropRect = evalContIO do
    input' <- useNullIfNothing useObj input
    cropRect' <- useNullIfNothing useStorable $ fmap toSKRect cropRect
    result' <-
        liftIO $
            sk_imagefilter_new_drop_shadow
                (coerce dx)
                (coerce dy)
                (coerce sigmaX)
                (coerce sigmaY)
                (coerce color)
                input'
                cropRect'
    toObjectFin sk_imagefilter_unref result'

{- | Create a filter that renders a drop shadow, in exactly the same manner as
 'createDropShadow', except that the resulting image does not include the input
 content. This allows the shadow and input to be composed by a filter DAG in a
 more flexible manner.
-}
createDropShadowOnly ::
    (MonadIO m) =>
    -- | The X and Y offset of the shadow.
    V2 Float ->
    -- | The blur radii for the shadow, along the X and Y axis respectively.
    V2 Float ->
    -- | \"color\". The color of the drop shadow.
    SKColor ->
    -- | \"input\". The input filter, or will use the source bitmap if this is null.
    Maybe SKImageFilter ->
    -- | \"cropRect\". Optional rectangle that crops the input and output.
    Maybe (Rect Float) ->
    m SKImageFilter
createDropShadowOnly (V2 dx dy) (V2 sigmaX sigmaY) color input cropRect = evalContIO do
    input' <- useNullIfNothing useObj input
    cropRect' <- useNullIfNothing useStorable $ fmap toSKRect cropRect
    result' <-
        liftIO $
            sk_imagefilter_new_drop_shadow_only
                (coerce dx)
                (coerce dy)
                (coerce sigmaX)
                (coerce sigmaY)
                (coerce color)
                input'
                cropRect'
    toObjectFin sk_imagefilter_unref result'

{- | Create a filter that draws the 'srcRect' portion of image into 'dstRect'
 using the given filter quality. Similar to 'SKCanvas.drawImageRect'. The
 returned image filter evaluates to transparent black if the input image is
 'Nothing'.
-}
createImage ::
    (MonadIO m) =>
    -- | The image that is output by the filter, subset by 'srcRect'.
    Maybe SKImage ->
    -- | \"srcRect\". The source pixels sampled into 'dstRect'.
    Rect Float ->
    -- | \"dstRect\". The local rectangle to draw the image into.
    Rect Float ->
    -- | The sampling to use when drawing the image.
    SKSamplingOptions ->
    m SKImageFilter
createImage image srcRect dstRect sampling = evalContIO do
    image' <- useNullIfNothing useObj image
    srcRect' <- useStorable $ toSKRect srcRect
    dstRect' <- useStorable $ toSKRect dstRect
    sampling' <- useStorable $ marshalSKSamplingOptions sampling
    result' <- liftIO $ sk_imagefilter_new_image image' srcRect' dstRect' sampling'
    toObjectFin sk_imagefilter_unref result'

{- | Create a filter that draws the image using the given sampling. Similar to
 'SKCanvas.drawImage'. The returned image filter evaluates to transparent black
 if 'image' is 'Nothing'.
-}
createImageSimple ::
    (MonadIO m) =>
    -- | The image that is output by the filter.
    Maybe SKImage ->
    -- | The sampling to use when drawing the image.
    SKSamplingOptions ->
    m SKImageFilter
createImageSimple image sampling = evalContIO do
    image' <- useNullIfNothing useObj image
    sampling' <- useStorable $ marshalSKSamplingOptions sampling
    result' <- liftIO $ sk_imagefilter_new_image_simple image' sampling'
    toObjectFin sk_imagefilter_unref result'

-- | Create a filter that fills 'lensBounds' with a magnification of the input.
createMagnifier ::
    (MonadIO m) =>
    -- | \"lensBounds\". The outer bounds of the magnifier effect.
    Rect Float ->
    -- | \"zoomAmount\". The amount of magnification applied to the input image.
    Float ->
    -- | \"inset\". The size or width of the fish-eye distortion around the magnified content.
    Float ->
    -- | \"sampling\". The SkSamplingOptions applied to the input image when magnified.
    SKSamplingOptions ->
    -- | \"input\". The input filter that is magnified; if null the source bitmap is used.
    Maybe SKImageFilter ->
    -- | \"cropRect\". Optional rectangle that crops the input and output.
    Maybe (Rect Float) ->
    m SKImageFilter
createMagnifier lensBounds zoomAmount inset sampling input cropRect = evalContIO do
    lensBounds' <- useStorable $ toSKRect lensBounds
    sampling' <- useStorable $ marshalSKSamplingOptions sampling
    input' <- useNullIfNothing useObj input
    cropRect' <- useNullIfNothing useStorable $ fmap toSKRect cropRect
    result' <-
        liftIO $
            sk_imagefilter_new_magnifier
                lensBounds'
                (coerce zoomAmount)
                (coerce inset)
                sampling'
                input'
                cropRect'
    toObjectFin sk_imagefilter_unref result'

{- | Create a filter that applies an NxM image processing kernel to the input image. This can be
 used to produce effects such as sharpening, blurring, edge detection, etc.
-}
createMatrixConvolution ::
    (MonadIO m) =>
    -- | The kernel size in pixels, in each dimension (N by M).
    V2 Int ->
    -- | The image processing kernel. Must contain N * M elements, in row order.
    VS.Vector Float ->
    -- | \"gain\". A scale factor applied to each pixel after convolution.
    Float ->
    -- | \"bias\". A bias factor added to each pixel after convolution.
    Float ->
    -- | \"kernelOffset\". An offset applied to each pixel coordinate before convolution.
    V2 Int ->
    -- | \"tileMode\". How accesses outside the image are treated.
    SKShaderTileMode ->
    -- | \"convolveAlpha\". If true, all channels are convolved.
    Bool ->
    -- | \"input\". The input image filter, if 'Nothing' the source bitmap is used instead.
    Maybe SKImageFilter ->
    -- | \"cropRect\". Optional rectangle to which the output processing will be limited.
    Maybe (Rect Float) ->
    m SKImageFilter
createMatrixConvolution kernelSize@(V2 kernelW kernelH) kernel gain bias kernelOffset tileMode convolveAlpha input cropRect = evalContIO do
    when (VS.length kernel /= kernelW * kernelH) do
        let msg = "The specified kernel size is " <> show kernelSize <> "), but kernel has " <> show (VS.length kernel) <> " element(s)."
        liftIO $ Control.Exception.throwIO $ BadArgumentError msg

    kernelSize' <- useStorable $ toSKISize kernelSize
    kernel' <- useStorableVector kernel
    kernelOffset' <- useStorable $ toSKIPoint kernelOffset
    input' <- useNullIfNothing useObj input
    cropRect' <- useNullIfNothing useStorable $ fmap toSKRect cropRect
    result' <-
        liftIO $
            sk_imagefilter_new_matrix_convolution
                kernelSize'
                (coercePtr kernel')
                (coerce gain)
                (coerce bias)
                kernelOffset'
                (marshalSKEnum tileMode)
                (fromBool convolveAlpha)
                input'
                cropRect'
    toObjectFin sk_imagefilter_unref result'

{- | Create a filter that transforms the input image by 'matrix'. This matrix
 transforms the local space, which means it effectively happens prior to any
 transformation coming from the SkCanvas initiating the filtering.
-}
createMatrixTransform ::
    (MonadIO m) =>
    -- | \"matrix\". The matrix to apply to the original content.
    M33 Float ->
    -- | \"sampling\". How the image will be sampled when it is transformed.
    SKSamplingOptions ->
    -- | \"input\". The image filter to transform, or null to use the source image.
    Maybe SKImageFilter ->
    m SKImageFilter
createMatrixTransform matrix sampling input = evalContIO do
    matrix' <- useStorable $ toSKMatrix matrix
    sampling' <- useStorable $ marshalSKSamplingOptions sampling
    input' <- useNullIfNothing useObj input
    result' <- liftIO $ sk_imagefilter_new_matrix_transform matrix' sampling' input'
    toObjectFin sk_imagefilter_unref result'

{- | Create a filter that merges the filters together by drawing their results
 in order with src-over blending.
-}
createMerge ::
    (MonadIO m) =>
    -- | The input filters to merge.
    [SKImageFilter] ->
    -- | Optional rectangle that crops all input filters and the output.
    Maybe (Rect Float) ->
    m SKImageFilter
createMerge filters cropRect = evalContIO do
    cropRect' <- useNullIfNothing useStorable $ fmap toSKRect cropRect
    filters <- traverse useObj filters
    (filters', numFilters) <- ContT $ withArrayLen' filters
    result' <- liftIO $ sk_imagefilter_new_merge filters' (fromIntegral numFilters) cropRect'
    toObjectFin sk_imagefilter_unref result'

-- | Create a filter that merges the results of the two filters together with src-over blending.
createMergeSimple ::
    (MonadIO m) =>
    -- | The first input filter, or the source bitmap if this is 'Nothing'.
    Maybe SKImageFilter ->
    -- | The second input filter, or the source bitmap if this is 'Nothing'.
    Maybe SKImageFilter ->
    -- | Optional rectangle that crops the inputs and output.
    Maybe (Rect Float) ->
    m SKImageFilter
createMergeSimple first second cropRect = evalContIO do
    first' <- useNullIfNothing useObj first
    second' <- useNullIfNothing useObj second
    cropRect' <- useNullIfNothing useStorable $ fmap toSKRect cropRect
    result' <- liftIO $ sk_imagefilter_new_merge_simple first' second' cropRect'
    toObjectFin sk_imagefilter_unref result'

-- | Create a filter that offsets the input filter by the given vector.
createOffset ::
    (MonadIO m) =>
    -- | The x and y offset in local space that the image is shifted.
    V2 Float ->
    -- | The input that will be moved, if 'Nothing' the source bitmap is used instead.
    Maybe SKImageFilter ->
    -- | Optional rectangle to crop the input and output.
    Maybe (Rect Float) ->
    m SKImageFilter
createOffset (V2 dx dy) input cropRect = evalContIO do
    input' <- useNullIfNothing useObj input
    cropRect' <- useNullIfNothing useStorable $ fmap toSKRect cropRect
    result' <- liftIO $ sk_imagefilter_new_offset (coerce dx) (coerce dy) input' cropRect'
    toObjectFin sk_imagefilter_unref result'

{- | Create a filter that produces the SkPicture as its output, clipped to both
 'targetRect' and the picture's internal cull rect.

If the input picture is 'Nothing', the returned image filter produces
transparent black.
-}
createPictureWithRect ::
    (MonadIO m) =>
    -- | The picture that is drawn for the filter output.
    Maybe SKPicture ->
    -- | The drawing region for the picture.
    Rect Float ->
    m SKImageFilter
createPictureWithRect pic targetRect = evalContIO do
    pic' <- useNullIfNothing useObj pic
    targetRect' <- useStorable $ toSKRect targetRect
    result' <- liftIO $ sk_imagefilter_new_picture_with_rect pic' targetRect'
    toObjectFin sk_imagefilter_unref result'

{- | As 'createPictureWithRect', but uses 'SKPicture'\'s cullRect for the
drawing region.

If the input picture is 'Nothing', the returned image filter produces
transparent black.
-}
createPicture ::
    (MonadIO m) =>
    -- | The picture that is drawn for the filter output.
    Maybe SKPicture ->
    m SKImageFilter
createPicture pic = evalContIO do
    pic' <- useNullIfNothing useObj pic
    result' <- liftIO $ sk_imagefilter_new_picture pic'
    toObjectFin sk_imagefilter_unref result'

createShader ::
    (MonadIO m) =>
    SKShader ->
    -- | Use dither?
    Bool ->
    -- | Optional rectangle to crop the input and output.
    Maybe (Rect Float) ->
    m SKImageFilter
createShader shader dither cropRect = evalContIO do
    shader' <- useObj shader
    cropRect' <- useNullIfNothing useStorable $ fmap toSKRect cropRect
    result <- liftIO $ sk_imagefilter_new_shader shader' (fromBool dither) cropRect'
    toObjectFin sk_imagefilter_unref result

-- | Create a tile image filter.
createTile ::
    (MonadIO m) =>
    -- | \"src\". Defines the pixels to tile
    Rect Float ->
    -- | \"dst\". Defines the pixel region that the tiles will be drawn to
    Rect Float ->
    -- | The input that will be tiled, if 'Nothing' the source bitmap is used
    -- instead.
    Maybe SKImageFilter ->
    m SKImageFilter
createTile src dst input = evalContIO do
    src' <- useStorable $ toSKRect src
    dst' <- useStorable $ toSKRect dst
    input' <- useNullIfNothing useObj input
    result' <- liftIO $ sk_imagefilter_new_tile src' dst' input'
    toObjectFin sk_imagefilter_unref result'

{- | Create a filter that dilates each input pixel's channel values to the max
value within the given radii along the x and y axes.
-}
createDilate ::
    (MonadIO m) =>
    -- | The distance to dilate along the x and Y axes to either side of each
    -- pixel.
    V2 Float ->
    -- | The input that will be tiled, if 'Nothing' the source bitmap is used
    -- instead.
    Maybe SKImageFilter ->
    -- | Optional rectangle to crop the input and output.
    Maybe (Rect Float) ->
    m SKImageFilter
createDilate (V2 rx ry) input cropRect = evalContIO do
    input' <- useNullIfNothing useObj input
    cropRect' <- useNullIfNothing useStorable $ fmap toSKRect $ cropRect
    result' <- liftIO $ sk_imagefilter_new_dilate (coerce rx) (coerce ry) input' cropRect'
    toObjectFin sk_imagefilter_unref result'

{- | Create a filter that erodes each input pixel's channel values to the
minimum channel value within the given radii along the x and y axes.
-}
createErode ::
    (MonadIO m) =>
    -- | The distance to dilate along the x and Y axes to either side of each
    -- pixel.
    V2 Float ->
    -- | The input that will be tiled, if 'Nothing' the source bitmap is used
    -- instead.
    Maybe SKImageFilter ->
    -- | Optional rectangle to crop the input and output.
    Maybe (Rect Float) ->
    m SKImageFilter
createErode (V2 rx ry) input cropRect = evalContIO do
    input' <- useNullIfNothing useObj input
    cropRect' <- useNullIfNothing useStorable $ fmap toSKRect $ cropRect
    result' <- liftIO $ sk_imagefilter_new_erode (coerce rx) (coerce ry) input' cropRect'
    toObjectFin sk_imagefilter_unref result'

{-
Constructs 'Light' and 'Material' can be found in Google Skia's
src/effects/imagefilters/SkLightingImageFilter.cpp.
-}

data Light
    = Light'Point
        { location :: V3 Float
        }
    | Light'Distant
        { direction :: V3 Float
        }
    | Light'Spot
        { location :: V3 Float
        , target :: V3 Float
        , falloffExponent :: Float
        , cutoffAngle :: Degrees
        }
    deriving (Show, Eq, Ord)

data Material
    = Material'Diffuse
        { kd :: Float
        -- ^ Diffuse reflectance coefficient
        }
    | Material'Specular
        { ks :: Float
        -- ^ Specular reflectance coefficient.
        , shininess :: Float
        -- ^ Specular exponent determining how shiny the surface is.
        }
    deriving (Show, Eq, Ord)

{- | Create a filter that calculates the illumination from a light source.

This function is a unified interface for the following functions:

    * 'sk_imagefilter_new_point_lit_diffuse'

    * 'sk_imagefilter_new_point_lit_specular'

    * 'sk_imagefilter_new_distant_lit_diffuse'

    * 'sk_imagefilter_new_distant_lit_specular'

    * 'sk_imagefilter_new_spot_lit_diffuse'

    * 'sk_imagefilter_new_spot_lit_specular'
-}
createLighting ::
    (MonadIO m) =>
    -- | Light source
    Light ->
    -- | Light color
    SKColor ->
    -- | Material information
    Material ->
    -- | \"surfaceScale\": Scale factor to transform from alpha values to
    -- physical height.
    --
    -- For reference, Google's Skia uses 0.0 as the default.
    Float ->
    -- | The input filter that defines surface normals (as alpha), or uses the source bitmap when 'Nothing'.
    Maybe SKImageFilter ->
    -- | Optional rectangle to crop the input and output.
    Maybe (Rect Float) ->
    m SKImageFilter
createLighting light lightColor material surfaceScale input cropRect = evalContIO do
    let
        elimLight ::
            ((forall a. (Ptr Sk_point3 -> Sk_color -> a) -> a) -> ContT r IO b) ->
            ((forall a. (Ptr Sk_point3 -> Sk_color -> a) -> a) -> ContT r IO b) ->
            ((forall a. (Ptr Sk_point3 -> Ptr Sk_point3 -> CFloat -> CFloat -> Sk_color -> a) -> a) -> ContT r IO b) ->
            ContT r IO b
        elimLight contPoint contDistant contSpot = do
            case light of
                Light'Point{location} -> do
                    location' <- useStorable $ toSKPoint3 location
                    contPoint \f -> f location' (coerce lightColor)
                Light'Distant{direction} -> do
                    direction' <- useStorable $ toSKPoint3 direction
                    contDistant \f -> f direction' (coerce lightColor)
                Light'Spot{location, target, falloffExponent, cutoffAngle} -> do
                    location' <- useStorable $ toSKPoint3 location
                    target' <- useStorable $ toSKPoint3 target
                    contSpot \f -> f location' target' (coerce falloffExponent) (coerce cutoffAngle) (coerce lightColor)

        elimMaterial ::
            ((forall a. (CFloat -> CFloat -> a) -> a) -> ContT r IO b) ->
            ((forall a. (CFloat -> CFloat -> CFloat -> a) -> a) -> ContT r IO b) ->
            ContT r IO b
        elimMaterial contDiffuse contSpecular = do
            case material of
                Material'Diffuse{kd} -> do
                    contDiffuse \f -> f (coerce surfaceScale) (coerce kd)
                Material'Specular{ks, shininess} -> do
                    contSpecular \f -> f (coerce surfaceScale) (coerce ks) (coerce shininess)

        aSuffix ::
            (Ptr Sk_imagefilter -> Ptr Sk_rect -> IO (Ptr Sk_imagefilter)) ->
            ContT r IO SKImageFilter
        aSuffix f = do
            input' <- useNullIfNothing useObj input
            cropRect' <- useNullIfNothing useStorable $ fmap toSKRect $ cropRect
            imfilter' <- liftIO $ f input' cropRect'
            toObjectFin sk_imagefilter_unref imfilter'

    elimLight
        ( \aPoint ->
            elimMaterial
                (\aDiffuse -> sk_imagefilter_new_point_lit_diffuse & aPoint & aDiffuse & aSuffix)
                (\aSpecular -> sk_imagefilter_new_point_lit_specular & aPoint & aSpecular & aSuffix)
        )
        ( \aDistant ->
            elimMaterial
                (\aDiffuse -> sk_imagefilter_new_distant_lit_diffuse & aDistant & aDiffuse & aSuffix)
                (\aSpecular -> sk_imagefilter_new_distant_lit_specular & aDistant & aSpecular & aSuffix)
        )
        ( \aSpot ->
            elimMaterial
                (\aDiffuse -> sk_imagefilter_new_spot_lit_diffuse & aSpot & aDiffuse & aSuffix)
                (\aSpecular -> sk_imagefilter_new_spot_lit_specular & aSpot & aSpecular & aSuffix)
        )
