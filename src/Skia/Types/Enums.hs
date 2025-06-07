module Skia.Types.Enums where

import NeatInterpolation
import Skia.Bindings
import Skia.Types.Internal.Utils.TH

$( qGenerateSKEnum
    "SKColorType"
    ''Sk_colortype
    [trimming|
        Describes how pixel bits encode color. A pixel may be an alpha mask, a grayscale, RGB, or ARGB.

        'Skia.SKColorType.getNative32' selects the native 32-bit ARGB format for the current configuration.
    |]
    [ ("Unknown", 'UNKNOWN_SK_COLORTYPE, "Uninitialized")
    , ("Alpha'8", 'ALPHA_8_SK_COLORTYPE, "Pixel with alpha in 8-bit byte")
    , ("RGB'565", 'RGB_565_SK_COLORTYPE, "Pixel with 5 bits red, 6 bits green, 5 bits blue, in 16-bit word")
    , ("ARGB'4444", 'ARGB_4444_SK_COLORTYPE, "Pixel with 4 bits for alpha, red, green, blue; in 16-bit word")
    , ("RGBA'8888", 'RGBA_8888_SK_COLORTYPE, "Pixel with 8 bits for red, green, blue, alpha; in 32-bit word")
    , ("RGB'888x", 'RGB_888X_SK_COLORTYPE, "Pixel with 8 bits each for red, green, blue; in 32-bit word")
    , ("BGRA'8888", 'BGRA_8888_SK_COLORTYPE, "Pixel with 8 bits for blue, green, red, alpha; in 32-bit word")
    , ("RGBA'1010102", 'RGBA_1010102_SK_COLORTYPE, "10 bits for red, green, blue; 2 bits for alpha; in 32-bit word")
    , ("BGRA'1010102", 'BGRA_1010102_SK_COLORTYPE, "10 bits for blue, green, red; 2 bits for alpha; in 32-bit word")
    , ("RGB'101010x", 'RGB_101010X_SK_COLORTYPE, "Pixel with 10 bits each for red, green, blue; in 32-bit word")
    , ("BGR'101010x", 'BGR_101010X_SK_COLORTYPE, "Pixel with 10 bits each for blue, green, red; in 32-bit word")
    , ("BGR'101010x'XR", 'BGR_101010X_XR_SK_COLORTYPE, "Pixel with 10 bits each for blue, green, red; in 32-bit word, extended range")
    , ("RGBA'10x6", 'RGBA_10X6_SK_COLORTYPE, "Pixel with 10 used bits (most significant) followed by 6 unused bits for red, green, blue, alpha; in 64-bit word")
    , ("Gray'8", 'GRAY_8_SK_COLORTYPE, "Pixel with grayscale level in 8-bit byte")
    , ("RGBA'F16'Norm", 'RGBA_F16_NORM_SK_COLORTYPE, "Pixel with half floats in [0,1] for red, green, blue, alpha; in 64-bit word")
    , ("RGBA'F16", 'RGBA_F16_SK_COLORTYPE, "Pixel with half floats or red, green, blue, alpha; in 64-bit word")
    , ("RGBA'F32", 'RGBA_F32_SK_COLORTYPE, "Pixel using C float for red, green, blue, alpha; in 128-bit word")
    , ("SRGBA'8888", 'SRGBA_8888_SK_COLORTYPE, "") -- FIXME: Google Skia does not document this
    , ("R8'Unorm", 'R8_UNORM_SK_COLORTYPE, "") -- FIXME: Google Skia does not document this
    , ("Read'R8G8'Unorm", 'R8G8_UNORM_SK_COLORTYPE, "Pixel with a @uint8_t@ for red and green. This color type is only for reading from - not for rendering to")
    , ("Read'A16'Float", 'A16_FLOAT_SK_COLORTYPE, "Pixel with a half float for alpha. This color type is only for reading from - not for rendering to")
    , ("Read'R16G16'Float", 'R16G16_FLOAT_SK_COLORTYPE, "Pixel with a half float for red and green. This color type is only for reading from - not for rendering to")
    , ("Read'A16'Unorm", 'A16_UNORM_SK_COLORTYPE, "Pixel with a little endian @uint16_t@ for alpha. This color type is only for reading from - not for rendering to")
    , ("Read'R16G16'Unorm", 'R16G16_UNORM_SK_COLORTYPE, "Pixel with a little endian @uint16_t@ for red and green. This color type is only for reading from - not for rendering to")
    , ("Read'R16G16B16A16'Unorm", 'R16G16B16A16_UNORM_SK_COLORTYPE, "Pixel with a little endian @uint16_t@ for red, green, blue, and alpha. This color type is only for reading from - not for rendering to")
    ]
 )

$( qGenerateSKEnum
    "SKAlphaType"
    ''Sk_alphatype
    [trimming|
        Describes how to interpret the alpha component of a pixel. A pixel may
        be opaque, or alpha, describing multiple levels of transparency.

        In simple blending, alpha weights the draw color and the destination
        color to create a new color. If alpha describes a weight from zero to one:

        @
        new color = draw color * alpha + destination color * (1 - alpha)
        @

        In practice alpha is encoded in two or more bits, where 1.0 equals all bits set.

        RGB may have alpha included in each component value; the stored
        value is the original RGB multiplied by alpha. Premultiplied color
        components improve performance.
    |]
    [ ("Unknown", 'UNKNOWN_SK_ALPHATYPE, "Uninitialized")
    , ("Opaque", 'OPAQUE_SK_ALPHATYPE, "Pixel is opaque")
    , ("Premul", 'PREMUL_SK_ALPHATYPE, "Pixel components are premultiplied by alpha")
    , ("Unpremul", 'UNPREMUL_SK_ALPHATYPE, "Pixel components are independent of alpha")
    ]
 )

$( qGenerateSKEnum
    "SKPixelGeometry"
    ''Sk_pixelgeometry
    [trimming|
        Description of how the LCD strips are arranged for each pixel. If this is unknown, or the
        pixels are meant to be "portable" and/or transformed before showing (e.g. rotated, scaled)
        then use 'SKPixelGeometry'Unknown'.
    |]
    [ ("Unknown", 'UNKNOWN_SK_PIXELGEOMETRY, "")
    , ("RGB'H", 'RGB_H_SK_PIXELGEOMETRY, "")
    , ("BGR'H", 'BGR_H_SK_PIXELGEOMETRY, "")
    , ("RGB'V", 'RGB_V_SK_PIXELGEOMETRY, "")
    , ("BGR'V", 'BGR_V_SK_PIXELGEOMETRY, "")
    ]
 )

$( qGenerateSKEnum
    "SKBlendMode"
    ''Sk_blendmode
    [trimming|
        Blends are operators that take in two colors (source, destination) and return a new color.
        Many of these operate the same on all 4 components: red, green, blue, alpha. For these,
        we just document what happens to one component, rather than naming each one separately.

        Different SkColorTypes have different representations for color components:

        @
            8-bit: 0..255
            6-bit: 0..63
            5-bit: 0..31
            4-bit: 0..15
           floats: 0...1
        @

        The documentation is expressed as if the component values are always 0..1 (floats).
        
        For brevity, the documentation uses the following abbreviations

        @
        s  : source
        d  : destination
        sa : source alpha
        da : destination alpha
        @

        Results are abbreviated

        @
        r  : if all 4 components are computed in the same manner
        ra : result alpha component
        rc : result "color": red, green, blue components
        @
    |]
    [ ("Clear", 'CLEAR_SK_BLENDMODE, "@r = 0@")
    , ("Src", 'SRC_SK_BLENDMODE, "@r = s@")
    , ("Dst", 'DST_SK_BLENDMODE, "@r = d@")
    , ("SrcOver", 'SRCOVER_SK_BLENDMODE, "@r = s + (1-sa)*d@")
    , ("DstOver", 'DSTOVER_SK_BLENDMODE, "@r = d + (1-da)*s@")
    , ("SrcIn", 'SRCIN_SK_BLENDMODE, "@r = s * da@")
    , ("DstIn", 'DSTIN_SK_BLENDMODE, "@r = d * sa@")
    , ("SrcOut", 'SRCOUT_SK_BLENDMODE, "@r = s * (1-da)@")
    , ("DstOut", 'DSTOUT_SK_BLENDMODE, "@r = d * (1-sa)@")
    , ("SrcATop", 'SRCATOP_SK_BLENDMODE, "@r = s*da + d*(1-sa)@")
    , ("DstATop", 'DSTATOP_SK_BLENDMODE, "@r = d*sa + s*(1-da)@")
    , ("Xor", 'XOR_SK_BLENDMODE, "@r = s*(1-da) + d*(1-sa)@")
    , ("Plus", 'PLUS_SK_BLENDMODE, "@r = min(s + d, 1)@")
    , ("Modulate", 'MODULATE_SK_BLENDMODE, "@r = s*d@")
    , ("Screen", 'SCREEN_SK_BLENDMODE, "@r = s + d - s*d@")
    , ("Overlay", 'OVERLAY_SK_BLENDMODE, "Multiply or screen, depending on destination")
    , ("Darken", 'DARKEN_SK_BLENDMODE, "@rc = s + d - max(s*da, d*sa), ra = 'SKBlendMode'SrcOver'@")
    , ("Lighten", 'LIGHTEN_SK_BLENDMODE, "@rc = s + d - min(s*da, d*sa), ra = 'SKBlendMode'SrcOver'@")
    , ("ColorDodge", 'COLORDODGE_SK_BLENDMODE, "Brighten destination to reflect source")
    , ("ColorBurn", 'COLORBURN_SK_BLENDMODE, "Darken destination to reflect source")
    , ("HardLight", 'HARDLIGHT_SK_BLENDMODE, "Multiply or screen, depending on source")
    , ("SoftLight", 'SOFTLIGHT_SK_BLENDMODE, "Lighten or darken, depending on source")
    , ("Difference", 'DIFFERENCE_SK_BLENDMODE, "@rc = s + d - 2*(min(s*da, d*sa)), ra = 'SKBlendMode'SrcOver'@")
    , ("Exclusion", 'EXCLUSION_SK_BLENDMODE, "@rc = s + d - two(s*d), ra = 'SKBlendMode'SrcOver'@")
    , ("Multiply", 'MULTIPLY_SK_BLENDMODE, "@r = s*(1-da) + d*(1-sa) + s*d@")
    , ("Hue", 'HUE_SK_BLENDMODE, "Hue of source with saturation and luminosity of destination")
    , ("Saturation", 'SATURATION_SK_BLENDMODE, "Saturation of source with hue and luminosity of destination")
    , ("Color", 'COLOR_SK_BLENDMODE, "Hue and saturation of source with luminosity of destination")
    , ("Luminosity", 'LUMINOSITY_SK_BLENDMODE, "Luminosity of source with hue and saturation of destination")
    ]
 )

$( qGenerateSKEnum
    "SKPointMode"
    ''Sk_point_mode
    [trimming|
        Selects if an array of points are drawn as discrete points, as lines, or as
        an open polygon.
    |]
    [ ("Points", 'POINTS_SK_POINT_MODE, "Draw each point separately")
    , ("Lines", 'LINES_SK_POINT_MODE, "Draw each pair of points as a line segment")
    , ("Polygon", 'POLYGON_SK_POINT_MODE, "Draw the array of points as an open polygon")
    ]
 )

$( qGenerateSKEnum
    "SKTextAlign"
    ''Sk_text_align
    [trimming|
        Specifies text alignment
    |]
    [ ("Left", 'LEFT_SK_TEXT_ALIGN, "")
    , ("Center", 'CENTER_SK_TEXT_ALIGN, "")
    , ("Right", 'RIGHT_SK_TEXT_ALIGN, "")
    ]
 )

$( qGenerateSKEnum
    "SKTextEncoding"
    ''Sk_text_encoding
    [trimming|
        Specifies how a given textual 'CString' (@char *@) is encoded
    |]
    [ ("UTF8", 'UTF8_SK_TEXT_ENCODING, "Uses bytes to represent UTF-8 or ASCII")
    , ("UTF16", 'UTF16_SK_TEXT_ENCODING, "Uses two byte words to represent most of Unicode")
    , ("UTF32", 'UTF32_SK_TEXT_ENCODING, "Uses four byte words to represent all of Unicode")
    , ("GlyphId", 'GLYPH_ID_SK_TEXT_ENCODING, "Uses 'Skia.Types.GlyphId' to represent glyph indices")
    ]
 )

$( qGenerateSKEnum
    "SKPathFillType"
    ''Sk_path_filltype
    "Specifies the method of filling an 'SKPath'"
    [ ("Winding", 'WINDING_SK_PATH_FILLTYPE, "Specifies that \"inside\" is computed by a non-zero sum of signed edge crossings")
    , ("EvenOdd", 'EVENODD_SK_PATH_FILLTYPE, "Specifies that \"inside\" is computed by an odd number of edge crossings")
    , ("InverseWinding", 'INVERSE_WINDING_SK_PATH_FILLTYPE, "Same as 'SKPathFillType'Winding', but draws outside of the path, rather than inside")
    , ("InverseEvenOdd", 'INVERSE_EVENODD_SK_PATH_FILLTYPE, "Same as 'SKPathFillType'EvenOdd', but draws outside of the path, rather than inside")
    ]
 )

$( qGenerateSKEnum
    "SKFontStyleSlant"
    ''Sk_font_style_slant
    "Specifies font slant. This type is associated with 'Skia.Types.SKFontStyle'."
    [ ("Upright", 'UPRIGHT_SK_FONT_STYLE_SLANT, "")
    , ("Italic", 'ITALIC_SK_FONT_STYLE_SLANT, "")
    , ("Oblique", 'OBLIQUE_SK_FONT_STYLE_SLANT, "")
    ]
 )

$( qGenerateSKEnum
    "SKColorChannel"
    ''Sk_color_channel
    "Describes different color channels one can manipulate"
    [ ("R", 'R_SK_COLOR_CHANNEL, "The red channel")
    , ("G", 'G_SK_COLOR_CHANNEL, "The green channel")
    , ("B", 'B_SK_COLOR_CHANNEL, "The blue channel")
    , ("A", 'A_SK_COLOR_CHANNEL, "The alpha channel")
    ]
 )

$( qGenerateSKEnum
    "SKRegionOp"
    ''Sk_region_op
    "The logical operations that can be performed when combining two 'SKRegion'."
    [ ("Difference", 'DIFFERENCE_SK_REGION_OP, "Target minus operand")
    , ("Intersect", 'INTERSECT_SK_REGION_OP, "Target intersected with operand")
    , ("Union", 'UNION_SK_REGION_OP, "Target unioned with operand")
    , ("Xor", 'XOR_SK_REGION_OP, "Target exclusive or with operand")
    , ("ReverseDifference", 'REVERSE_DIFFERENCE_SK_REGION_OP, "Operand minus target")
    , ("Replace", 'REPLACE_SK_REGION_OP, "Replace target with operand")
    ]
 )

$( qGenerateSKEnum
    "SKClipOp"
    ''Sk_clipop
    "The logical operations that can be performed when combining two clips."
    [ ("Difference", 'DIFFERENCE_SK_CLIPOP, "")
    , ("Intersect", 'INTERSECT_SK_CLIPOP, "")
    ]
 )

$( qGenerateSKEnum
    "SKEncodedImageFormat"
    ''Sk_encoded_image_format
    "Enum describing format of encoded image data."
    [ ("BMP", 'BMP_SK_ENCODED_FORMAT, "")
    , ("GIF", 'GIF_SK_ENCODED_FORMAT, "")
    , ("ICO", 'ICO_SK_ENCODED_FORMAT, "")
    , ("JPEG", 'JPEG_SK_ENCODED_FORMAT, "")
    , ("PNG", 'PNG_SK_ENCODED_FORMAT, "")
    , ("WBMP", 'WBMP_SK_ENCODED_FORMAT, "")
    , ("WEBP", 'WEBP_SK_ENCODED_FORMAT, "")
    , ("PKM", 'PKM_SK_ENCODED_FORMAT, "")
    , ("KTX", 'KTX_SK_ENCODED_FORMAT, "")
    , ("ASTC", 'ASTC_SK_ENCODED_FORMAT, "")
    , ("DNG", 'DNG_SK_ENCODED_FORMAT, "")
    , ("HEIF", 'HEIF_SK_ENCODED_FORMAT, "")
    , ("AVIF", 'AVIF_SK_ENCODED_FORMAT, "")
    , ("JPEGXL", 'JPEGXL_SK_ENCODED_FORMAT, "")
    ]
 )

$( qGenerateSKEnum
    "SKEncodedOrigin"
    ''Sk_encodedorigin
    [trimming|
        Specifies an origin point.

        See "Skia.SKEncodedOrigin" for interesting utility functions.

        These values match the orientation <https://www.exif.org/Exif2-2.PDF>.
    |]
    [ ("TopLeft", 'TOP_LEFT_SK_ENCODED_ORIGIN, "")
    , ("TopRight", 'TOP_RIGHT_SK_ENCODED_ORIGIN, "")
    , ("BottomRight", 'BOTTOM_RIGHT_SK_ENCODED_ORIGIN, "")
    , ("BottomLeft", 'BOTTOM_LEFT_SK_ENCODED_ORIGIN, "")
    , ("LeftTop", 'LEFT_TOP_SK_ENCODED_ORIGIN, "")
    , ("RightTop", 'RIGHT_TOP_SK_ENCODED_ORIGIN, "")
    , ("RightBottom", 'RIGHT_BOTTOM_SK_ENCODED_ORIGIN, "")
    , ("LeftBottom", 'LEFT_BOTTOM_SK_ENCODED_ORIGIN, "")
    -- , ("Default", 'DEFAULT_SK_ENCODED_ORIGIN, "") -- NOTE: This is a synonym of TopLeft. See include/codec/SkEncodedOrigin.h.
    ]
 )

$( qGenerateSKEnum
    "SKCodecResult"
    ''Sk_codec_result
    "Error codes for various \"Skia.SKCodec\" methods."
    [ ("Success", 'SUCCESS_SK_CODEC_RESULT, "General return value for success.")
    , ("IncompleteInput", 'INCOMPLETE_INPUT_SK_CODEC_RESULT, "The input is incomplete. A partial image was generated.")
    ,
        ( "ErrorInInput"
        , 'ERROR_IN_INPUT_SK_CODEC_RESULT
        , [trimming|
            Like 'SKCodecResult.IncompleteInput', except the input had an error.

            If returned from an incremental decode, decoding cannot continue, even with more data.
        |]
        )
    , ("InvalidConversion", 'INVALID_CONVERSION_SK_CODEC_RESULT, "The generator cannot convert to match the request, ignoring dimensions.")
    , ("InvalidScale", 'INVALID_SCALE_SK_CODEC_RESULT, "The generator cannot scale to requested size.")
    , ("InvalidParameters", 'INVALID_PARAMETERS_SK_CODEC_RESULT, "Parameters (besides info) are invalid. e.g. NULL pixels, rowBytes too small, etc.")
    , ("InvalidInput", 'INVALID_INPUT_SK_CODEC_RESULT, "The input did not contain a valid image.")
    , ("CouldNotRewind", 'COULD_NOT_REWIND_SK_CODEC_RESULT, "Fulfilling this request requires rewinding the input, which is not supported for this input.")
    , ("InternalError", 'INTERNAL_ERROR_SK_CODEC_RESULT, "An internal error, such as out-of-memory.")
    , ("Unimplemented", 'UNIMPLEMENTED_SK_CODEC_RESULT, "This method is not implemented by this codec.")
    ]
 )

{-
'SKCodecZeroInitialized' can be replaced by 'Bool'.

\$( qGenerateSKEnum
    "SKCodecZeroInitialized"
    ''Sk_codec_zero_initialized
    ""
    [ ("Yes", 'YES_SK_CODEC_ZERO_INITIALIZED, "")
    , ("No", 'NO_SK_CODEC_ZERO_INITIALIZED, "")
    ]
 )
-}

$( qGenerateSKEnum
    "SKCodecScanlineOrder"
    ''Sk_codec_scanline_order
    [trimming|
        The order in which rows are output from the scanline decoder is not the
        same for all variations of all image types.  This explains the possible
        output row orderings.
    |]
    [
        ( "TopDown"
        , 'TOP_DOWN_SK_CODEC_SCANLINE_ORDER
        , [trimming|
            By far the most common, this indicates that the image can be decoded
            reliably using the scanline decoder, and that rows will be output in
            the logical order.
        |]
        )
    ,
        ( "BottomUp"
        , 'BOTTOM_UP_SK_CODEC_SCANLINE_ORDER
        , [trimming|
            This indicates that the scanline decoder reliably outputs rows, but
            they will be returned in reverse order.  If the scanline format is
            'SKCodecScanlineOrder'BottomUp', the 'Skia.SKCodec.nextScanline' API can be used to determine the actual
            y-coordinate of the next output row, but the client is not forced
            to take advantage of this, given that it's not too tough to keep
            track independently.
            
            For full image decodes, it is safe to get all of the scanlines at
            once, since the decoder will handle inverting the rows as it
            decodes.
            
            For subset decodes and sampling, it is simplest to get and skip
            scanlines one at a time, using the 'Skia.SKCodec.nextScanline' API. It is
            possible to ask for larger chunks at a time, but this should be used
            with caution.  As with full image decodes, the decoder will handle
            inverting the requested rows, but rows will still be delivered
            starting from the bottom of the image.
            
            Upside down BMPs are an example.
        |]
        )
    ]
 )

$( qGenerateSKEnum
    "SKPathVerb"
    ''Sk_path_verb
    "Verb instructs 'SKPath' how to interpret one or more SkPoint and optional conic weight; manage contour, and terminate 'SKPath'."
    [ ("Move", 'MOVE_SK_PATH_VERB, "")
    , ("Line", 'LINE_SK_PATH_VERB, "")
    , ("Quad", 'QUAD_SK_PATH_VERB, "")
    , ("Conic", 'CONIC_SK_PATH_VERB, "")
    , ("Cubic", 'CUBIC_SK_PATH_VERB, "")
    , ("Close", 'CLOSE_SK_PATH_VERB, "")
    , ("Done", 'DONE_SK_PATH_VERB, "")
    ]
 )

$( qGenerateSKEnum
    "SKPathAddMode"
    ''Sk_path_add_mode
    [trimming|
        Dictates how 'Skia.SKPath.addPath' appends. Adding one 'SKPath' to another can extend
        the last contour or start a new contour.
    |]
    [
        ( "Append"
        , 'APPEND_SK_PATH_ADD_MODE
        , "Contours are appended to the destination path as new contours."
        )
    ,
        ( "Extend"
        , 'EXTEND_SK_PATH_ADD_MODE
        , [trimming|
            Extends the last contour of the destination path with the first contour
            of the source path, connecting them with a line.  If the last contour is
            closed, a new empty contour starting at its start point is extended instead.
            If the destination path is empty, the result is the source path.
            The last path of the result is closed only if the last path of the source is.
        |]
        )
    ]
 )

$( qGenerateSKEnum
    "SKPathEffect1DStyle"
    ''Sk_path_effect_1d_style
    ""
    [ ("Translate", 'TRANSLATE_SK_PATH_EFFECT_1D_STYLE, "Translate the shape to each position")
    , ("Rotate", 'ROTATE_SK_PATH_EFFECT_1D_STYLE, "Rotate the shape about its center")
    , ("Morph", 'MORPH_SK_PATH_EFFECT_1D_STYLE, "Transform each point, and turn lines into curves")
    ]
 )

$( qGenerateSKEnum
    "SKPathEffectTrimMode"
    ''Sk_path_effect_trim_mode
    ""
    [ ("Normal", 'NORMAL_SK_PATH_EFFECT_TRIM_MODE, "Return the subset path [start,stop]")
    , ("Inverted", 'INVERTED_SK_PATH_EFFECT_TRIM_MODE, "Return the complement/subset paths [0,start] + [stop,1]")
    ]
 )

$( qGenerateSKEnum
    "SKStrokeCap"
    ''Sk_stroke_cap
    [trimming|
        This enum is relevant in 'Skia.SKPaint'.

        Cap draws at the beginning and end of an open path contour.
    |]
    [ ("Butt", 'BUTT_SK_STROKE_CAP, "No stroke extension")
    , ("Round", 'ROUND_SK_STROKE_CAP, "Adds circle")
    , ("Square", 'SQUARE_SK_STROKE_CAP, "Adds square")
    ]
 )

$( qGenerateSKEnum
    "SKStrokeJoin"
    ''Sk_stroke_join
    [trimming|
        This enum is relevant in 'Skia.SKPaint'.

        Join specifies how corners are drawn when a shape is stroked. Join
        affects the four corners of a stroked rectangle, and the connected segments in a
        stroked path.

        Choose miter join to draw sharp corners. Choose round join to draw a circle with a
        radius equal to the stroke width on top of the corner. Choose bevel join to minimally
        connect the thick strokes.

        The fill path constructed to describe the stroked path respects the join setting but may
        not contain the actual join. For instance, a fill path constructed with round joins does
        not necessarily include circles at each connected segment.
    |]
    [ ("Miter", 'MITER_SK_STROKE_JOIN, "Extends to miter limit")
    , ("Round", 'ROUND_SK_STROKE_JOIN, "Adds circle")
    , ("Bevel", 'BEVEL_SK_STROKE_JOIN, "Connects outside edges")
    ]
 )

$( qGenerateSKEnum
    "SKShaderTileMode"
    ''Sk_shader_tilemode
    ""
    [ ("Clamp", 'CLAMP_SK_SHADER_TILEMODE, "")
    , ("Repeat", 'REPEAT_SK_SHADER_TILEMODE, "")
    , ("Mirror", 'MIRROR_SK_SHADER_TILEMODE, "")
    , ("Decal", 'DECAL_SK_SHADER_TILEMODE, "")
    ]
 )

$( qGenerateSKEnum
    "SKBlurStyle"
    ''Sk_blurstyle
    ""
    [ ("Normal", 'NORMAL_SK_BLUR_STYLE, "")
    , ("Solid", 'SOLID_SK_BLUR_STYLE, "")
    , ("Outer", 'OUTER_SK_BLUR_STYLE, "")
    , ("Inner", 'INNER_SK_BLUR_STYLE, "")
    ]
 )

$( qGenerateSKEnum
    "SKPathDirection"
    ''Sk_path_direction
    ""
    [ ("CW", 'CW_SK_PATH_DIRECTION, "")
    , ("CCW", 'CCW_SK_PATH_DIRECTION, "")
    ]
 )

$( qGenerateSKEnum
    "SKPathArcSize"
    ''Sk_path_arc_size
    ""
    [ ("Small", 'SMALL_SK_PATH_ARC_SIZE, "")
    , ("Large", 'LARGE_SK_PATH_ARC_SIZE, "")
    ]
 )

$( qGenerateSKEnum
    "SKPaintStyle"
    ''Sk_paint_style
    ""
    [ ("Fill", 'FILL_SK_PAINT_STYLE, "")
    , ("Stroke", 'STROKE_SK_PAINT_STYLE, "")
    , ("StrokeAndFill", 'STROKE_AND_FILL_SK_PAINT_STYLE, "")
    ]
 )

$( qGenerateSKEnum
    "SKFontHinting"
    ''Sk_font_hinting
    ""
    [ ("None", 'NONE_SK_FONT_HINTING, "")
    , ("Slight", 'SLIGHT_SK_FONT_HINTING, "")
    , ("Normal", 'NORMAL_SK_FONT_HINTING, "")
    , ("Full", 'FULL_SK_FONT_HINTING, "")
    ]
 )

$( qGenerateSKEnum
    "SKFontEdging"
    ''Sk_font_edging
    ""
    [ ("Alias", 'ALIAS_SK_FONT_EDGING, "")
    , ("Antialias", 'ANTIALIAS_SK_FONT_EDGING, "")
    , ("SubpixelAntialias", 'SUBPIXEL_ANTIALIAS_SK_FONT_EDGING, "")
    ]
 )

$( qGenerateSKEnum
    "GRSurfaceOrigin"
    ''Gr_surfaceorigin
    ""
    [ ("TopLeft", 'TOP_LEFT_GR_SURFACE_ORIGIN, "")
    , ("BottomLeft", 'BOTTOM_LEFT_GR_SURFACE_ORIGIN, "")
    ]
 )

$( qGenerateSKEnum
    "GRBackend"
    ''Gr_backend
    ""
    [ ("OpenGL", 'OPENGL_GR_BACKEND, "")
    , ("Vulkan", 'VULKAN_GR_BACKEND, "")
    , ("Metal", 'METAL_GR_BACKEND, "")
    , ("Direct3D", 'DIRECT3D_GR_BACKEND, "")
    , ("Unsupported", 'UNSUPPORTED_GR_BACKEND, "")
    ]
 )

$( qGenerateSKEnum
    "SKPathOp"
    ''Sk_pathop
    ""
    [ ("Difference", 'DIFFERENCE_SK_PATHOP, "")
    , ("Intersect", 'INTERSECT_SK_PATHOP, "")
    , ("Union", 'UNION_SK_PATHOP, "")
    , ("Xor", 'XOR_SK_PATHOP, "")
    , ("ReverseDifference", 'REVERSE_DIFFERENCE_SK_PATHOP, "")
    ]
 )

$( qGenerateSKEnum
    "SKLatticeRectType"
    ''Sk_lattice_recttype
    ""
    [ ("Default", 'DEFAULT_SK_LATTICE_RECT_TYPE, "")
    , ("Transparent", 'TRANSPARENT_SK_LATTICE_RECT_TYPE, "")
    , ("FixedColor", 'FIXED_COLOR_SK_LATTICE_RECT_TYPE, "")
    ]
 )

$( qGenerateSKEnum
    "SKImageCachingHint"
    ''Sk_image_caching_hint
    ""
    [ ("Allow", 'ALLOW_SK_IMAGE_CACHING_HINT, "")
    , ("Disallow", 'DISALLOW_SK_IMAGE_CACHING_HINT, "")
    ]
 )

$( qGenerateSKEnum
    "SKCodecAnimationDisposalMethod"
    ''Sk_codecanimation_disposalmethod
    ""
    [ ("Keep", 'KEEP_SK_CODEC_ANIMATION_DISPOSAL_METHOD, "")
    , ("RestoreBgColor", 'RESTORE_BG_COLOR_SK_CODEC_ANIMATION_DISPOSAL_METHOD, "")
    , ("RestorePrevious", 'RESTORE_PREVIOUS_SK_CODEC_ANIMATION_DISPOSAL_METHOD, "")
    ]
 )

$( qGenerateSKEnum
    "SKCodecAnimationBlend"
    ''Sk_codecanimation_blend
    ""
    [ ("SrcOver", 'SRC_OVER_SK_CODEC_ANIMATION_BLEND, "")
    , ("Src", 'SRC_SK_CODEC_ANIMATION_BLEND, "")
    ]
 )

$( qGenerateSKEnum
    "SKVerticesVertexMode"
    ''Sk_vertices_vertex_mode
    ""
    [ ("Triangles", 'TRIANGLES_SK_VERTICES_VERTEX_MODE, "")
    , ("TriangleStrip", 'TRIANGLE_STRIP_SK_VERTICES_VERTEX_MODE, "")
    , ("TriangleFan", 'TRIANGLE_FAN_SK_VERTICES_VERTEX_MODE, "")
    ]
 )

$( qGenerateSKEnum
    "SKHighContrastConfigInvertStyle"
    ''Sk_highcontrastconfig_invertstyle
    ""
    [ ("NoInvert", 'NO_INVERT_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE, "")
    , ("InvertBrightness", 'INVERT_BRIGHTNESS_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE, "")
    , ("InvertLightness", 'INVERT_LIGHTNESS_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE, "")
    ]
 )

$( qGenerateSKEnum
    "SKJpegEncoderDownsample"
    ''Sk_jpegencoder_downsample
    ""
    [ ("Downsample420", 'DOWNSAMPLE_420_SK_JPEGENCODER_DOWNSAMPLE, "")
    , ("Downsample422", 'DOWNSAMPLE_422_SK_JPEGENCODER_DOWNSAMPLE, "")
    , ("Downsample444", 'DOWNSAMPLE_444_SK_JPEGENCODER_DOWNSAMPLE, "")
    ]
 )

$( qGenerateSKEnum
    "SKJpegEncoderAlphaOption"
    ''Sk_jpegencoder_alphaoption
    ""
    [ ("Ignore", 'IGNORE_SK_JPEGENCODER_ALPHA_OPTION, "")
    , ("BlendOnBlack", 'BLEND_ON_BLACK_SK_JPEGENCODER_ALPHA_OPTION, "")
    ]
 )

$( qGenerateSKEnum
    "SKWebpEncoderCompression"
    ''Sk_webpencoder_compression
    ""
    [ ("Lossy", 'LOSSY_SK_WEBPENCODER_COMPTRESSION, "")
    , ("Lossless", 'LOSSLESS_SK_WEBPENCODER_COMPTRESSION, "")
    ]
 )

$( qGenerateSKEnum
    "SKRoundRectType"
    ''Sk_rrect_type
    ""
    [ ("Empty", 'EMPTY_SK_RRECT_TYPE, "")
    , ("Rect", 'RECT_SK_RRECT_TYPE, "")
    , ("Oval", 'OVAL_SK_RRECT_TYPE, "")
    , ("Simple", 'SIMPLE_SK_RRECT_TYPE, "")
    , ("NinePatch", 'NINE_PATCH_SK_RRECT_TYPE, "")
    , ("Complex", 'COMPLEX_SK_RRECT_TYPE, "")
    ]
 )

$( qGenerateSKEnum
    "SKRoundRectCorner"
    ''Sk_rrect_corner
    ""
    [ ("UpperLeft", 'UPPER_LEFT_SK_RRECT_CORNER, "")
    , ("UpperRight", 'UPPER_RIGHT_SK_RRECT_CORNER, "")
    , ("LowerRight", 'LOWER_RIGHT_SK_RRECT_CORNER, "")
    , ("LowerLeft", 'LOWER_LEFT_SK_RRECT_CORNER, "")
    ]
 )

$( qGenerateSKEnum
    "SKRuntimeEffectUniformType"
    ''Sk_runtimeeffect_uniform_type
    ""
    [ ("Float", 'FLOAT_SK_RUNTIMEEFFECT_UNIFORM_TYPE, "")
    , ("Float2", 'FLOAT2_SK_RUNTIMEEFFECT_UNIFORM_TYPE, "")
    , ("Float3", 'FLOAT3_SK_RUNTIMEEFFECT_UNIFORM_TYPE, "")
    , ("Float4", 'FLOAT4_SK_RUNTIMEEFFECT_UNIFORM_TYPE, "")
    , ("Float2x2", 'FLOAT2X2_SK_RUNTIMEEFFECT_UNIFORM_TYPE, "")
    , ("Float3x3", 'FLOAT3X3_SK_RUNTIMEEFFECT_UNIFORM_TYPE, "")
    , ("Float4x4", 'FLOAT4X4_SK_RUNTIMEEFFECT_UNIFORM_TYPE, "")
    , ("Int", 'INT_SK_RUNTIMEEFFECT_UNIFORM_TYPE, "")
    , ("Int2", 'INT2_SK_RUNTIMEEFFECT_UNIFORM_TYPE, "")
    , ("Int3", 'INT3_SK_RUNTIMEEFFECT_UNIFORM_TYPE, "")
    , ("Int4", 'INT4_SK_RUNTIMEEFFECT_UNIFORM_TYPE, "")
    ]
 )

$( qGenerateSKEnum
    "SKRuntimeEffectChildType"
    ''Sk_runtimeeffect_child_type
    ""
    [ ("Shader", 'SHADER_SK_RUNTIMEEFFECT_CHILD_TYPE, "")
    , ("ColorFilter", 'COLOR_FILTER_SK_RUNTIMEEFFECT_CHILD_TYPE, "")
    , ("Blender", 'BLENDER_SK_RUNTIMEEFFECT_CHILD_TYPE, "")
    ]
 )

$( qGenerateSKEnum
    "SKFilterMode"
    ''Sk_filter_mode
    ""
    [ ("Nearest", 'NEAREST_SK_FILTER_MODE, "")
    , ("Linear", 'LINEAR_SK_FILTER_MODE, "")
    ]
 )

$( qGenerateSKEnum
    "SKMipmapMode"
    ''Sk_mipmap_mode
    ""
    [ ("None", 'NONE_SK_MIPMAP_MODE, "")
    , ("Nearest", 'NEAREST_SK_MIPMAP_MODE, "")
    , ("Linear", 'LINEAR_SK_MIPMAP_MODE, "")
    ]
 )
