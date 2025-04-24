from __future__ import annotations
from typing import *
from pathlib import Path
from dataclasses import dataclass
from enum import Enum
import argparse
from pycparser import c_ast, c_generator
import gzip
import pycparser
import io
import sys
import tempfile
import wordninja
import itertools
import logging
import subprocess
import shlex
import re

logging.basicConfig(level=logging.DEBUG, stream=sys.stdout)
logger = logging.getLogger(__name__)


def check_python_version() -> None:
    ver = sys.version_info
    if not (3, 11) <= (ver.major, ver.minor):
        raise Exception(
            f"Python's version should be at least 3.11. Got {ver}")


check_python_version()

BORING_CLIB_TYPEDEF_NAMES = set("""
__u_char
__u_short
__u_int
__u_long
__int8_t
__uint8_t
__int16_t
__uint16_t
__int32_t
__uint32_t
__int64_t
__uint64_t
__int_least8_t
__uint_least8_t
__int_least16_t
__uint_least16_t
__int_least32_t
__uint_least32_t
__int_least64_t
__uint_least64_t
__quad_t
__u_quad_t
__intmax_t
__uintmax_t
__dev_t
__uid_t
__gid_t
__ino_t
__ino64_t
__mode_t
__nlink_t
__off_t
__off64_t
__pid_t
__fsid_t
__clock_t
__rlim_t
__rlim64_t
__id_t
__time_t
__useconds_t
__suseconds_t
__suseconds64_t
__daddr_t
__key_t
__clockid_t
__timer_t
__blksize_t
__blkcnt_t
__blkcnt64_t
__fsblkcnt_t
__fsblkcnt64_t
__fsfilcnt_t
__fsfilcnt64_t
__fsword_t
__ssize_t
__syscall_slong_t
__syscall_ulong_t
__loff_t
__caddr_t
__intptr_t
__socklen_t
__sig_atomic_t
int8_t
int16_t
int32_t
int64_t
uint8_t
uint16_t
uint32_t
uint64_t
int_least8_t
int_least16_t
int_least32_t
int_least64_t
uint_least8_t
uint_least16_t
uint_least32_t
uint_least64_t
int_fast8_t
int_fast16_t
int_fast32_t
int_fast64_t
uint_fast8_t
uint_fast16_t
uint_fast32_t
uint_fast64_t
intptr_t
uintptr_t
intmax_t
uintmax_t
ptrdiff_t
size_t
wchar_t
max_align_t
""".split())


HASKELL_KEYWORDS = {
    "instance",
    "data",
    "deriving",
    "class",
    "type",
}


# Some SkiaSharp C-API functions have struct type arguments and Haskell's FFI is
# incapable of interacting with these functions directly.
#
# We have to manually define FFI bindings for them instead of using the
# auto-generator.
MONO_SKIA_FUNCTIONS_BLOCKLIST = {
    "gr_direct_context_make_direct3d",
    "gr_direct_context_make_direct3d_with_options",
    "gr_direct_context_make_vulkan",
    "gr_direct_context_make_vulkan_with_options",
    "sk_canvas_clear_color4f",
    "sk_canvas_draw_color4f",
}


def to_c_source(node: c_ast.Node) -> str:
    """
    Converts a c_ast.Node to C source.
    """
    g = c_generator.CGenerator()
    src = g.visit(node)
    return src


def upper_head(text: str) -> str:
    """
    Make the first character of the input text uppercase.
    """
    return text[0].upper() + text[1:]


def lower_head(text: str) -> str:
    """
    Make the first character of the input text lowercase.
    """
    return text[0].lower() + text[1:]


def make_camel_case(words: list[str], *, capital: bool = False) -> str:
    """
    Converts inputs like ["sk", "runtime", "effect", "uniform", "flags"] to
    - "SkRuntimeEffectUniformFlags" (if capital is True)
    - "skRuntimeEffectUniformFlags" (if capital is False)
    """
    if capital:
        return "".join([upper_head(word) for word in words])
    else:
        return words[0] + "".join([upper_head(word) for word in words[1:]])


class WordBreaker:
    def __init__(self):
        # Default wordninja (and even LLMs like Gemini 2.5 Pro) break words
        # incorrectly, e.g.:
        # - webpencoder -> web/p/encoder (We want web/encoder)
        # - skottie -> s/kot/tie (We want skottie)
        # - font -> font/m/gr (We want font/mgr)

        # This is likely because wordninja's default word list is derived from
        # contexts in normal English instead of Skia/Chromium/graphics/general
        # programming.

        # So special words are injected and priotized above all else.
        special_words = [
            # other terms
            "cliperator",
            "spanerator",
            "pixmap",
            "bbh",
            "shader",
            "sksg",
            "skottie",
            "rsx",
            "webp",
            "codec",
            "icc",
            "svg",
            "vk",
            "gl",
            "utf",
            "flattenable"
            "allocator",
            "allocation",
            "allocate",
            "alloc",
            "premul",
            "unpremul",
            "src",
            "dst",
            # image formats
            "bmp",
            "gif",
            "ico",
            "jpeg",
            "png",
            "wbmp",
            "webp",
            "pkm",
            "ktx",
            "astc",
            "dng",
            "heif",
            "avif",
            "jpegxl",
            # colors
            "r8",
            "g8",
            "b8",
            "a8",
            "r16",
            "g16",
            "b16",
            "a16",
            "r32",
            "g32",
            "b32",
            "a32",
            "bgr",
            "rgb",
            "rgba",
            "bgra",
            "argb",
            "srgb",
            "srgba",
        ]
        words = []
        words.extend(special_words)
        with gzip.open("./wordninja_words.txt.gz") as f:
            words.extend(f.read().decode().split())
        self._model = wordninja.LanguageModel(words)

        # Special breaks to further refine word-breaking algorithm.
        special_breaks = {
            "skottie": "skottie",
            "skresources": "sk/resources",
            "dxgi": "dxgi",
            "d3d": "d3d",
            "nway": "n/way",
        }

        self._special_breaks = {
            key: value.split("/")
            for key, value
            in special_breaks.items()
        }

    def break_snake_case(self, text: str) -> list[str]:
        """
        Break mangled lowercase snake case inputs like "skresources_multi_frame_image_asset"
        into ["sk", "resources", "multi", "frame", "image", "asset"].

        This function is aware of Skia library specific words like "skottie" and
        would not break them.
        """
        words = []
        for w in text.split("_"):
            words.extend(self.break_word(w))
        return words

    def break_screaming_snake_case(self, text: str) -> list[str]:
        return self.break_snake_case(text.lower())

    def break_word(self, word: str) -> list[str]:
        if len(word) <= 2:
            # Immediately forgive words like "sk", "gl", "rx", "ry", "rz", ...
            return [word]
        if word in self._special_breaks:
            return self._special_breaks[word]
        else:
            return self._model.split(word)


class TestWordBreaker:
    def test_breaker(self):
        breaker = WordBreaker()

        def test(input_text: str, answer_text: str):
            assert breaker.break_word(input_text) == answer_text.split("/")

        def test_snake(input_text: str, answer_text: str):
            assert breaker.break_snake_case(
                input_text) == answer_text.split("/")

        test("skottie", "skottie")
        test("codecanimation", "codec/animation")
        test("webpencoder", "webp/encoder")
        test("jpegencoder", "jpeg/encoder")
        test("runtimeeffect", "runtime/effect")
        test("dynamicstreamwriter", "dynamic/stream/writer")
        test("rawiterator", "raw/iterator")
        test("wstream", "w/stream")
        test("streamrewindable", "stream/rewindable")
        test("dxgi", "dxgi")
        test("nway", "n/way")
        test("overdraw", "overdraw")
        test("glinterface", "gl/interface")
        test("vkinterface", "vk/interface")
        test_snake("gr_vk_backendmemory", "gr/vk/backend/memory")
        test_snake("svgcanvas", "svg/canvas")


class NameTranslator:
    def __init__(self, breaker: WordBreaker):
        self._breaker = breaker

    def c_type_to_hs(
        self,
        name: str, *,
        return_words: bool = False,
        has_t_suffix: bool = True,
    ) -> str | list[str]:
        """
        Converts a C type name to Haskell.

        e.g., sk_runtimeeffect_uniform_flags_t -> SkRuntimeEffectUniformFlags
        """

        if has_t_suffix:
            if not name.endswith("_t"):
                raise ValueError(
                    f"{name=} does not have suffix '_t'. You sure this is a C type name?")
            name = name[:-2]  # Strip '_t'

        words = self._breaker.break_screaming_snake_case(name)
        if return_words:
            return words
        else:
            return make_camel_case(words, capital=True)

    def c_normal_enum_value_to_hs(self, name: str, ty_name: str) -> str:
        """
        Converts a C normal enum value name to Haskell.

        e.g., HUE_SK_BLENDMODE (ty_name="sk_blendmode_t") -> SkBlendMode'Hue
        """
        name_words = self._breaker.break_screaming_snake_case(name)

        # Process name words
        if ty_name.endswith("sk_encoded_image_format_t"):
            # sk_encoded_image_format_t is an exception
            # its values' names's suffixes are mistyped(?).
            name_words = name_words[:-3]  # r-strip "SK_ENCODED_FORMAT"
        else:
            # r-strip type-name suffixed value names
            ty_words = self.c_type_to_hs(ty_name, return_words=True)
            if name_words[-len(ty_words):] == ty_words:
                name_words = name_words[:-len(ty_words)]

        return lower_head(self.c_type_to_hs(ty_name)) + "'" + make_camel_case(name_words, capital=True)

    def c_fn_name_to_hs(self, name: str) -> str:
        """
        Converts a C function name to Haskell

        e.g., sk_picture_get_recording_canvas -> skPictureGetRecordingCanvas
        """
        words = self._breaker.break_snake_case(name)
        return make_camel_case(words)

    def c_struct_field_to_hs(self, name: str) -> str:
        """
        Converts a C struct field name to Haskell.

        e.g., fGlyphCacheTextureMaximumBytes -> glyphCacheTextureMaximumBytes
        e.g., fICCProfileDescription -> iccProfileDescription
        e.g., fInstance -> instance_ (underscore is added to prevent Haskell syntax errors)
        """

        def make_field_name():
            def lower_head_word(word):
                # ICCProfileDescription -> iccProfileDescription
                # AlphaOption -> alphaOption

                if word[0].islower():
                    # got a word like "xmpMetadata"
                    # no need for processing
                    return word
                elif word.isupper():
                    # got a word that is entirely uppercase
                    # like "PDFA" or "R"/"G"/"B"
                    return word.lower()
                elif word[1].islower():
                    # word is like "AlphaOption"
                    return lower_head(word)
                else:
                    # word is special, like "ICCProfileDescription"
                    # we need to turn it into "iccProfileDescription"

                    for i in range(len(word)):
                        if word[i].islower():
                            break

                    # 'i' is now the index of the first lowercase letter

                    return word[:(i-1)].lower() + word[(i-1):]

            if name[0] == "f" and name[1].isupper():
                return lower_head_word(name[1:])
            elif name.startswith("_private_"):
                # "_private_fUsesSystemHeap" of "gr_vk_alloc_t" is an exception
                return lower_head_word(name[len("_private_"):])
            else:
                # handle other field names like "xmpMetadata"
                return lower_head_word(name)

        field_name = make_field_name()
        # NOTE: field_name might be illegal; e.g.; "instance", which is a
        # Haskell keyword. We need to take care of them.

        if field_name in HASKELL_KEYWORDS:
            return field_name + "_"  # Pad an underscore
        else:
            return field_name


class TypeContext:
    def __init__(self, translator: NameTranslator):
        self._translator = translator

        primitive_types = {
            # "bool"s are defined as "_Bool" apparently? see "bool fAvoidStencilBuffers" of "gr_context_options_t"
            "_Bool": "CBool",
            # This is cheating but should be ok.
            "void": "()",
            "double": "CDouble",
            "float": "CFloat",
            "int": "CInt",
            "long": "CLong",
            "unsigned/int": "CUInt",
            "long/long": "CLLong",
            "size_t": "CSize",
            "char": "CChar",
            "intptr_t": "CIntPtr",
            "int8_t": "Int8",
            "int16_t": "Int16",
            "int32_t": "Int32",
            "int64_t": "Int64",
            "uint8_t": "Word8",
            "uint16_t": "Word16",
            "uint32_t": "Word32",
            "uint64_t": "Word64",
        }

        self._primitive_types = {
            tuple(key.split("/")): value
            for key, value
            in primitive_types.items()
        }

        self._opaque_struct_types = set()
        self._types = set()
        self._type_aliases = {}  # c name -> aliased hs name
        self._func_types = {}

    def add_type(self, c_ty_name: str) -> None:
        self._types.add(c_ty_name)
        logger.info(f"type context registered other type: {c_ty_name}")

    def add_func_type(self, c_ty_name: str, hs_ty: str) -> None:
        self._func_types[c_ty_name] = hs_ty
        logger.info(
            f"type context registered func type: {c_ty_name} ~~~ {hs_ty}")

    def add_type_alias(self, c_name: str, hs_ty: str) -> None:
        self._type_aliases[c_name] = hs_ty
        logger.info(
            f"type context registered type alias: {c_name} ~~~ {hs_ty}")

    def _wrap_ptrs(self, ptr_levels: int, hs_ty: str) -> str:
        for i in range(ptr_levels):
            hs_ty = f"Ptr ({hs_ty})"
        return hs_ty

    def unwrap_c_type(self, in_ty: c_ast.Node) -> (int, list[str]):
        """
        Returns (number of pointer levels/arrays, base type keywords + the name itself
        (e.g., ["int"], ["unsigned", "int"]))
        """
        # Unwrap * types
        ptr_level = 0
        base_ty: c_ast.node.Node = in_ty
        while True:
            if isinstance(base_ty, c_ast.PtrDecl):
                base_ty = base_ty.type
                ptr_level += 1
            elif isinstance(base_ty, c_ast.ArrayDecl):
                base_ty = base_ty.type
                ptr_level += 1
            elif isinstance(base_ty, (c_ast.TypeDecl, c_ast.Typename)):
                # FIXME: For some reason pycparser (excessively?) wraps things
                # in TypeDecl/Typename, I don't know what it does. I am just going to
                # unwrap them anyway.
                base_ty = base_ty.type
            else:
                break

        if not isinstance(base_ty, c_ast.IdentifierType):
            raise ValueError(f"encountered weird type: {in_ty}")

        return ptr_level, base_ty.names

    def parse_c_type(self, in_ty: c_ast.Node) -> str:
        ptr_level, base_ty_names = self.unwrap_c_type(in_ty)

        if tuple(base_ty_names) in self._primitive_types:
            # Handle primitive types like "float", "double", "int", "unsigned int", "bool", ...
            hs_name = self._primitive_types[tuple(base_ty_names)]
            return self._wrap_ptrs(ptr_level, hs_name)
        else:
            if len(base_ty_names) != 1:
                raise ValueError(
                    f"encountered weird identifier type: {in_ty}")

            c_name = base_ty_names[0]
            if c_name in self._type_aliases:
                hs_name = self._translator.c_type_to_hs(c_name)
                return self._wrap_ptrs(ptr_level, hs_name)
            if c_name in self._types:
                hs_name = self._translator.c_type_to_hs(c_name)
                return self._wrap_ptrs(ptr_level, hs_name)
            elif c_name in self._func_types:
                hs_name = self._translator.c_type_to_hs(
                    c_name, has_t_suffix=False)

                if ptr_level != 0:
                    # TODO: This is probably not a weird, but I am going to put
                    # a kill-switch here in case the parser hits this part for
                    # some reason.
                    raise ValueError(f"encountered type: {base_ty}.")

                # NOTE: ptr_level == 0
                fun_ty = hs_name
                return f"FunPtr ({fun_ty})"
            elif c_name in self._opaque_struct_types:
                if ptr_level == 0:
                    raise ValueError(
                        f"encountered weird identifier type: {base_ty}. Specifically, the type is an opaque struct and must be wrapped in pointers, but it isn't.")

                hs_name = self._translator.c_opaque_struct_type_to_hs(
                    c_name)
                return self._wrap_ptrs(ptr_level - 1, hs_name)
            else:
                raise ValueError(f"unknown identifier type: {c_name}")

    def parse_fn_type(self, ty: c_ast.FuncDecl) -> str:
        hs_params: list[str] = []
        for param in ty.args.params:
            # NOTE: Special case: if the function resembles `my_func(void)`, the
            # function in fact takes no argument.  It is really inconvenient to
            # deal with...
            #
            # This tests if param is "void". If so, stop looking at the
            # remaining args. This can handle functions that are typed like
            # `my_func(void)`.
            if self.unwrap_c_type(param.type) == (0, ["void"]):
                break

            hs_ty = self.parse_c_type(param.type)
            hs_params.append(hs_ty)

        hs_ret_ty = self.parse_c_type(ty.type)

        # Form "(hs_params[0]) -> (hs_params[1]) -> IO (hs_ret_ty)"
        txts = []
        for hs_param in hs_params:
            txts.append(f"({hs_param})")
        txts.append(f"IO ({hs_ret_ty})")

        hs_func_ty = " -> ".join(txts)
        return hs_func_ty


class MonoSkiaVisitor(c_ast.NodeVisitor):
    def __init__(self, output: io.StringIO) -> None:
        self._output = output

        word_breaker = WordBreaker()
        self._translator = NameTranslator(word_breaker)

        self._tyctx = TypeContext(translator=self._translator)

    def write_line(self, line: str) -> None:
        self._output.write(f"{line}\n")
        logger.info(f"... emitted: {line}")

    def visit_FuncDecl(self, decl: c_ast.FuncDecl) -> None:
        def _extract_fn_name():
            # FIXME: c_parser's FuncDecl can bury the function's name under
            # PtrDecl if the return type is a pointer.
            #
            # This is a workaround to extract the function's name.
            node = decl.type
            while True:
                if isinstance(node, c_ast.TypeDecl):
                    return node.declname
                elif isinstance(node, c_ast.ArrayDecl):
                    node = node.type
                elif isinstance(node, c_ast.PtrDecl):
                    node = node.type
                else:
                    raise ValueError(f"weird FuncDecl: {decl}")

        c_fn_name = _extract_fn_name()
        hs_fn_name = self._translator.c_fn_name_to_hs(c_fn_name)

        logger.info(
            f"processing top-level function declaration: {c_fn_name} -> {hs_fn_name}")

        if c_fn_name in MONO_SKIA_FUNCTIONS_BLOCKLIST:
            logger.info(
                f"skipped function '{c_fn_name}' because it is on the blocklist")
            return

        hs_fn_ty = self._tyctx.parse_fn_type(decl)

        self.write_line(f"-- | `{c_fn_name}`")
        self.write_line(
            f"foreign import ccall \"{c_fn_name}\" {hs_fn_name} :: {hs_fn_ty}")

        # logger.warn(f"{node.type.declname}")
        # raise ValueError()

    def visit_Typedef(self, node: c_ast.Typedef) -> None:
        if node.name in BORING_CLIB_TYPEDEF_NAMES:
            return

        if isinstance(node.type.type, c_ast.IdentifierType):
            ty: c_ast.Typename = node.type.type
            self.on_type_alias(node, ty)
        elif isinstance(node.type.type, c_ast.Struct):
            struct_ty: c_ast.Struct = node.type.type
            if struct_ty.decls is None:
                self.on_opaque_struct(node, struct_ty)
            else:
                self.on_normal_struct(node, struct_ty)
        elif isinstance(node.type.type, c_ast.Enum):
            enum_ty: c_ast.Enum = node.type.type
            self.on_normal_enum(node, enum_ty)
        elif isinstance(node.type.type, c_ast.FuncDecl):
            func_ty: c_ast.FuncDecl = node.type.type
            self.on_func_type_decl(node, func_ty)
        else:
            raise ValueError(f"Unhandled node", node.name,
                             type(node.type.type))

    def on_type_alias(self, root: c_ast.Node, c_target_ty: c_ast.Typename):
        logger.info(f"processing identifier type {root.name}")

        c_ty_name = root.name
        hs_ty_name = self._translator.c_type_to_hs(c_ty_name)

        hs_target_ty = self._tyctx.parse_c_type(c_target_ty)
        self._tyctx.add_type_alias(c_ty_name, hs_target_ty)

        self.write_line(f"-- | `{c_ty_name}`")
        self.write_line(f"type {hs_ty_name} = {hs_target_ty}")

    def on_func_type_decl(self, root: c_ast.Node, ty: c_ast.FuncDecl):
        c_ty_name = root.name
        hs_ty_name = self._translator.c_type_to_hs(
            c_ty_name, has_t_suffix=False)
        logger.info(
            f"processing function pointer: {c_ty_name} -> {hs_ty_name}")

        hs_func_ty = self._tyctx.parse_fn_type(ty)
        self._tyctx.add_func_type(c_ty_name, hs_func_ty)

        # Emit
        self.write_line(f"-- | `{c_ty_name}`")
        self.write_line(f"type {hs_ty_name} = {hs_func_ty}")

    def on_opaque_struct(self, root: c_ast.Node, struct_ty: c_ast.Struct):
        c_ty_name = root.name
        hs_ty_name = self._translator.c_type_to_hs(c_ty_name)
        logger.info(f"processing opaque struct: {c_ty_name}")

        # FIXME: For some reason mono-skia has duplicate opaque struct definitions
        if c_ty_name in self._tyctx._types:
            return

        self._tyctx.add_type(c_ty_name)

        self.write_line(f"-- | `{c_ty_name}`")
        self.write_line(f"data {hs_ty_name} = {hs_ty_name}")
        self.write_line(f"  deriving (Show, Eq, Ord)")

    def on_normal_struct(self, root: c_ast.Node, struct_ty: c_ast.Struct):
        c_ty_name = root.name
        hs_ty_name = self._translator.c_type_to_hs(c_ty_name)
        logger.info(f"processing normal struct: {c_ty_name} -> {hs_ty_name}")

        self._tyctx.add_type(c_ty_name)

        lines_fields = []
        lines_offsets = []
        lines_peek = []
        lines_poke = []

        for i, decl in enumerate(struct_ty.decls):
            decl: c_ast.Decl

            c_field_name = decl.name
            hs_field_name = self._translator.c_struct_field_to_hs(c_field_name)

            logger.info(
                f"processing struct field value: {c_field_name} -> {hs_field_name}")

            c_field_ty = decl.type
            hs_field_ty = self._tyctx.parse_c_type(c_field_ty)

            # For "Write data definition"
            prefixer = "{" if i == 0 else ","
            lines_fields.append(
                f"  {prefixer} {hs_field_name} :: {hs_field_ty}")
            lines_fields.append(f"  -- ^ `{c_field_name}`")

            # For "Write Offset instances"
            lines_offsets.append(
                f"instance Offset \"{hs_field_name}\" {hs_ty_name} where rawOffset = (#offset {c_ty_name}, {c_field_name})")

            # For "Write the Storable instance"
            lines_peek.append(
                f"    {hs_field_name} <- (#peek {c_ty_name}, {c_field_name}) in'ptr")
            lines_poke.append(
                f"    (#poke {c_ty_name}, {c_field_name}) in'ptr in'value.{hs_field_name}")

        # Write data definition
        self.write_line(f"-- | `{root.name}`")
        self.write_line(f"data {hs_ty_name} = {hs_ty_name}")
        for line in lines_fields:
            self.write_line(line)
        self.write_line(f"  }} deriving (Show, Eq, Ord)")

        # Write Offset instances
        for line in lines_offsets:
            self.write_line(line)

        # Write the Storable instance
        self.write_line(f"instance Storable {hs_ty_name} where")

        self.write_line(f"  sizeOf _ = (#size {c_ty_name})")

        self.write_line(f"  alignment _ = (#alignment {c_ty_name})")

        self.write_line(f"  peek in'ptr = do")
        for line in lines_peek:
            self.write_line(line)
        self.write_line(f"    pure {hs_ty_name}{{..}}")

        self.write_line(f"  poke in'ptr in'value = do")
        for line in lines_poke:
            self.write_line(line)

    def on_normal_enum(self, root: c_ast.Node, enum_ty: c_ast.Enum):
        c_ty_name = root.name
        hs_ty_name = self._translator.c_type_to_hs(c_ty_name)
        logger.info(f"processing normal enum: {c_ty_name}")

        self._tyctx.add_type(c_ty_name)

        self.write_line(f"-- | `{c_ty_name}`")
        self.write_line(
            f"newtype {hs_ty_name} = {hs_ty_name} (#type {c_ty_name})")
        self.write_line(
            f"  deriving (Show, Eq, Ord) deriving newtype (Num, Bits, Storable)")

        # Convert to list so I can use len() conveniently
        enum_ty_values = list(enum_ty.values)
        for i, value in enumerate(enum_ty_values):
            c_value_name = value.name
            hs_value_name = self._translator.c_normal_enum_value_to_hs(
                c_value_name, c_ty_name)
            logger.info(
                f"processing enum value: {c_value_name} -> {hs_value_name}")

            # NOTE: I am not using hsc2hs's #enum here so I can add comment to each
            # enum value.
            self.write_line(f"-- | `{c_value_name}`")
            self.write_line(f"{hs_value_name} :: {hs_ty_name}")
            self.write_line(f"{hs_value_name} = #const {c_value_name}")


def parse_mono_skia_headers(mono_skia_include_dir: Path) -> c_ast.node:
    """
    Reads all SkiaSharp's C-API headers and returns a giant c_parser AST that
    has all the SkiaSharp types and functions.
    """

    # NOTE: Why #define __attribute__? See
    # https://github.com/eliben/pycparser/wiki/FAQ#what-do-i-do-about-__attribute__

    src = "#define __attribute__(x)\n\n\n"

    for header_path in (mono_skia_include_dir / "c").glob("*.h"):
        src += f"#include <c/{header_path.name}>\n"

    with tempfile.NamedTemporaryFile(delete_on_close=False) as tmpfile:
        tmpfile.write(src.encode())
        tmpfile.close()

        ast = pycparser.parse_file(
            tmpfile.name,
            use_cpp=True,
            cpp_args=f"-I{shlex.quote(str(mono_skia_include_dir))}"
        )

    return ast


def run(
    *,
    mono_skia_include_dir: Path | None,
    module_name: str,
    output_path: Path,
    dst_hsc_file: Path
) -> None:
    if mono_skia_include_dir is None:
        # TODO: Handle wacky (but improbably) situations, e.g., bad outputs
        output = subprocess.check_output(
            ["pkg-config", "--cflags-only-I", "skia"], shell=False, encoding="utf-8")
        # [2:] to cut off the "-I" in "-I/my/path/to/skia"
        mono_skia_include_dir = Path(output[2:].strip())

        logger.info(
            f"Inferred mono-skia include directory through pkg-config: {mono_skia_include_dir}")

    with output_path.open("w") as f:
        f.write(f"""\
module {module_name} where

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Storable
import Foreign.Storable.Offset

#include "c/sk_types.h"

""")
        v = MonoSkiaVisitor(output=f)
        v.visit(parse_mono_skia_headers(mono_skia_include_dir))
    logger.info(f"Written generated bindings to {output_path}")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--mono-skia-include-dir",
                        type=Path, default=None, required=False)
    parser.add_argument("-m", "--module-name", type=str,
                        default="MonoSkiaBindings", required=False)
    parser.add_argument("-o", "--output-path", type=Path,
                        default=Path.cwd() / "Bindings.hsc", required=False)
    parser.add_argument("dst_hsc_file", type=Path,
                        default="Bindings.hsc", nargs="?")
    args = parser.parse_args()
    run(**args.__dict__)


if __name__ == "__main__":
    main()
