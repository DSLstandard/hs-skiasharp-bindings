from __future__ import annotations

from .misc import (
    BORING_CLIB_TYPEDEF_NAMES,
    get_skia_ast,
    render_ast,
    capitalize_head
)
from dataclasses import dataclass
from pathlib import Path
from pycparser import c_ast
from typing import *
import io
import logging
import pycparser
import shlex
import subprocess
import sys
import tempfile


logger = logging.getLogger(__name__)


def remove_suffix(string: str, suffix: str) -> str:
    if not string.endswith(suffix):
        raise ValueError(
            f"String {repr(string)} does not have suffix {repr(suffix)}")
    return string[:-len(suffix)]


type HaskellType = str


@dataclass
class UnwrappedCType:
    """
    """

    ptr_levels: int
    words: list[str]


def unwrap_c_type(in_ty: c_ast.Node) -> UnwrappedCType:
    """
    Converts a C type represented by a 'c_ast.Node' into an 'UnwrappedCType' and returns it.

    See the following examples to see what the function returns:
    - `unsigned int***` -> UnwrappedCType(ptr_levels=3, words=["unsigned", "int"])
    - `float[]` -> UnwrappedCType(ptr_levels=1, words=["float"])
    - `float*[]` -> UnwrappedCType(ptr_levels=2, words=["float"])
    - `bool` -> UnwrappedCType(ptr_levels=0, words=["bool"])
    """
    # Unwrap * types
    ptr_levels = 0
    base_ty: c_ast.node.Node = in_ty
    while True:
        if isinstance(base_ty, c_ast.PtrDecl):
            base_ty = base_ty.type
            ptr_levels += 1
        elif isinstance(base_ty, c_ast.ArrayDecl):
            base_ty = base_ty.type
            ptr_levels += 1
        elif isinstance(base_ty, (c_ast.TypeDecl, c_ast.Typename)):
            # FIXME: For some reason pycparser (excessively?) wraps things
            # in TypeDecl/Typename, I don't know what it does. I am just going to
            # unwrap them anyway.
            base_ty = base_ty.type
        else:
            break

    if not isinstance(base_ty, c_ast.IdentifierType):
        raise ValueError(f"encountered weird type: {in_ty}")

    return UnwrappedCType(ptr_levels=ptr_levels, words=base_ty.names)


@dataclass
class UnwrappedCFuncArg:
    name: str | None  # Name of the argument. 'None' if there is no given name.
    arg_type: c_ast.Node


@dataclass
class UnwrappedCFuncDecl:
    parameter: list[UnwrappedCFuncArg]
    # The return type of the function. 'None' if the return type is 'void'.
    return_type: c_ast.Node | None


def unwrap_c_func_decl(ty: c_ast.FuncDecl) -> UnwrappedCFuncDecl:
    parameter = []
    for param in ty.args.params:
        # NOTE: Special case: if the function resembles `my_func(void)`, the
        # function in fact takes no argument.  It is really inconvenient to
        # deal with...
        #
        # This tests if param is "void". If so, stop looking at the
        # remaining args. This can handle functions that are typed like
        # `my_func(void)`.
        if unwrap_c_type(param.type) == UnwrappedCType(ptr_levels=0, words=["void"]):
            break

        parameter.append(UnwrappedCFuncArg(
            name=param.name, arg_type=param.type))

    return UnwrappedCFuncDecl(
        parameter=parameter,
        return_type=ty.type,
    )


def wrap_hs_ptr(hs_ty: HaskellType, ptr_levels: int) -> HaskellType:
    """
    Wraps the Haskell "Ptr (...)" type around the input Haskell type for multiple times.
    """
    for i in range(ptr_levels):
        hs_ty = f"Ptr ({hs_ty})"
    return hs_ty


class TypeContext:
    def __init__(self):
        self._c_def_name_to_hs_type: Dict[str, HaskellType] = {}

        self._c_primitive_type_to_hs_type: Dict[Tuple[str], HaskellType] = {}
        self._initialize_c_primitive_type_to_hs_type()

    def register_type(self, c_def_name: str, hs_type: HaskellType) -> None:
        self._c_def_name_to_hs_type[c_def_name] = hs_type

    def _initialize_c_primitive_type_to_hs_type(self):
        """
        Initializes 'self._c_primitive_type_to_hs_type' to be Dict mapping C types to Haskell types
        like so:

        {
            ("unsigned", "int"): "CUInt",
            ("int",): "CInt",
            ("size_t",): "CSize",
            ...
        }
        """

        entries = {
            "_Bool": "CBool",
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
            "void": "()",
        }

        self._c_primitive_type_to_hs_type = {
            tuple(key.split("/")): value
            for key, value in entries.items()
        }

    def convert_c_type_to_hs_type(self, c_type: c_ast.Node) -> HaskellType:
        t = unwrap_c_type(c_type)

        if tuple(t.words) in self._c_primitive_type_to_hs_type:
            hs_type = self._c_primitive_type_to_hs_type[tuple(t.words)]
            hs_type = wrap_hs_ptr(hs_type, t.ptr_levels)
            return hs_type

        if len(t.words) == 1:
            w = t.words[0]
            if w in self._c_def_name_to_hs_type:
                hs_type = self._c_def_name_to_hs_type[w]
                hs_type = wrap_hs_ptr(hs_type, t.ptr_levels)
                return hs_type

        raise ValueError(
            f"Failed to convert C type '{render_ast(c_type)}' to a Haskell type")

    def parse_c_function_type(self, ty: c_ast.FuncDecl) -> Tuple[list[HaskellType], HaskellType]:
        hs_arg_tys: list[str] = []
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
            hs_arg_tys.append(hs_ty)

        hs_ret_ty = self.parse_c_type(ty.type)

        return (hs_arg_tys, hs_ret_ty)


@dataclass
class HsField:
    name: str
    ty: HaskellType
    comment: str | None = None


@dataclass
class HsArgType:
    ty: HaskellType
    comment: str | None = None


@dataclass
class HsReturnType:
    ty: HaskellType
    comment: str | None = None


class HsSourceWriter:
    def __init__(self, buffer: io.StringIO):
        self._buffer = buffer
        self._indent_size = 2

    def indent(self, levels: int) -> str:
        return " " * (self._indent_size * levels)

    def write_source(self, source: str) -> None:
        self._buffer.write(source)

    def write_line(self, line: str) -> None:
        self._buffer.write(line)
        self._buffer.write("\n")

    def write_lines(self, lines: str) -> None:
        for line in lines:
            self.write_line(line)

    def write_function_type(self, *, name: str, prefix: str | None, comment: str | None = None, args: list[HsArgType] | None = None, ret: HsArgType) -> None:
        if args is None:
            args = []

        if comment is not None:
            self.write_line(f"-- | {comment}")

        if prefix is None:
            self.write_line(f"{name} ::")
        else:
            self.write_line(f"{prefix} {name} ::")

        indent = self.indent(1)

        for i, arg in enumerate(args + [ret]):
            line: str
            if i == 0:
                line = f"{indent}{arg.ty}"
            else:
                line = f"{indent}-> {arg.ty}"

            if arg.comment is not None:
                line += f" -- ^ {arg.comment}"

            self.write_line(line)

    def write_record_type(self, *, name: str, comment: str | None = None, fields: list[HsField] | None = None) -> None:
        """
        Writes a single constructor record type of the following form:
        ```
        -- | comment
        data MyRecordType = MyRecordType
            { field1 :: Field1Type -- ^ Comment
            , field2 :: Field2Type -- ^ Comment
            }
        ```

        'fields' can be empty, in which case, the following will be generated.
        ```
        -- | comment
        data MyRecordType = MyRecordType
        ```
        """

        if fields is None:
            fields = []

        indent = self.indent(1)

        if comment is not None:
            self.write_line(f"-- | {comment}")

        self.write_line(f"data {name} = {name}")

        for i, field in enumerate(fields):
            start_ch = "{" if i == 0 else ","

            line = f"{indent}{start_ch} {field.name} :: {field.ty}"

            if field.comment is not None:
                line += f" -- ^ {field.comment}"

            self.write_line(line)

        if len(fields) > 0:
            self.write_line(f"{indent}}}")


# A set of functions to be excluded from having their 'foreign import ccall'
# definitions auto-generated.
# 
# Some SkiaSharp C API functions have struct-type arguments, and Haskell's FFI
# is incapable of interacting with these functions directly. We must manually
# define FFI bindings for them instead of relying on the auto-generator.
FUNCTION_BLOCKLIST = set("""
gr_direct_context_make_direct3d
gr_direct_context_make_direct3d_with_options
gr_direct_context_make_vulkan
gr_direct_context_make_vulkan_with_options
sk_canvas_clear_color4f
sk_canvas_draw_color4f
""".split())


class CSourceVisitor(c_ast.NodeVisitor):
    def __init__(self, srcwriter: HsSourceWriter):
        self.srcwriter = srcwriter
        self.typectx = TypeContext()

        # We need to track what opaque structs we've encountered. This is
        # because SkiaSharp's C API erroneously defines duplicate definitions.
        # We need to skip them.
        self._visited_opaque_structs: Set[str] = set()

    def visit_Typedef(self, node: c_ast.Typedef) -> None:
        """
        This function visits all 5 types of Skia C type definitions:
        - 1. Type aliases, e.g., `typedef uint32_t sk_pmcolor_t;`
        - 2. Enum types, e.g., `typedef enum { ... } gr_backend_t;`
        - 3. Struct types, e.g., `typedef struct { ... } gr_gl_framebufferinfo_t;`
        - 4. Opaque struct types, e.g., `typedef struct gr_d3d_memory_allocator_t gr_d3d_memory_allocator_t;`
        - 5. Function types, e.g., `typedef VKAPI_ATTR void (VKAPI_CALL *gr_vk_func_ptr)(void);`
        """

        if node.name in BORING_CLIB_TYPEDEF_NAMES:
            return

        if isinstance(node.type.type, c_ast.IdentifierType):
            self.handle_typedef_type_alias(node)
        elif isinstance(node.type.type, c_ast.Enum):
            self.handle_typedef_enum(node)
        elif isinstance(node.type.type, c_ast.Struct):
            struct_ty: c_ast.Struct = node.type.type
            if struct_ty.decls is None:
                self.handle_typedef_opaque_struct(node)
            else:
                self.handle_typedef_normal_struct(node)
        elif isinstance(node.type.type, c_ast.FuncDecl):
            self.handle_typedef_func_type_decl(node)
        else:
            raise ValueError(f"Unhandled node", node.name,
                             type(node.type.type))

    def handle_typedef_type_alias(self, node: c_ast.Node):
        c_def_name = node.name
        hs_def_name = capitalize_head(remove_suffix(c_def_name, "_t"))

        c_type = node.type.type
        hs_type = self.typectx.convert_c_type_to_hs_type(c_type)

        self.typectx.register_type(c_def_name, hs_def_name)

        self.srcwriter.write_line(f"-- | C type alias: @{render_ast(node)}@")
        self.srcwriter.write_line(f"type {hs_def_name} = {hs_type}")

    def handle_typedef_enum(self, node: c_ast.Node):
        enum_ty: c_ast.Enum = node.type.type

        c_def_name = node.name
        hs_def_name = capitalize_head(remove_suffix(c_def_name, "_t"))

        self.typectx.register_type(c_def_name=c_def_name, hs_type=hs_def_name)

        indent = self.srcwriter.indent(1)

        self.srcwriter.write_source(f"""\
-- | C enum: @{c_def_name}@
newtype {hs_def_name} = {hs_def_name} (#type {c_def_name})
{indent}deriving stock (Show, Eq, Ord)
{indent}deriving newtype (Num, Bits, Storable)
""")

        enum_values = list(enum_ty.values)
        for i, value in enumerate(enum_values, 1):
            c_value_name = value.name
            hs_value_name = value.name

            self.srcwriter.write_source(f"""\
-- | C enum @{c_def_name}@ value ({i}/{len(enum_values)}): @{c_value_name}@
pattern {hs_value_name} :: {hs_def_name}
pattern {hs_value_name} = (#const {c_value_name})
""")

    def handle_typedef_opaque_struct(self, node: c_ast.Node):
        """
        This function handles opaque structs like so:

        If 'node' is `typedef struct gr_glinterface_t gr_glinterface_t;`,
        then the generated Haskell code is:

        ```
        -- | @gr_glinterface_t@
        data Gr_glinterface = Gr_glinterface
            deriving (Show, Eq, Ord)
        ```
        """

        struct_ty: c_ast.Struct = node.type.type

        c_def_name = node.name
        hs_def_name = capitalize_head(remove_suffix(c_def_name, "_t"))

        # See notes on self._visited_opaque_structs
        if c_def_name in self._visited_opaque_structs:
            return
        else:
            self._visited_opaque_structs.add(c_def_name)

        self.typectx.register_type(c_def_name=c_def_name, hs_type=hs_def_name)

        self.srcwriter.write_record_type(
            name=hs_def_name,
            comment=f"Opaque C struct @{c_def_name}@",
            fields=[],
        )

    def handle_typedef_normal_struct(self, node: c_ast.Node):
        struct_ty: c_ast.Struct = node.type.type

        c_def_name = node.name
        hs_def_name = capitalize_head(remove_suffix(node.name, "_t"))

        self.typectx.register_type(c_def_name, hs_def_name)

        fields: list[HsField] = []

        for i, decl in enumerate(struct_ty.decls):
            decl: c_ast.Decl

            # c_field_name = decl.name
            hs_field_name = decl.name

            c_field_ty = decl.type
            hs_field_ty = self.typectx.convert_c_type_to_hs_type(c_field_ty)

            field = HsField(
                name=hs_field_name,
                ty=hs_field_ty,
                comment=f"C field: @{render_ast(decl)}@"
            )
            fields.append(field)

        self.srcwriter.write_record_type(
            name=hs_def_name,
            comment=f"C struct name: @{c_def_name}@",  # TODO:
            fields=fields,
        )

        indent = self.srcwriter.indent(1)
        indent2 = self.srcwriter.indent(2)

        # Generate the struct's Haskell 'Offset' instances
        for i, decl in enumerate(struct_ty.decls):
            c_field_name = decl.name
            hs_field_name = c_field_name

            self.srcwriter.write_line(
                f"instance Foreign.Storable.Offset.Offset \"{hs_field_name}\" {hs_def_name} where")
            self.srcwriter.write_line(
                f"{indent}rawOffset = (#offset {c_def_name}, {c_field_name})")

        # Generate the struct's Haskell 'Storable' instance
        self.srcwriter.write_line(
            f"instance Foreign.Storable.Storable {hs_def_name} where")
        self.srcwriter.write_line(f"{indent}sizeOf _ = (#size {c_def_name})")
        self.srcwriter.write_line(
            f"{indent}alignment _ = (#alignment {c_def_name})")

        # ... Generate the "peek" function
        self.srcwriter.write_line(f"{indent}peek p' = do")
        for i, decl in enumerate(struct_ty.decls):
            c_field_name = decl.name
            hs_field_name = c_field_name

            self.srcwriter.write_line(
                f"{indent2}{hs_field_name} <- (#peek {c_def_name}, {c_field_name}) p'")
        self.srcwriter.write_line(f"{indent2}pure {hs_def_name}{{..}}")

        # ... Generate the "poke" function
        self.srcwriter.write_line(f"{indent}poke p' {hs_def_name}{{..}} = do")
        for i, decl in enumerate(struct_ty.decls):
            c_field_name = decl.name
            hs_field_name = c_field_name

            self.srcwriter.write_line(
                f"{indent2}(#poke {c_def_name}, {c_field_name}) p' {hs_field_name}")

    def handle_typedef_func_type_decl(self, node: c_ast.Node):
        decl: c_ast.FuncDecl = node.type.type

        c_def_name = node.name
        hs_def_name = capitalize_head(c_def_name)

        f = unwrap_c_func_decl(decl)

        hs_arg_types: list[HaskellType] = []
        for arg in f.parameter:
            hs_arg_type = self.typectx.convert_c_type_to_hs_type(arg.arg_type)
            hs_arg_types.append(hs_arg_type)

        hs_ret_type = self.typectx.convert_c_type_to_hs_type(f.return_type)
        hs_ret_type = f"IO ({hs_ret_type})"

        hs_fn_type = " -> ".join(hs_arg_types + [hs_ret_type])

        self.srcwriter.write_source(f"""\
-- | C function pointer type: @{render_ast(node)}@
type {hs_def_name} = {hs_fn_type}
""")

        self.typectx.register_type(
            c_def_name=c_def_name, hs_type=f"FunPtr {hs_def_name}")

    def visit_FuncDecl(self, decl: c_ast.FuncDecl) -> None:
        def extract_fn_name() -> str:
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
                    raise ValueError(f"cannot unrecognize FuncDecl: {decl}")

        c_def_name = extract_fn_name()
        hs_def_name = c_def_name

        # See notes on FUNCTION_BLOCKLIST.
        if c_def_name in FUNCTION_BLOCKLIST:
            return

        f = unwrap_c_func_decl(decl)

        args: list[HsArgType] = []
        for c_arg in f.parameter:
            comment: str
            if c_arg.name is None:
                comment = f"C argument type: @{render_ast(c_arg.arg_type)}@"
            else:
                comment = f"C argument @\"{c_arg.name}\"@ of type @{render_ast(c_arg.arg_type)}@"

            args.append(HsArgType(
                ty=self.typectx.convert_c_type_to_hs_type(c_arg.arg_type),
                comment=comment
            ))

        ret = HsArgType(
            ty=f"IO ({self.typectx.convert_c_type_to_hs_type(f.return_type)})",
            comment=f"C return type: @{render_ast(f.return_type)}@",
        )

        self.srcwriter.write_function_type(
            name=hs_def_name,
            prefix=f"foreign import ccall \"{c_def_name}\"",
            comment=f"C function: @{render_ast(decl)}@",
            args=args,
            ret=ret,
        )


def gen_code(*, project_root_dir: Path) -> None:
    """
    Generates Haskell FFI definitions from the SkiaSharp C API headers.
    """
    ast = get_skia_ast()

    module_name = "SkiaSharp.Bindings.Internal.AutoGenerated"

    with (project_root_dir / "src" / "SkiaSharp" / "Bindings" / "Internal" / "AutoGenerated.hsc").open("w") as buffer:
        srcwriter = HsSourceWriter(buffer)

        extensions: list[str] = """
DeriveAnyClass
DeriveFoldable
DeriveFunctor
DeriveGeneric
DeriveTraversable
DerivingStrategies
DerivingVia
DuplicateRecordFields
EmptyCase
ExplicitForAll
GeneralizedNewtypeDeriving
NamedFieldPuns
OverloadedLabels
OverloadedRecordDot
OverloadedStrings
PatternSynonyms
ScopedTypeVariables
""".split()

        for extension in extensions:
            srcwriter.write_line(f"{{-# LANGUAGE {extension} #-}}")

        srcwriter.write_source(f"""
module {module_name} where

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Storable
import Foreign.Storable.Offset

#include \"c/sk_types.h\"

""")

        visitor = CSourceVisitor(srcwriter)
        visitor.visit(ast)
