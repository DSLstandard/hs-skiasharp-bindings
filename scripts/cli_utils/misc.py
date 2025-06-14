from __future__ import annotations


from pathlib import Path
from pycparser import c_ast, c_generator
from dataclasses import dataclass
import pycparser
import shlex
import subprocess
import tempfile


def extract_name_from_func_decl(decl: FuncDecl) -> str:
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


class SkiaCSourceVisitor(c_ast.NodeVisitor):
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

    def visit_FuncDecl(self, decl: c_ast.FuncDecl) -> None:
        return self.handle_func_decl(decl)

    def handle_typedef_type_alias(self, node: c_ast.Node) -> None:
        pass

    def handle_typedef_enum(self, node: c_ast.Node) -> None:
        pass

    def handle_typedef_opaque_struct(self, node: c_ast.Node) -> None:
        pass

    def handle_typedef_normal_struct(self, node: c_ast.Node) -> None:
        pass

    def handle_typedef_func_type_decl(self, node: c_ast.Node) -> None:
        pass

    def handle_func_decl(self, decl: c_ast.FuncDecl) -> None:
        pass


def capitalize_head(string: str) -> str:
    """
    Capitalizes the first character of the input string.
    """
    return string[0].upper() + string[1:]


def render_ast(node: c_ast.Node) -> str:
    """
    Renders a 'c_ast.Node' to C source.
    """
    g = c_generator.CGenerator()
    src = g.visit(node)
    return src


@dataclass
class SkiaIncludeInfo:
    # The Skia include directory. It should be the parent of the subdirectories
    # 'c/' and 'xamarin/'.
    include_dir: Path

    # Paths of all C API headers relative to 'include_dir'.
    c_headers: list[Path]


def get_skia_include_info() -> SkiaIncludeInfo:
    """
    Finds the directory containing the header files of Mono Skia's C API through
    pkg-config.
    """

    args = ["pkg-config", "--cflags-only-I", "skia"]
    output = subprocess.check_output(args, shell=False, encoding="utf-8")
    # 'output' should look something like '-I/usr/include/skia'

    include_dir = Path(output[2:].strip())

    c_headers = []

    for path in (include_dir / "c").glob("*.h"):
        c_headers.append(path.relative_to(include_dir))
    for path in (include_dir / "xamarin").glob("sk_*.h"):
        c_headers.append(path.relative_to(include_dir))

    return SkiaIncludeInfo(include_dir=include_dir, c_headers=c_headers)


def get_skia_ast(info: SkiaIncludeInfo) -> pc_ast.Node:
    """
    Returns a giant pycparser AST node containing all Mono Skia C API types and
    functions.
    """

    # This is the general procedure of this function:
    # - 1. Generate a dummy header file that includes all Mono Skia C header files.
    # - 2. Run the C pre-processor on that dummy header file.
    # - 3. Have pycparser parse the output of step 2.

    # Without putting this at the start of the dummy header file,
    # 'pycparser.parse_file' will raise an error. More details about this in
    # https://github.com/eliben/pycparser/wiki/FAQ#what-do-i-do-about-__attribute__.
    dummy_header_src = "#define __attribute__(x)\n"

    for path in info.c_headers:
        dummy_header_src += f"#include <{path}>\n"

    with tempfile.NamedTemporaryFile(delete_on_close=False) as dummy_header_file:
        dummy_header_file.write(dummy_header_src.encode())
        dummy_header_file.close()

        return pycparser.parse_file(
            dummy_header_file.name,
            use_cpp=True,
            cpp_args=f"-I{shlex.quote(str(info.include_dir))}"
        )


# A set of C built-in types.
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

# A set of Haskell keywords.
HASKELL_KEYWORDS = set("""
instance
data
deriving
class
type
""".split())
