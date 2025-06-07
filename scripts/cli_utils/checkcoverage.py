from __future__ import annotations

from dataclasses import dataclass
from .misc import (
    get_skia_include_info,
    get_skia_ast,
    render_ast,
    capitalize_head,
    SkiaCSourceVisitor,
    extract_name_from_func_decl,
)

import math


class SourceStringFinder:
    def __init__(self):
        self._sources = []

    def add_source(self, source: str) -> None:
        self._sources.append(source)

    def add_source_from_path(self, path: Path) -> None:
        self.add_source(path.read_text())

    def has_string(self, needle: str) -> bool:
        for src in self._sources:
            if needle in src:
                return True
        return False


@dataclass
class FunctionEntry:
    name: str
    decl: c_ast.FuncDecl


class FunctionCollectorVisitor(SkiaCSourceVisitor):
    def __init__(self):
        self._entries: list[FunctionEntry] = []

    def visit(self, node: c_ast.Node) -> list[FunctionEntry]:
        super().visit(node)
        return self._entries

    def handle_func_decl(self, decl: c_ast.FuncDecl) -> None:
        fn_name = extract_name_from_func_decl(decl)

        entry = FunctionEntry(name=fn_name, decl=decl)
        self._entries.append(entry)


def get_haskell_source_file_paths(project_root_dir: Path) -> list[Path]:
    paths = []
    for path in (project_root_dir / "src").glob("**/*.hs"):
        if path.match(f"Skia/Bindings/Internal/*.hs"):
            continue
        paths.append(path)
    return paths


def check_coverage(
    *,
    project_root_dir: Path,
    list_files: bool,
    print_c_signature: bool = True,
    print_unused_only: bool = False,
) -> None:
    hs_paths = get_haskell_source_file_paths(project_root_dir)

    if list_files:
        for hs_path in hs_paths:
            print(hs_path)
    else:
        hssrcfinder = SourceStringFinder()
        for hs_path in hs_paths:
            hssrcfinder.add_source_from_path(hs_path)

        info = get_skia_include_info()
        entries = FunctionCollectorVisitor().visit(get_skia_ast(info))

        entry_i_width = math.ceil(math.log(len(entries), 10))

        num_used = 0
        for entry_i, entry in enumerate(entries, 1):
            is_used = hssrcfinder.has_string(entry.name)
            if is_used:
                num_used += 1

            ### Filter

            if print_unused_only and is_used:
                continue

            ### Print

            entry_i_str = f"{entry_i}".rjust(entry_i_width)
            marker = "[x]" if is_used else "[ ]"
            function = f"{render_ast(entry.decl)}" if print_c_signature else entry.name

            print(f"{entry_i_str} {marker} {function}")

        # Print annotations and statistics
        percent = num_used / len(entries) * 100
        print(f"========================")
        print(f"  [x] covered   [ ] unused")
        print()
        print(f"  {num_used} out of {len(entries)} functions are used. {len(entries) - num_used} unused.")
        print(f"  ... {percent:.3f}% coverage")
