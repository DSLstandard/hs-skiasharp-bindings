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
import re
import math
import yaml


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
class IgnoreListEntry:
    pattern: re.Pattern
    why_ignore: str


class IgnoreListManager:
    """
    A manager class to provide convenience API to query things from
    "checkcoverage-ignorelist.yaml".
    """

    def __init__(self):
        self._entries: List[IgnoreListEntry] = []

    def add_entry(self, entry: IgnoreListEntry) -> None:
        self._entries.append(entry)

    def find_entry(self, fn_name: str) -> IgnoreListEntry | None:
        """
        Given a C function name, lookup an matching 'IgnoreListEntry'. Returns
        'None' if there are no matches.
        """
        # This is O(n), where n = len(self._entries), but n is expected to be <=
        # 10, so no need to hyperoptimize.
        for entry in self._entries:
            if re.fullmatch(entry.pattern, fn_name):
                return entry
        return None

    @staticmethod
    def parse_file(path: Path) -> IgnoreListManager:
        """
        Parses "checkcoverage-ignorelist.yaml".
        """

        with path.open("r") as f:
            data0 = yaml.load(f, Loader=yaml.Loader)

        ignorelist = IgnoreListManager()

        for entry0 in data0["entries"]:
            entry = IgnoreListEntry(
                pattern=re.compile(entry0["pattern"]),
                why_ignore=entry0["why-ignore"],
            )
            ignorelist.add_entry(entry)

        return ignorelist


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
    print_c_signature: bool,
    print_unused_only: bool,
    show_ignored: bool,
    ignorelist_file: Path,
) -> None:
    hs_paths = get_haskell_source_file_paths(project_root_dir)

    if list_files:
        for hs_path in hs_paths:
            print(hs_path)
    else:
        hssrcfinder = SourceStringFinder()
        for hs_path in hs_paths:
            hssrcfinder.add_source_from_path(hs_path)

        ignorelist = IgnoreListManager.parse_file(ignorelist_file)

        info = get_skia_include_info()
        entries = FunctionCollectorVisitor().visit(get_skia_ast(info))

        entry_i_width = math.ceil(math.log(len(entries), 10))

        num_used = 0
        num_ignored = 0
        for entry_i, entry in enumerate(entries, 1):
            ignore_entry = ignorelist.find_entry(entry.name)
            is_ignored = ignore_entry is not None

            is_used = hssrcfinder.has_string(entry.name)
            if is_used:
                num_used += 1

            if is_ignored:
                num_ignored += 1

            # *** Filter

            if print_unused_only and is_used:
                continue

            if not show_ignored and is_ignored:
                continue

            # *** Print
            output = ""

            # Entry index
            output += f"{entry_i}".rjust(entry_i_width)

            # Marker annotation
            marker: str = {
                (False, False): "[ ]",
                (True, False):  "[x]",
                (False, True):  "[i]",
                (True, True):   "[*]"
            }[is_used, is_ignored]
            output += " "
            output += marker

            # Function name/signature
            output += " "
            if print_c_signature:
                output += f"{render_ast(entry.decl)}"
            else:
                output += entry.name

            # PSA
            if is_ignored and show_ignored:
                output += f" // Ignored: {ignore_entry.why_ignore}"

            print(output)

        # Print marker annotations and statistics
        print(f"""\
========================
  [ ] = unused, to be implemented
  [x] = covered
  [i] = ignored, need no to be implemented
  [*] = covered yet ignored

  There are {len(entries)} functions in total
  {num_used} are used ({num_used / len(entries) * 100:.3f}%)
  {num_ignored} are ignored ({num_ignored / len(entries) * 100:.3f}%)\
""")
