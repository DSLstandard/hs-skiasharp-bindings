from __future__ import annotations

from .misc import (
    get_skia_ast,
    render_ast,
    capitalize_head,
    SkiaCSourceVisitor,
)


class ListEnumsVisitor(SkiaCSourceVisitor):
    def __init__(self, *, skip_flag_enums: bool):
        self.skip_flag_enums = skip_flag_enums

    def handle_typedef_enum(self, node: c_ast.Node) -> None:
        if self.skip_flag_enums:
            # Flag enums have suffix 'flags_t'/'mask_t'. We can exploit this.
            c_def_name = node.name

            is_flag = c_def_name.endswith("flags_t") or c_def_name.endswith("mask_t")
            if is_flag:
                return

        print()
        print(render_ast(node))

def list_enums():
    ast = get_skia_ast()
    visitor = ListEnumsVisitor(skip_flag_enums=True)
    visitor.visit(ast)
