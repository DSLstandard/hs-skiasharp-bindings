from __future__ import annotations

from .misc import (
    get_skia_ast,
    render_ast,
    capitalize_head,
    SkiaCSourceVisitor,
)


class ListOpaqueStructsVisitor(SkiaCSourceVisitor):
    def __init__(self):
        pass

    def handle_typedef_opaque_struct(self, node: c_ast.Node) -> None:
        struct_ty: c_ast.Struct = node.type.type

        c_def_name = node.name
        print(c_def_name)

def list_opaque_structs():
    ast = get_skia_ast()
    visitor = ListOpaqueStructsVisitor()
    visitor.visit(ast)
