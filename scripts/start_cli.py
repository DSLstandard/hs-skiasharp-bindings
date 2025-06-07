from __future__ import annotations

from pathlib import Path
import logging
import argparse
import sys
import cli_utils.gencode
import cli_utils.listenums
import cli_utils.listopaquestructs
import cli_utils.misc
import cli_utils.checkcoverage


def main():
    logging.basicConfig(level=logging.DEBUG, stream=sys.stderr)

    # Setup parser
    root = argparse.ArgumentParser(
        description="Generates Haskell code and does other things",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    root.add_argument(
        "-C", "--project-root-dir", type=Path,
        required=False,
        default=Path.cwd().parent,
        help="specify the directory of the project (it is usually the same as this project's git root directory) so the program knows where to put generated files at."
    )

    root_ = root.add_subparsers()

    root_findskiainclude = root_.add_parser(
        "find-skia-include",
        description="Prints the include directory of Mono's Skia library"
    )
    root_findskiainclude.set_defaults(command="find-skia-include")

    root_gencode = root_.add_parser(
        "gen-code",
        description="Generates Haskell FFI definitions from Mono's Skia C API headers."
    )
    root_gencode.set_defaults(command="gen-code")

    root_listenums = root_.add_parser(
        "list-enums",
        description="Lists all SkiaSharp C API enums. Can be used for creating prompts for AI-assisted code generation."
    )
    root_listenums.set_defaults(command="list-enums")

    root_listopaquestructs = root_.add_parser(
        "list-opaque-structs",
        description="Lists all SkiaSharp C API opaque structs. Can be used for creating prompts for AI-assisted code generation."
    )
    root_listopaquestructs.set_defaults(command="list-opaque-structs")

    root_checkcoverage = root_.add_parser(
        "check-coverage",
        description="Checks which SkiaSharp C API functions have been used by modules in src/ (excluding modules in src/SkiaSharp/Bindings). It helps to assess the library's bindings coverage during development."
    )
    root_checkcoverage.set_defaults(command="check-coverage")
    root_checkcoverage.add_argument(
        "-l", "--list-files",
        help="Makes the command print out all source files it will check for function coverage and then terminate.",
        action="store_true",
    )
    root_checkcoverage.add_argument(
        "-u", "--print-unused-only",
        help="Only list unused functions",
        action="store_true",
    )
    root_checkcoverage.add_argument(
        "-p", "--print-c-signature",
        help="If enabled, C function signatures are printed, instead of C function names.",
        action="store_true",
    )

    # TODO: what
    args = root.parse_args()

    if not hasattr(args, "command"):
        # The user should be typing 'python start_cli.py <command_name>', but
        # the code reaches here if the user does not write '<command_name>'.
        root.print_help()
        return

    if args.command == "find-skia-include":
        info = cli_utils.misc.get_skia_include_info()
        print(str(info.include_dir))
    elif args.command == "gen-code":
        cli_utils.gencode.gen_code(project_root_dir=args.project_root_dir)
    elif args.command == "list-enums":
        cli_utils.listenums.list_enums()
    elif args.command == "list-opaque-structs":
        cli_utils.listopaquestructs.list_opaque_structs()
    elif args.command == "check-coverage":
        cli_utils.checkcoverage.check_coverage(
            project_root_dir=args.project_root_dir,
            list_files=args.list_files,
            print_c_signature=args.print_c_signature,
            print_unused_only=args.print_unused_only,
        )
    else:
        raise NotImplementedError(
            f"Command '{args.command}' is not implemented")


if __name__ == "__main__":
    main()
