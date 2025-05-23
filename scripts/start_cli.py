from __future__ import annotations

from pathlib import Path
import logging
import argparse
import sys
import cli_utils.gencode
import cli_utils.misc


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
        description="Prints the include directory of the Skia library"
    )
    root_findskiainclude.set_defaults(command="find-skia-include")

    root_gencode = root_.add_parser(
        "gen-code",
        description="Generates Haskell FFI definitions from the SkiaSharp C API headers."
    )
    root_gencode.set_defaults(command="gen-code")

    # TODO: what
    args = root.parse_args()

    if not hasattr(args, "command"):
        # The user should be typing 'python start_cli.py <command_name>', but
        # the code reaches here if the user does not write '<command_name>'.
        root.print_help()
        return

    if args.command == "find-skia-include":
        print(str(cli_utils.misc.find_skia_include_dir()))
    elif args.command == "gen-code":
        cli_utils.gencode.gen_code(project_root_dir=args.project_root_dir)
    else:
        raise NotImplementedError(
            f"Command '{args.command}' is not implemented")


if __name__ == "__main__":
    main()
