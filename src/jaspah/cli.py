"""Command-line entry point for jaspah (``jaspah`` console script).

Later phases register ``session``, ``scratch``, and ``agent`` subcommands here; for
now it exposes ``--version`` and a ``doctor`` diagnostic.
"""

from __future__ import annotations

import argparse
import shutil
import sys

from jaspah import __version__


def cmd_doctor(_args: argparse.Namespace) -> int:
    """Print a short environment diagnostic."""
    print(f"jaspah  {__version__}")
    print(f"python  {sys.version.split()[0]}")
    print(f"venv    {sys.prefix}")
    print(f"tmux    {shutil.which('tmux') or 'MISSING'}")
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="jaspah", description="Personal automation toolkit."
    )
    parser.add_argument("--version", action="version", version=f"jaspah {__version__}")
    parser.set_defaults(func=None)

    subparsers = parser.add_subparsers(dest="command")
    doctor = subparsers.add_parser("doctor", help="Print environment diagnostics.")
    doctor.set_defaults(func=cmd_doctor)

    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if args.func is None:
        parser.print_help()
        return 0
    result: int = args.func(args)
    return result


if __name__ == "__main__":
    raise SystemExit(main())
