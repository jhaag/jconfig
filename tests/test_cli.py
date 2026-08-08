"""Smoke tests for the jaspah CLI, so the package is self-contained and validatable."""

from __future__ import annotations

from jaspah.cli import build_parser, main


def test_parser_builds() -> None:
    assert build_parser().prog == "jaspah"


def test_doctor_runs() -> None:
    assert main(["doctor"]) == 0


def test_no_command_prints_help() -> None:
    assert main([]) == 0
