"""jaspah — personal automation toolkit.

Houses the dynamic, computed tier of tmux session management (scratch panes from
org conflicts, out-of-date packages, …) plus agent spawning and session helpers.
A self-contained, installable package: it builds, tests, and validates on its own
and does not need to be a member of any other uv workspace. Installed editable into
the bashrc-managed base venv (``~/.venv/dev``) by ``uv sync``, so ``import jaspah``
and the ``jaspah`` console script work from any shell.
"""

from __future__ import annotations

__version__ = "0"
