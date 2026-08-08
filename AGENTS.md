# AGENTS.md

Personal dotfiles for bash, emacs, git, tmux, plus toolchain infrastructure and a uv workspace for Python automation.

@.principles/README.md

## Entry point

`./configure.sh` (idempotent) bootstraps prereqs and syncs the live system to repo state. Implementation in `.toolchain/`.

## Repo-wide conventions

- **OS detection**: `HOST_OS` from `uname` (`linux` / `darwin`). Linux is primary; Darwin is supported for cross-platform pieces only.
- **Cached state**: `~/.config/jaspah/*.env` holds machine-local values (identity) — written by bootstrap, read by sync and by the live shell.
- **Python workspace**: self-contained uv workspace whose root `pyproject.toml` is the primary `jaspah` package (`src/jaspah/`); related packages are members under `packages/<name>/` (see `packages/README.md`). It builds/tests/type-checks standalone and is consumable by other uv workspaces directly (path/git source to the repo root) or per member — never as a nested workspace member. `uv sync --all-packages --inexact` (run by `.toolchain/sync.sh`) installs the root + members editable into the base venv `~/.venv/dev` (activated by `bash/.bashrc`) without pruning deps owned by other repos that share that venv; console scripts (e.g. `jaspah`) land on `PATH`.
- **Python tooling**: ruff (format + lint) and basedpyright (typecheck, `standard`) are mandatory; shared config lives in the root `pyproject.toml`. Write fully typed code. The canonical gate is `scripts/py-quality.sh [--fix] <paths>`, enforced two ways:
  - **All harnesses / humans**: a repo-scoped git pre-commit hook (`git/hooks/pre-commit`, wired via `core.hooksPath`) blocks commits that fail. codex and pi inherit enforcement here; run `scripts/py-quality.sh <paths>` yourself before committing.
  - **Claude Code**: a `PostToolUse` hook (`.claude/hooks/py-quality.sh` → the shared gate with `--fix`) auto-formats/fixes each edited `*.py` and surfaces type errors immediately.
- **Config deployment**: append-then-source. `scripts/utilities.sh::load_custom_config` injects a delimited include block at the top of `~/.bashrc`, `~/.gitconfig`, `~/.tmux.conf`. Repo files are not symlinked into `~` except powerline and Herdr configs.
