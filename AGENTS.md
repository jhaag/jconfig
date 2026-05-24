# AGENTS.md

Personal dotfiles for bash, emacs, git, tmux, plus toolchain infrastructure and a uv workspace for Python automation.

@.principles/README.md

## Entry point

`./configure.sh` (idempotent) bootstraps prereqs and syncs the live system to repo state. Implementation in `.toolchain/`.

## Repo-wide conventions

- **OS detection**: `HOST_OS` from `uname` (`linux` / `darwin`). Linux is primary; Darwin is supported for cross-platform pieces only.
- **Cached state**: `~/.config/jconfig/*.env` holds machine-local values (identity, build settings) — written by bootstrap, read by sync and by the live shell.
- **Base venv**: `~/.venv/dev`, activated by `bash/.bashrc`. `uv sync` installs workspace members editable here, importable from any shell.
- **Config deployment**: append-then-source. `scripts/utilities.sh::load_custom_config` injects a delimited include block at the top of `~/.bashrc`, `~/.gitconfig`, `~/.tmux.conf`. Repo files are not symlinked into `~` except powerline and Herdr configs.
