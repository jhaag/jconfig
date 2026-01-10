# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

Personal dotfiles repository managing configurations for bash, emacs, git, and tmux across Linux and macOS.

## Setup & Deployment

Run `./configure.sh` to deploy configurations. It uses a sourcing/including pattern rather than symlinks:
- Bash: Adds source block to `~/.bashrc` that loads this repo's `.bashrc`
- Git: Adds `[include]` to `~/.gitconfig` pointing to this repo's `.gitconfig`
- Tmux: Adds `source-file` to `~/.tmux.conf`
- Powerline: Copies configs directly to home directory

The `scripts/utilities.sh` file contains `load_custom_config()` which manages versioned config blocks with delimiter guards.

## Emacs Configuration

Uses literate programming with org-babel. Main config is `.emacs.d/init.org` which tangles to elisp.

Structure:
- `.emacs.d/languages/` - Language-specific configs (j-js.el, j-org.el, j-rust.el, etc.)
- `.emacs.d/packages/` - Package configurations (j-helm.el, j-solarized.el, etc.)
- `.emacs.d/tangles/` - Generated elisp (gitignored)

To update emacs config after editing init.org: narrow to Bootstrap Process section (C-x n s), then tangle (C-c C-v t).

## Shell Configuration

- `aliases/shared.sh` - Cross-platform aliases
- `aliases/linux.sh` and `aliases/darwin.sh` - OS-specific aliases
- `scripts/shell_prompt.sh` - Powerline-shell PROMPT_COMMAND setup
- `powerline_opam_switch.py` - Custom powerline segment for OCaml

OS detection uses `HOST_OS` variable set from `uname`.

## Key Files

- `.gitconfig` - Extensive git aliases (see `git aliases` command). User-specific data in `.gitconfig.user` (not tracked)
- `.tmux.conf` - Prefix is Ctrl+Space, pane nav uses Ctrl-Alt + emacs keys (b/f/p/n)
- `update-alternatives-llvm-toolchain.sh` - LLVM/Clang version management via update-alternatives
