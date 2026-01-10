# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this directory.

## Overview

Shell aliases and functions, split by platform.

## Files

- `shared.sh` - Cross-platform aliases loaded on all systems
- `linux.sh` - Linux-specific aliases (loaded when `HOST_OS=Linux`)
- `darwin.sh` - macOS-specific aliases (loaded when `HOST_OS=Darwin`)

## Loading

The main `.bashrc` sources the appropriate files based on `HOST_OS` variable (set from `uname`).

## Notable Functions in shared.sh

- `cdl` - cd and ls
- `cheat` - Query cht.sh cheat sheets
- `venv-activate <name>` - Activate Python venv from `~/.venv/<name>`
- `venv-deactivate` - Deactivate current venv
- `git-brprune` - Prune local branches not tracking remote
