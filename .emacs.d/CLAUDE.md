# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this directory.

## Overview

Emacs configuration using literate programming with org-babel. The main config is `init.org` which tangles to `~/.emacs`.

## Tangling Process

To regenerate `~/.emacs` after editing `init.org`:
1. Narrow to Bootstrap Process section: `C-x n s`
2. Tangle: `C-c C-v t`

The header property `#+PROPERTY: header-args:emacs-lisp :tangle ...` controls where code blocks are tangled.

## Directory Structure

- `languages/` - Language-specific configs, each file named `j-<lang>.el` and provides `'j-<lang>`
- `packages/` - Package configurations, each file named `j-<pkg>.el` and provides `'j-<pkg>`
- `tangles/` - Generated elisp from org-babel (gitignored)
- `.emacs-custom.el` - Emacs customize output (gitignored)

## Conventions

All elisp modules use `j-` prefix and must end with `(provide 'j-<name>)`. They are loaded via `(require 'j-<name>)` in `languages.el`.

Package configs use `use-package` with `:defer t` for lazy loading where appropriate.

## Key Files

- `init.org` - Main config, sections: Bootstrap Process, Settings, Hooks, Keybindings, Packages
- `j-dot-org.org` - Org-mode specific configuration (also literate style)
- `languages.el` - Loads all language modules from `languages/`
