# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this directory.

## Overview

Shell utility scripts sourced by the main `.bashrc`.

## Key Functions

### `load_custom_config()`
Core deployment function used by `configure.sh`. Manages versioned config blocks with delimiter guards.

Parameters:
1. Custom config content string
2. Target file path
3. Comment prefix for the file type

Creates or updates a delimited section at the top of config files, preserving existing content below.

### `mkcd()`
Create directory and cd into it.

### `is_interactive_shell()`
Returns true if running in interactive bash session.

## Files

- `utilities.sh` - Core helper functions
- `shell_prompt.sh` - Powerline-shell PROMPT_COMMAND setup
