# scripts

Runtime shell helpers — sourced or invoked by the live shell environment (`.bashrc`, tmux, prompt).

## `load_custom_config()` in `utilities.sh`

Used by `.toolchain/sync.sh` to inject a delimited include block at the top of `~/.bashrc`, `~/.gitconfig`, `~/.tmux.conf`. Parameters:

1. Custom config content string
2. Target file path
3. Comment prefix for the target file's syntax (e.g. `#`)

Re-runs strip the previous delimited block before re-inserting, preserving content below it.
