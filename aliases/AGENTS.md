# aliases

Shell aliases and functions, dispatched by `$HOST_OS`.

`bash/.bashrc` sources `shared.sh` plus exactly one of `linux.sh` or `darwin.sh`.

## Notable functions in shared.sh

- `cdl` — cd and ls.
- `cheat` — query cht.sh cheat sheets.
- `venv-activate <name>` / `venv-deactivate` — activate/deactivate `~/.venv/<name>`.
- `git-brprune` — prune local branches not tracking a remote.
