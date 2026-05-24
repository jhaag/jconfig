# Toolchain

Bootstrap and sync infrastructure for the jaspah repo. Invoked via the root `configure.sh` shim.

## Scripts

Two idempotent scripts, run in order by the root shim:

- `bootstrap.sh` — interactive one-time prereqs.
- `sync.sh` — non-interactive sync of live system to repo state.

Supporting one-shot migration script:

- `scripts/relocate.sh [new-local-clone-name] [--origin <git-url>]` — rename a pre-rename checkout, update origin, migrate old machine-local state into the `jaspah` namespace, then run `configure.sh`.

Bootstrap/sync require `JASPAH_ROOT` in env. See each script's header for scope.
