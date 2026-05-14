# Toolchain

Bootstrap and sync infrastructure for the jconfig repo. Invoked via the root `configure.sh` shim.

## Scripts

Two idempotent scripts, run in order by the root shim:

- `bootstrap.sh` — interactive one-time prereqs.
- `sync.sh` — non-interactive sync of live system to repo state.

Both require `JCONFIG_ROOT` in env. See each script's header for scope.

## Emacs build

Builds emacs from source into `~/.local/emacs/`; sources extracted under `~/.cache/jconfig/emacs-build/`. Settings cached in `~/.config/jconfig/emacs.env`:

- `JCONFIG_EMACS_VERSION` — pinned semver (e.g. `29.4`)
- `JCONFIG_EMACS_AUTO_UPDATE_MAJOR` / `JCONFIG_EMACS_AUTO_UPDATE_MINOR` — auto-update policy

Configure flags live in `~/.config/jconfig/emacs-configure.sh`, generated on first run and user-editable. Defaults include native AOT compilation, tree-sitter, xwidgets, imagemagick, GTK3, and cairo.
