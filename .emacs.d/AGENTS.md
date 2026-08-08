# .emacs.d

Emacs configuration in literate org-babel form. `init.org` tangles to `~/.emacs`.

## Tangling

To regenerate `~/.emacs` after editing `init.org`:
1. Narrow to Bootstrap Process section: `C-x n s`
2. Tangle: `C-c C-v t`

Tangle destinations are controlled by `#+PROPERTY: header-args:emacs-lisp :tangle ...` headers within `init.org`.

## Conventions

- Modules under `languages/` and `packages/` are named `j-<name>.el` and end with `(provide 'j-<name>)`. Loaded via `(require 'j-<name>)` in `languages.el`.
- Package configs use `use-package` with `:defer t` where appropriate.
- `tangles/` and `.emacs-custom.el` are gitignored (build/customize output).

## Notable files

- `init.org` — main literate config. Top-level sections: Bootstrap Process, Settings, Hooks, Keybindings, Packages.
- `j-dot-org.org` — org-mode-specific configuration, also literate.
- `languages.el` — loads all `languages/` modules.
