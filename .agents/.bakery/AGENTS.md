# Pi bakery

## Scope

Applies under `.agents/.bakery/`.

## Layout invariants

- `packages/` is for local pi package source.
- `pies/` is for selectable `.pi` profiles and profile skeletons.
- Pies may contain checked-in declarative settings, but must not contain runtime databases, indexes, caches, credentials, or other machine-local state.
- Runtime state should live under repo-local `.local/`, which is gitignored.

## Package posture

- The current global `pi install` setup is transitional, not the target management model.
- Prefer self-managed, reproducible package management unless that decision is revisited explicitly.
- Candidate management paths are Nix-managed tooling, `nvm`/npm-managed tooling, and per-pie `package.json` plus lockfile state.
- `jconfig` should consume upstream keybindings, themes, prompts, skills, and UI/UX resources where possible.
- Add local functionality only where upstream resources do not cover the intended workflow.
- Do not duplicate source-owned settings when an indirection to the canonical file or upstream resource suffices.
