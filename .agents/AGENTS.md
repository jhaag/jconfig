# Agent tooling

## Scope

Applies under `.agents/`. Subtree `AGENTS.md` files add narrower rules.

## Source vs local state

- Committed symlinks that select the active agent profile are source-owned configuration.
- `.pi` should point at `.agents/.tastiest_pi`.
- `.agents/.tastiest_pi` should point at the selected pie under `.agents/.bakery/pies/`.
- Runtime state, databases, caches, indexes, credentials, and host identity belong under `.local/` or another gitignored path.

## Package posture

- Prefer upstream packages/resources over local forks.
- Local packages should integrate, adapt, or extend upstream behavior; they should not vendor upstream resources without a specific reason.
- Keep package source portable enough for future Nix, devcontainer, and OCI use.
