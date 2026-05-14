# Exceptions

Human-approved deviations from `README.md`. Each entry names the file or pattern, the principle deviated from, and the rationale.

## `/CLAUDE.md`

- Deviates from: *self-contained* — files should serve a present purpose in the repo as it is.
- Rationale: compatibility shim for agents that load `CLAUDE.md` but not `AGENTS.md`. Contents: `@AGENTS.md`.
