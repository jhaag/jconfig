# R&D -- tailoring `pi` to my needs

## Goals

Roughly-ordered:
- [ ] nix-based reproducible dev environments
- [ ] containerized (sub)agents
- [ ] literate programming for everything
- [ ] self-improving CLI / dev-tooling
  - [ ] emacs
  - [ ] tmux
  - [ ] `pi`
- [ ] micro-agents & agent-swarms

### Operational

- efficiency:
  - [ ] efficient context utilization
  - [ ] effective subagents
- reproducibility / experimentation:
  - [ ] reproducible pi configuration
  - [ ] deployable pi configs for different (sub)agents
- security:
  - [ ] containerized agents
  - [ ] tmux-based interaction + screenshots for terminal UI/UX+tooling dev.

## Current bootstrap state

- Package installation is currently global/user-level via `pi install`.
  - This is transitional: move to self-managed reproducible package management ASAP.
  - Preferred candidates are Nix-managed tooling, `nvm`/npm-managed tooling, and per-pie `package.json` plus lockfile state.
  - Avoid third-party package-management layers for now unless that decision is revisited explicitly.
  - The active package list below is observational, not a lockfile.
- The active project `.pi` is selected by committed symlinks:
  - `.pi` -> `.agents/.tastiest_pi`
  - `.agents/.tastiest_pi` -> `.bakery/pies/kitchen_sink`
- `.agents/.tastiest_pi` is the stable profile selector.
- `.agents/.fresh_pi` is reserved as the experimental profile selector when needed.
- The selected pie should contain declarative profile settings only.
- Runtime state belongs under gitignored `.local/`.
  - `pi-total-recall` data lives at `.local/pi/total-recall`.
  - Memory/session databases, indexes, caches, credentials, and host identity must not live under `pies/` or `packages/`.

## `jconfig` package direction

`jconfig` should be a local integration package/catalog for pi and this repo.

- Prefer consuming upstream keybindings, themes, prompts, skills, and UI/UX resources.
- Add local glue/functionality where upstream resources do not cover the intended workflow.
- Do not vendor or duplicate upstream resources without a specific reason.
- Keep package source portable for future Nix/devcontainer/OCI use.
- Keep host-specific activation separate from package source.

Initial likely shape:

```text
.agents/.bakery/packages/jconfig/
  package.json
  README.md
  prompts/
  skills/
  themes/
```

This package should start thin. Existing shell, tmux, Emacs, powerline, and git settings remain canonical in their current repo locations until a cleaner package/activation boundary exists.

## Reproducibility/container path

1. Keep source-owned agent profile selectors committed.
2. Keep machine-local runtime state under `.local/`.
3. Move pi package management from global `pi install` to a self-managed reproducible mechanism.
   - Option A: Nix provides `node`, `npm`, `pi`, and package activation.
   - Option B: `nvm` pins Node while each pie owns `package.json` plus `package-lock.json`.
   - Option C: combine Nix for system/toolchain reproducibility with per-pie npm lockfiles for pi package sets.
4. Add a Nix `devShell` as the first reproducible execution boundary.
5. Later emit devcontainer/OCI images from the Nix/package boundary.

## Open questions

- Whether package ownership should be Nix-first, `nvm`/npm-first, or hybrid.
- Whether each pie should own its own `package.json`/`package-lock.json`, or share one package set.
- Whether `jconfig` should be installed globally, per-pie, or exposed through a Nix/devShell activation path.
- Which upstream packages should provide the canonical keybindings/themes/UI resources.
- Which pie profiles should be committed beyond `kitchen_sink`.

## Tried

`pi` packages I've tried.

cf. for a collection of cool packages: https://github.com/ogulcancelik/pi-extensions

### Active

#### Core

- [pi-subagents](https://pi.dev/packages/pi-subagents): subagent support
- [pi-lean-ctx](https://pi.dev/packages/pi-lean-ctx): [LeanCtx](https://leanctx.com/) bindings for context compaction (focused on tool usage)
- [pi-total-recall](https://pi.dev/packages/pi-total-recall): "complete context stack"
  - [memory](https://github.com/samfoy/pi-memory)
  - [session history](https://github.com/samfoy/pi-session-search)
  - [knowledge base](https://github.com/samfoy/pi-knowledge-search)

#### QoL

- [pi-mermaid](https://pi.dev/packages/pi-mermaid): inline ASCII rendering of mermaid diagrams
- [pi-simplify](https://pi.dev/packages/pi-simplify): review diffs for clarity, consistency & maintainability
- [pi-minimal-footer](https://pi.dev/packages/@ogulcancelik/pi-minimal-footer): replacement footer with model usage / subscription info
- [pi-tmux](https://github.com/ogulcancelik/pi-extensions/tree/main/packages/pi-tmux): tmux pane management tool for long-running commands when Pi is running inside tmux

### Retired

### Avoided

- [pi-depo](https://pi.dev/packages/pi-depo): declarative `pi` package manager
  - Avoided for now: prefer self-managed reproducibility via Nix, `nvm`/npm, and/or per-pie `package.json` plus `package-lock.json`.
  - Revisit only if the self-managed path becomes more complex than the package-manager dependency.

## Planned

`pi` packages I plan to try:

### Toolchain

- Nix-based package/tool activation for `pi` and related Node tooling.
- `nvm`/npm-based package activation with per-pie `package.json` plus `package-lock.json`.

### Core

- [pi-lens](https://pi.dev/packages/pi-lens): real-time, tool-based code feedback for `pi`
  - "LSP, linters, formatters, type-checking, structural analysis & booboo"
- [pi-prompt-template-model](https://pi.dev/packages/pi-prompt-template-model): det. control of dispatch/routing
  - frontmatter control knobs w/deterministic handling: model, skills, subagents, etc...
  - splice data into prompts
  - prompts
- [pi-autocontext](https://pi.dev/packages/pi-autocontext): recursive self-improvement via [autocontext](https://github.com/greyhaven-ai/autocontext) (cf. https://greyhaven.ai/)
  
### Domains
  
- [feynman](https://pi.dev/packages/@companion-ai/feynman): [alphaXiv](https://www.alphaxiv.org/) based deep-research
