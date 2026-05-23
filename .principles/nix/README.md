# Nix Principles

These rules govern `flake.nix`, `flake.lock`, `nix/`, Nix-backed shell integration, and Nix-backed tool profiles.

## Role

Use Nix as the reproducible toolchain and packaging layer.

Keep `jconfig` responsible for live user configuration:

- bash, git, tmux, and Emacs settings
- cached host identity under `~/.config/jconfig/`
- append-then-source dotfile deployment
- project language environments managed by their native tools

## Entry points

Expose Nix behavior through flake outputs.

- Use `devShells` for project-scoped interactive environments.
- Use `packages` for installable tools and stable profile contents.
- Use `apps` for command entry points.
- Use `checks` for validation.
- Use `formatter` for Nix formatting.

Keep app outputs thin. Delegate to existing repo entry points instead of duplicating shell logic in Nix.

## Ownership

Prefer the narrowest owner.

- Use a dev shell for tools needed by one repository.
- Use a profile package for tools used across normal shells.
- Use Home Manager for user files only when `jconfig` does not own the same file.
- Use NixOS modules for machine state only on NixOS hosts.
- Use bootstrap scripts for pre-Nix setup and host discovery.

Do not let two owners mutate the same target file.

## Profiles

Treat `nix profile` as an installation surface, not as the source of truth.

Keep durable profile contents in `packages` outputs. Install profiles from those outputs.

Use profile installs to expose stable tools to normal shells. Keep experiments in dev shells until their interface is stable.

## Dev shells

Use `devShells.default` as the default interactive environment for this repo.

Add named dev shells when a narrower workflow has distinct dependencies.

Keep dev shells side-effect-free. Entering a dev shell must not modify tracked files, profile state, package caches inside the repo, or live dotfiles.

## Language dependencies

Nix provides runtimes and global CLIs.

Language-native tools own project dependency graphs:

- `uv` owns Python virtual environments and Python lockfiles.
- `npm` owns Node package manifests and lockfiles.
- Pi package sets live in Pi settings and npm package state.

Do not use global `pip install` or global `npm install -g` for tools managed by this repo.

## Packages

Prefer `nixpkgs` packages.

Use local package definitions when repo-specific wrappers or integration are required.

Add custom `buildNpmPackage` or equivalent derivations only for important tools missing from `nixpkgs` or requiring local patches.

## Reference patterns

Use `dustinlyons/nixos-config` for the practical flake shape:

- keep `flake.nix` as the public interface
- expose operational commands as `apps`
- expose normal-shell tools as installable `packages`
- group local overrides and patches under `overlays/` when overlays are required
- keep host, user, and platform concerns separated

Use `mightyiam/dendritic` for module decomposition:

- make each non-entry Nix module implement one feature
- let paths name features, not expression types
- share values through module configuration, not broad `specialArgs` pass-through
- use automatic or explicit imports consistently within each module tree
- prefer feature modules that can contribute to several lower-level configurations

## Modules

Prefer small modules with explicit imports.

Split by feature, ownership, and volatility:

- host
- user
- program
- toolchain
- service
- project shell

Use module systems when option merging is useful. Use plain packages and apps when a wrapper is enough.

## Overlays

Use overlays only when the package set itself must change.

Prefer these before overlays:

1. upstream `nixpkgs`
2. `callPackage` local packages
3. app wrappers
4. dev shell composition

Keep each overlay small, named, and documented at its definition site.

## Incrementality

Every Nix change needs a validation command and a rollback path.

Prefer this order:

1. dev shell
2. app wrapper
3. profile package
4. managed language lockfile
5. local package derivation
6. overlay
7. Home Manager or NixOS ownership

## References

Use these as primary references:

- Nix manual: flakes, `nix develop`, `nix profile`, store semantics, configuration, and command behavior
- nix.dev: official tutorials and guides for declarative shells, flakes, packages, and language workflows
- Nix Pills: foundational Nix language, derivation, and package construction material under `https://nixos.org/guides/nix-pills/`
- Nixpkgs manual: package functions, hooks, JavaScript tooling, Python tooling, overlays, and stdenv phases
- NixOS manual: system configuration, modules, options, services, and activation semantics
- NixOS Wiki: practical Nix, NixOS, flakes, Home Manager, language, and tool pages under `https://wiki.nixos.org/wiki/`
- Home Manager manual: user-level declarative configuration and file ownership
- `dustinlyons/nixos-config`: practical flake, app, overlay, host, and Home Manager organization
- `mightyiam/dendritic`: dendritic module decomposition and feature-oriented configuration parts

Follow community repository patterns deliberately. Adapt the pattern; do not copy host assumptions, secrets layout, package choices, or platform ownership boundaries.
