# jconfig Docker + Emacs Integration — Design Spec

**Date:** 2026-03-13
**Status:** Approved
**Branch:** `tmux-to-emacs`

---

## Overview

Add a Docker-based development environment to `jconfig` with deep Emacs integration. A single keybinding (`C-c a j`) sets up a reproducible, Emacs-version-matched Ubuntu container with a structured tab-bar workspace: README on the upper-left, a container terminal (vterm) on the lower-left, and an agent-shell on the right. Cron keeps the image in sync with the host Emacs. The design is forwards-compatible with multiple named containers mapped to git worktrees for multi-agent collaboration.

---

## Architecture

Five layers, each with a single clear responsibility:

| Layer | Responsibility | Location |
|---|---|---|
| **Dockerfile** | Ubuntu 24.04 + Emacs (source-built, version-matched to host) | `docker/Dockerfile.jconfig` |
| **Shell scripts** | Container lifecycle: build, start, stop, restart, attach | `scripts/jconfig-docker.sh` |
| **Cron** | Weekly image rebuild; daily container restart | `cron/jconfig-docker-weekly`, `cron/jconfig-docker-daily` |
| **Emacs packages** | vterm, tramp, tab-bar, docker.el — new `init.org` sections | `.emacs.d/init.org` |
| **jconfig workspace** | `C-c a j` orchestrator: ensures container, opens layout | new `** jconfig-workspace` section in `init.org` |

---

## 1. Docker Infrastructure

### 1.1 Dockerfile (`docker/Dockerfile.jconfig`)

Multi-stage build. No default values for `EMACS_MAJOR` or `EMACS_MINOR` — the build fails explicitly if either is absent.

**Stage 1 — builder:**
- Base: `ubuntu:24.04`
- Build args: `EMACS_MAJOR`, `EMACS_MINOR` (both required, no defaults; Dockerfile opens with an explicit `ARG` + `RUN test -n` guard for each)
- Installs build deps: `build-essential`, `libncurses-dev`, `libgnutls28-dev`, `libxml2-dev`, `pkg-config`, `wget`
- Downloads `https://ftp.gnu.org/gnu/emacs/emacs-${EMACS_MAJOR}.${EMACS_MINOR}.tar.gz`; fails with a clear error message if the URL 404s (use `wget --server-response` and check exit code)
- Configures with `--without-x --with-gnutls --with-xml2`; builds with `make -j$(nproc)`; installs to `/usr/local`

**Stage 2 — final:**
- Base: `ubuntu:24.04`
- Copies `/usr/local` from builder
- Installs runtime deps: `libncurses6`, `libgnutls30`, `libxml2`, `git`, `curl`, `bash`
- Installs vterm module build deps: `cmake`, `libtool-bin`, `gcc`
- Sets `LABEL jconfig.emacs="${EMACS_MAJOR}.${EMACS_MINOR}"`
- `WORKDIR /root`, `CMD ["/bin/bash"]`

### 1.2 Build Script (`scripts/jconfig-docker.sh`)

Subcommands: `build`, `start`, `stop`, `restart`, `attach`.

**Version detection (used by `build`):**
```bash
EMACS_FULL=$(emacs --version 2>/dev/null | head -1 | grep -oP '(?<=GNU Emacs )\d+\.\d+')
[[ -z "$EMACS_FULL" ]] && { echo "ERROR: cannot determine host Emacs version" >&2; exit 1; }
EMACS_MAJOR=${EMACS_FULL%%.*}
EMACS_MINOR=${EMACS_FULL#*.}
```

The regex anchors to `GNU Emacs ` prefix to avoid false matches on other numeric strings in the version output. Only `MAJOR.MINOR` is extracted (e.g. `30.2` from `GNU Emacs 30.2.1` is intentional — patch/pre-release segments are not part of the tarball name on ftp.gnu.org).

**`build`:** Passes `--build-arg EMACS_MAJOR` and `--build-arg EMACS_MINOR`. Tags image as both `jconfig:latest` and `jconfig:emacs${EMACS_MAJOR}.${EMACS_MINOR}`.

**Container naming:** Default name is `jconfig`. `--name <dash-separated-name>` produces `jconfig-<name>` (e.g. `--name feat-auth` → container `jconfig-feat-auth`). All subcommands accept the same `--name` flag.

**`start [--name <n>]`:**
```bash
docker run -d \
  --name <resolved-name> \
  --label jconfig.worktree=<resolved-name> \
  --label jconfig.branch=$(git -C ~/jconfig rev-parse --abbrev-ref HEAD) \
  --label jconfig.emacs=${EMACS_MAJOR}.${EMACS_MINOR} \
  -v ~/jconfig:/root/jconfig \
  jconfig:latest \
  sleep infinity
```

Mount point is `~/jconfig` on host → `/root/jconfig` in container (mirrors host path; `~/` resolves identically in both contexts). **Forward-compatibility note:** when multi-worktree support is added, the mount source (`~/jconfig`) will be parameterised to `~/jconfig-<name>` per container — see Section 3.4.

**`stop/restart/attach [--name <n>]`:** Thin wrappers around `docker stop`, `docker restart`, and `docker exec -it <name> bash`.

All running jconfig containers are queryable via:
```bash
docker ps --filter label=jconfig.worktree
```

### 1.3 Cron

Two separate files (one entry each, compatible with the existing `configure.sh` cron installer which processes one crontab line per file):

**`cron/jconfig-docker-daily`:**
```
@daily $HOME/jconfig/scripts/jconfig-docker.sh restart
```

**`cron/jconfig-docker-weekly`:**
```
@weekly $HOME/jconfig/scripts/jconfig-docker.sh build && $HOME/jconfig/scripts/jconfig-docker.sh restart
```

---

## 2. `configure.sh` Changes

**Emacs ≥ 30 pre-flight check** (added before tangle step):
1. Check `emacs --version` for major version ≥ 30
2. If not satisfied: warn the user, explain that `snap install emacs --classic` removes snap confinement (full system access, no sandboxing), and prompt:
   ```
   Install Emacs via 'snap install emacs --classic'? This grants the snap
   full system access. [y/N]:
   ```
3. Abort cleanly on non-`y` response; proceed with snap install on `y`

**Tangle step** (tangles only `bash-emacs-deps` blocks from `init.org`, using anchored language regex):
```bash
emacs --batch -l org \
  --eval "(org-babel-tangle-file \
    \"$JCONFIG_ROOT/.emacs.d/init.org\" \
    \"$JCONFIG_ROOT/.emacs.d/tangles/system-setup.sh\" \
    \"^bash-emacs-deps$\")"
```

The second argument (`TARGET-FILE`) routes all matched blocks to `system-setup.sh` regardless of any per-block `:tangle` header args. This is intentional — every `bash-emacs-deps` block is expected to contribute to this single file. **Constraint:** do not add a `:tangle` header arg to individual `bash-emacs-deps` blocks; rely on the file-level property and the CLI override instead.

**`.bashrc` sourcing:** Added via `load_custom_config` in `configure.sh` (same mechanism used for existing bash config blocks), not hardcoded into the tracked `bash/.bashrc`:
```bash
source ~/.emacs.d/tangles/system-setup.sh
```

---

## 3. `init.org` Changes

### 3.1 New `bash-emacs-deps` org-babel language (Bootstrap section)

Added to the Bootstrap section so it is available before any `bash-emacs-deps` block is evaluated. Requires `ob-shell` first:

```elisp
(require 'ob-shell)
(add-to-list 'org-babel-tangle-lang-exts '("bash-emacs-deps" . "sh"))
(defalias 'org-babel-execute:bash-emacs-deps 'org-babel-execute:shell)
```

`org-babel-tangle-lang-exts` is a `defcustom` alist. Using `add-to-list` after `require` is safe here because this runs at Emacs startup before any customization file could overwrite it; the entry is idempotent across sessions since `add-to-list` is a no-op if the pair already exists.

File-level property added at top of `init.org` (second `#+PROPERTY` line, after the existing `emacs-lisp` one):
```
#+PROPERTY: header-args:bash-emacs-deps :tangle ~/.emacs.d/tangles/system-setup.sh :shebang "#!/usr/bin/env bash"
```

The `:shebang` is set once here and must **not** be repeated in individual `bash-emacs-deps` blocks, or it will appear multiple times in the output file.

### 3.2 `** Utilities` — verbosity toggle

A `defcustom j/verbose nil` with `:type 'boolean` lives in the `** Utilities` section. `j/toggle-verbose` flips it and renames all live buffers matching `^\*jconfig` to reflect the new naming format. Bound globally to `C-S-v`.

> **Note:** `C-S-v` (`Control-Shift-v`) is unbound by default in GUI Emacs. In terminal Emacs it may be intercepted by the terminal emulator as a paste shortcut — verify in context and rebind if needed.

Buffer naming convention (controlled by `j/verbose`):

| Mode | Format | Example |
|---|---|---|
| normal | `*<container> <type>*` | `*jconfig vterm*` |
| verbose | `*<container>:<M>.<m> <type>*` | `*jconfig:30.1 vterm*` |

The version segment is retrieved from the container's `jconfig.emacs` label via `docker inspect`.

### 3.3 New `init.org` package sections

All added under `* Packages`, alphabetically ordered with existing sections.

#### `** tramp` (replaces dead `packages/j-tramp.el`)

```elisp
(use-package tramp
  :ensure nil  ; built-in
  :custom
  (tramp-default-method "ssh")   ; general default; /docker: paths override explicitly
  (tramp-terminal-type "dumb"))  ; POSIX-standard fallback; avoid "tramp" which has no terminfo entry and causes remote shell failures
```

No extra package. TRAMP's `/docker:` method is built into Emacs 28+ and uses `docker exec` under the hood — no SSH server needed in the container. `tramp-default-method "ssh"` only applies when no method is specified in the path; `/docker:name:/path` always uses the docker method regardless.

#### `** vterm` (replaces `** shell-pop` + `** eterm-256color`)

System dep block (`bash-emacs-deps`):
```bash
dpkg -l libvterm-dev &>/dev/null || sudo apt-get install -y libvterm-dev cmake libtool-bin
```

Package config:
```elisp
(use-package vterm
  :ensure t
  :custom
  (vterm-max-scrollback 10000)
  (vterm-shell "/bin/bash"))

(use-package vterm-toggle
  :ensure t
  :bind (("C-c t t" . vterm-toggle)
         ("C-c t n" . vterm-toggle-cd)))
```

`** shell-pop` and `** eterm-256color` blocks are replaced with a migration comment.

#### `** tab-bar`

```elisp
(use-package tab-bar
  :ensure nil  ; built-in since Emacs 27
  :custom
  (tab-bar-show t)
  (tab-bar-new-tab-choice "*scratch*")
  :config
  (tab-bar-mode 1))
```

Helper `j/tab-bar/switch-or-create (name)` — switches to the named tab if it exists, creates and names it otherwise.

#### `** docker`

```elisp
(use-package dockerfile-mode
  :ensure t)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
```

> **Note:** verify `C-c d` is free in your keymap at implementation time — it is commonly used for diff or dired bindings.

#### `** jconfig-workspace`

Contains the full `C-c a j` orchestration. Key defuns:

**`j/jconfig/container-emacs-version (container)`** — queries `jconfig.emacs` label via `docker inspect`; returns version string or `nil`.

**`j/jconfig/buffer-name (container type)`** — returns buffer name respecting `j/verbose`:
- verbose: `*<container>:<version> <type>*`
- normal: `*<container> <type>*`

**`j/jconfig/ensure-container (name)`** — checks `docker ps --filter name=^<name>$ --format {{.Names}}` for a running container. If absent, calls `scripts/jconfig-docker.sh start [--name <suffix>]`. Then polls `docker inspect <name> --format {{.State.Running}}` every 0.5s up to 15s; signals a user-visible error (`error "Timed out waiting for container %s"`) if the container does not reach running state within the timeout. This prevents a race condition where `j/jconfig/open-container-vterm` attempts a TRAMP `/docker:` open before `docker exec` is available.

**`j/jconfig/open-container-vterm (name)`** — opens a vterm buffer named via `j/jconfig/buffer-name` with `default-directory` set to `/docker:<name>:/root/jconfig`.

**`j/jconfig/setup-layout (name)`** — creates/switches to tab named `name` via `j/tab-bar/switch-or-create`, then:
1. `delete-other-windows`
2. `split-window-right` — RHS: agent-shell connected to container (Docker experimental API; see note below)
3. LHS `split-window-below`:
   - ULC: `(find-file "~/jconfig/README.md")` (host file)
   - BLC: `(j/jconfig/open-container-vterm name)`

**`j/jconfig/open (&optional name)`** — top-level interactive command:
1. Resolves container name: `jconfig` if name is empty, `jconfig-<name>` otherwise
2. Calls `j/jconfig/ensure-container`
3. Calls `j/jconfig/setup-layout`

**`j/jconfig/open-named ()`** — named defun for the prompted variant (not a lambda, required for `:bind`):
```elisp
(defun j/jconfig/open-named ()
  (interactive)
  (j/jconfig/open (read-string "Container name (leave blank for default): ")))
```

Keybindings:
```elisp
:bind (("C-c a j" . j/jconfig/open)
       ("C-c a J" . j/jconfig/open-named))
```

> **Implementation note:** agent-shell's Docker container integration is marked experimental upstream. The exact API (`agent-shell-start-in-container` or similar) must be verified against the installed package version at implementation time.

### 3.4 `** TODO` block in `init.org` — multi-agent roadmap

A `*** TODO Multi-agent jconfig workspaces` entry documents:

- **Naming convention:** `jconfig-<worktree>` maps 1:1 to a git worktree at `~/jconfig-<worktree>`; container name, tab-bar tab, and buffer prefix all share the same stem
- **Mount parameterisation needed:** `jconfig-docker.sh start` currently mounts `~/jconfig` for all containers; when multi-worktree support ships, the mount source must be parameterised to `~/jconfig-<name>` (or a supplied path) per container
- **`C-c a j <name>` already works** — orchestration extension is additive
- **`jconfig-docker.sh list`** — enumerate running jconfig containers and their associated branches/worktrees via label queries
- **Coordination model:** agents work on isolated branches/worktrees; PRs and issues on the shared host repo are the coordination mechanism; containers can `git fetch` via their respective `/root/jconfig` mounts
- **Tab-bar multi-workspace:** one tab per active `jconfig-*` container; `j/jconfig/setup-layout` is already parameterised by name

---

## 4. Keybinding Summary

| Binding | Command |
|---|---|
| `C-c a j` | `j/jconfig/open` (default container) |
| `C-c a J` | `j/jconfig/open-named` (prompts for name) |
| `C-c t t` | `vterm-toggle` (replaces `[f1]` shell-pop) |
| `C-c t n` | `vterm-toggle-cd` |
| `C-c d` | `docker` (docker.el transient menu — verify no conflict) |
| `C-S-v` | `j/toggle-verbose` (verify terminal emulator doesn't intercept) |

---

## 5. File Manifest

New files:
```
docker/Dockerfile.jconfig
scripts/jconfig-docker.sh
cron/jconfig-docker-daily
cron/jconfig-docker-weekly
docs/superpowers/specs/2026-03-13-docker-emacs-integration-design.md
```

Modified files:
```
.emacs.d/init.org   — new sections: tramp, vterm, tab-bar, docker, jconfig-workspace;
                      updated: Bootstrap (bash-emacs-deps registration),
                               Utilities (j/verbose), Tasks/TODO
configure.sh        — emacs>=30 pre-flight check + bash-emacs-deps tangle step
                      + load_custom_config for system-setup.sh sourcing
```

---

## 6. Out of Scope (this iteration)

- Multi-agent orchestration beyond naming/labelling conventions
- `jconfig-docker.sh list` command
- Per-worktree container automation (mount parameterisation)
- Non-Ubuntu host support
