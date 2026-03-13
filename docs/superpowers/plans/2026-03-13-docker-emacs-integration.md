# Docker + Emacs Integration Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a Docker-based jconfig development environment with deep Emacs integration behind a single `C-c a j` keybinding.

**Architecture:** Multi-stage Ubuntu 24.04 Docker image builds Emacs from source matching the host version exactly. `init.org` gains five new package sections (tramp, vterm, tab-bar, docker, jconfig-workspace) plus language registrations (ert, bash-emacs-deps) in Bootstrap and a verbosity toggle in Utilities. ERT tests live as `*** Test` subsections using `#+BEGIN_SRC ert` — a custom org-babel language with a dedicated tangle target (`tangles/tests/init-test.el`), following the same pattern as the agent-shell section. `configure.sh` gains an Emacs ≥30 pre-flight and a bash-emacs-deps tangle step.

**Tech Stack:** Docker (multi-stage build), bash, Emacs 30+ (org-babel `ert` + `bash-emacs-deps` languages, use-package, ERT), vterm, vterm-toggle, docker.el, TRAMP `/docker:` method.

**Spec:** `docs/superpowers/specs/2026-03-13-docker-emacs-integration-design.md`

---

## File Structure

**New files:**
| File | Responsibility |
|---|---|
| `docker/Dockerfile.jconfig` | Multi-stage build: compile Emacs from source, slim runtime image |
| `scripts/jconfig-docker.sh` | Container lifecycle: build, start, stop, restart, attach |
| `cron/jconfig-docker-daily` | Daily container restart cron entry (one line) |
| `cron/jconfig-docker-weekly` | Weekly image rebuild + restart cron entry (one line) |

**Modified files:**
| File | Change |
|---|---|
| `.emacs.d/init.org` | Top: add `#+PROPERTY` for `ert` and `bash-emacs-deps`; Bootstrap: register both languages; Utilities: `j/verbose` + Test subsection; Packages: tramp, vterm (replaces shell-pop), tab-bar + Test, docker, jconfig-workspace + Test; Tasks: TODOs |
| `configure.sh` | Emacs ≥30 pre-flight (with snap prompt) + bash-emacs-deps tangle step + system-setup.sh via `load_custom_config` |

No separate test file — all ERT tests tangle from `init.org` to `tangles/tests/init-test.el`.

---

## Chunk 1: Docker Infrastructure

### Task 1: Create `docker/Dockerfile.jconfig`

**Files:**
- Create: `docker/Dockerfile.jconfig`

- [ ] **Step 1: Create the docker directory and Dockerfile**

```bash
mkdir -p ~/jconfig/docker
```

Write `docker/Dockerfile.jconfig`:

```dockerfile
# docker/Dockerfile.jconfig
# Multi-stage build: compile Emacs from source to exactly match host version.
# Build with: scripts/jconfig-docker.sh build
#
# Both EMACS_MAJOR and EMACS_MINOR are required — no defaults.

# ── Stage 1: builder ─────────────────────────────────────────────────────────
FROM ubuntu:24.04 AS builder

ARG EMACS_MAJOR
ARG EMACS_MINOR

RUN test -n "${EMACS_MAJOR}" \
    || (echo "ERROR: EMACS_MAJOR build arg is required" >&2 && exit 1)
RUN test -n "${EMACS_MINOR}" \
    || (echo "ERROR: EMACS_MINOR build arg is required" >&2 && exit 1)

RUN apt-get update && apt-get install -y \
    build-essential \
    libncurses-dev \
    libgnutls28-dev \
    libxml2-dev \
    pkg-config \
    wget \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /tmp

RUN wget -q \
    "https://ftp.gnu.org/gnu/emacs/emacs-${EMACS_MAJOR}.${EMACS_MINOR}.tar.gz" \
    -O "emacs-${EMACS_MAJOR}.${EMACS_MINOR}.tar.gz" \
    || (echo "ERROR: Could not download Emacs ${EMACS_MAJOR}.${EMACS_MINOR} — check ftp.gnu.org for available versions" >&2 \
        && exit 1)

RUN tar xzf "emacs-${EMACS_MAJOR}.${EMACS_MINOR}.tar.gz"

WORKDIR /tmp/emacs-${EMACS_MAJOR}.${EMACS_MINOR}

RUN ./configure --without-x --with-gnutls --with-xml2 --prefix=/usr/local
RUN make -j$(nproc)
RUN make install

# ── Stage 2: runtime ─────────────────────────────────────────────────────────
FROM ubuntu:24.04

ARG EMACS_MAJOR
ARG EMACS_MINOR

RUN apt-get update && apt-get install -y \
    libncurses6 \
    libgnutls30 \
    libxml2 \
    git \
    curl \
    bash \
    cmake \
    libtool-bin \
    gcc \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /usr/local /usr/local

LABEL jconfig.emacs="${EMACS_MAJOR}.${EMACS_MINOR}"

WORKDIR /root
CMD ["/bin/bash"]
```

- [ ] **Step 2: Verify missing-arg guard fires**

```bash
docker build --build-arg EMACS_MAJOR= --build-arg EMACS_MINOR=1 \
  -f ~/jconfig/docker/Dockerfile.jconfig ~/jconfig/docker/ 2>&1 \
  | grep -q "ERROR: EMACS_MAJOR" && echo "PASS: missing arg guard" || echo "FAIL"
```

Expected: `PASS: missing arg guard`

- [ ] **Step 3: Commit**

```bash
cd ~/jconfig
git add docker/Dockerfile.jconfig
git commit -m "feat(docker): add multi-stage Dockerfile for jconfig container"
```

---

### Task 2: Create `scripts/jconfig-docker.sh`

**Files:**
- Create: `scripts/jconfig-docker.sh`

- [ ] **Step 1: Validate version-detection regex before writing the script**

```bash
result=$(echo "GNU Emacs 30.1"   | grep -oP '(?<=GNU Emacs )\d+\.\d+')
[[ "$result" == "30.1" ]] || { echo "FAIL 30.1: got '$result'"; exit 1; }

result=$(echo "GNU Emacs 30.2.1" | grep -oP '(?<=GNU Emacs )\d+\.\d+')
[[ "$result" == "30.2" ]] || { echo "FAIL 30.2.1: got '$result'"; exit 1; }

result=$(echo "something 1.2"    | grep -oP '(?<=GNU Emacs )\d+\.\d+')
[[ -z "$result" ]] || { echo "FAIL no-match: got '$result'"; exit 1; }

echo "PASS: version regex"
```

Expected: `PASS: version regex`

- [ ] **Step 2: Write the script**

```bash
#!/usr/bin/env bash
# scripts/jconfig-docker.sh — jconfig Docker container lifecycle manager
#
# Usage: jconfig-docker.sh <command> [--name <suffix>]
#
# Commands: build | start | stop | restart | attach
#
# Container naming:
#   (no --name)       →  container: jconfig
#   --name feat-auth  →  container: jconfig-feat-auth

set -euo pipefail

JCONFIG_ROOT="${JCONFIG_ROOT:-$HOME/jconfig}"
DOCKER_DIR="$JCONFIG_ROOT/docker"
DEFAULT_CONTAINER="jconfig"

# ── Helpers ──────────────────────────────────────────────────────────────────

usage() {
    cat <<EOF
Usage: $(basename "$0") <command> [--name <suffix>]

Commands:
  build               Build jconfig image (detects host Emacs version)
  start [--name <n>]  Start container  (default: jconfig; named: jconfig-<n>)
  stop  [--name <n>]  Stop and remove container
  restart [--name <n>] Stop then start container
  attach [--name <n>] Attach interactive bash shell to running container

Options:
  --name <n>  Container suffix — container will be named jconfig-<n>.
EOF
}

resolve_name() {
    local suffix="${1:-}"
    if [[ -z "$suffix" ]]; then
        echo "$DEFAULT_CONTAINER"
    else
        echo "${DEFAULT_CONTAINER}-${suffix}"
    fi
}

detect_emacs_version() {
    local full
    full=$(emacs --version 2>/dev/null \
           | head -1 \
           | grep -oP '(?<=GNU Emacs )\d+\.\d+' \
           || true)
    if [[ -z "$full" ]]; then
        echo "ERROR: cannot determine host Emacs version — is emacs in PATH?" >&2
        exit 1
    fi
    echo "$full"
}

# ── Commands ─────────────────────────────────────────────────────────────────

cmd_build() {
    local emacs_full emacs_major emacs_minor
    emacs_full=$(detect_emacs_version)
    emacs_major="${emacs_full%%.*}"
    emacs_minor="${emacs_full#*.}"

    echo "Building jconfig image for Emacs ${emacs_major}.${emacs_minor}..."
    docker build \
        --build-arg "EMACS_MAJOR=${emacs_major}" \
        --build-arg "EMACS_MINOR=${emacs_minor}" \
        -t "jconfig:latest" \
        -t "jconfig:emacs${emacs_major}.${emacs_minor}" \
        -f "$DOCKER_DIR/Dockerfile.jconfig" \
        "$DOCKER_DIR"
    echo "Build complete: jconfig:latest (emacs ${emacs_major}.${emacs_minor})"
}

cmd_start() {
    local suffix="${1:-}" name branch emacs_full emacs_major emacs_minor
    name=$(resolve_name "$suffix")

    if docker ps -q --filter "name=^${name}$" | grep -q .; then
        echo "Container '${name}' is already running."
        return 0
    fi

    branch=$(git -C "$JCONFIG_ROOT" rev-parse --abbrev-ref HEAD 2>/dev/null \
             || echo "unknown")
    emacs_full=$(detect_emacs_version)
    emacs_major="${emacs_full%%.*}"
    emacs_minor="${emacs_full#*.}"

    echo "Starting container '${name}'..."
    docker run -d \
        --name "$name" \
        --label "jconfig.worktree=${name}" \
        --label "jconfig.branch=${branch}" \
        --label "jconfig.emacs=${emacs_major}.${emacs_minor}" \
        -v "${JCONFIG_ROOT}:/root/jconfig" \
        jconfig:latest \
        sleep infinity
    echo "Container '${name}' started."
}

cmd_stop() {
    local suffix="${1:-}" name
    name=$(resolve_name "$suffix")
    echo "Stopping container '${name}'..."
    docker stop "$name" 2>/dev/null || true
    docker rm   "$name" 2>/dev/null || true
    echo "Container '${name}' stopped and removed."
}

cmd_restart() {
    local suffix="${1:-}"
    cmd_stop  "$suffix"
    cmd_start "$suffix"
}

cmd_attach() {
    local suffix="${1:-}" name
    name=$(resolve_name "$suffix")
    docker exec -it "$name" bash
}

# ── Argument parsing ──────────────────────────────────────────────────────────

COMMAND="${1:-}"
shift || true

SUFFIX=""
while [[ $# -gt 0 ]]; do
    case "$1" in
        --name)
            SUFFIX="${2:-}"
            shift 2
            ;;
        -h|--help|help)
            usage; exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            usage >&2; exit 1
            ;;
    esac
done

case "$COMMAND" in
    build)   cmd_build ;;
    start)   cmd_start   "$SUFFIX" ;;
    stop)    cmd_stop    "$SUFFIX" ;;
    restart) cmd_restart "$SUFFIX" ;;
    attach)  cmd_attach  "$SUFFIX" ;;
    help|-h|--help) usage ;;
    *)
        echo "Unknown command: '${COMMAND}'" >&2
        usage >&2; exit 1
        ;;
esac
```

Run: `chmod +x ~/jconfig/scripts/jconfig-docker.sh`

- [ ] **Step 3: Test argument parsing**

```bash
# Help works
~/jconfig/scripts/jconfig-docker.sh help | grep -q "build" && echo "PASS: help"

# Unknown command errors cleanly
~/jconfig/scripts/jconfig-docker.sh badcmd 2>&1 | grep -q "Unknown command" \
  && echo "PASS: unknown command" || echo "FAIL"

# resolve_name: default
bash -c 'source ~/jconfig/scripts/jconfig-docker.sh 2>/dev/null
         [[ "$(resolve_name "")" == "jconfig" ]] && echo "PASS: default name"'

# resolve_name: suffixed
bash -c 'source ~/jconfig/scripts/jconfig-docker.sh 2>/dev/null
         [[ "$(resolve_name "feat-auth")" == "jconfig-feat-auth" ]] \
           && echo "PASS: suffixed name"'
```

Expected: four `PASS` lines.

- [ ] **Step 4: Commit**

```bash
cd ~/jconfig
git add scripts/jconfig-docker.sh
git commit -m "feat(docker): add jconfig-docker.sh container lifecycle script"
```

---

## Chunk 2: Cron + configure.sh

### Task 3: Create cron files

**Files:**
- Create: `cron/jconfig-docker-daily`
- Create: `cron/jconfig-docker-weekly`

The existing cron installer in `configure.sh` reads each file and appends its full content as a single crontab line. **One entry per file** — no trailing newlines.

- [ ] **Step 1: Create `cron/jconfig-docker-daily`**

File content (no trailing newline):
```
@daily $HOME/jconfig/scripts/jconfig-docker.sh restart
```

- [ ] **Step 2: Create `cron/jconfig-docker-weekly`**

File content (no trailing newline):
```
@weekly $HOME/jconfig/scripts/jconfig-docker.sh build && $HOME/jconfig/scripts/jconfig-docker.sh restart
```

- [ ] **Step 3: Verify single-line format**

```bash
wc -l ~/jconfig/cron/jconfig-docker-daily  | grep -q "^1 " && echo "PASS: daily"
wc -l ~/jconfig/cron/jconfig-docker-weekly | grep -q "^1 " && echo "PASS: weekly"
```

Expected: `PASS: daily`, `PASS: weekly`

- [ ] **Step 4: Commit**

```bash
cd ~/jconfig
git add cron/jconfig-docker-daily cron/jconfig-docker-weekly
git commit -m "feat(cron): add daily restart and weekly rebuild cron entries"
```

---

### Task 4: Update `configure.sh`

**Files:**
- Modify: `configure.sh`

Three additions, all in the `#=== Emacs ===` section (around line 105).

- [ ] **Step 1: Add Emacs ≥30 pre-flight + bash-emacs-deps tangle step**

Find:
```bash
echo "I now auto-generate my ~/.emacs file using org-babel; open ~/jconfig/.emacs.d/init.org and tangle the Bootstrap Process header."
```

Insert immediately after it:

```bash
#--- Emacs version check -------------------------------------------------------
EMACS_MAJOR_VERSION=$(emacs --version 2>/dev/null \
    | head -1 \
    | grep -oP '(?<=GNU Emacs )\d+' \
    || echo "0")

if [[ "$EMACS_MAJOR_VERSION" -lt 30 ]]; then
    echo -e "WARNING: Emacs ${EMACS_MAJOR_VERSION} detected (need ≥30).\n"
    echo "This config requires Emacs 30+. snap install uses --classic mode,"
    echo "which grants the snap full system access (no confinement)."
    echo ""
    read -p "Install Emacs via 'snap install emacs --classic'? [y/N]: " snap_answer
    if [[ "${snap_answer,,}" == "y" ]]; then
        snap install emacs --classic
        echo -e "Emacs installed via snap.\n"
    else
        echo -e "Skipping snap install. Some features require Emacs 30+.\n"
    fi
fi

#--- Tangle bash-emacs-deps blocks from init.org --------------------------------
SYSTEM_SETUP_SH="$JCONFIG_ROOT/.emacs.d/tangles/system-setup.sh"
mkdir -p "$(dirname "$SYSTEM_SETUP_SH")"

echo -e "Tangling bash-emacs-deps blocks from init.org...\n"
emacs --batch -l org \
    --eval "(require 'ob-shell)" \
    --eval "(add-to-list 'org-babel-tangle-lang-exts '(\"bash-emacs-deps\" . \"sh\"))" \
    --eval "(org-babel-tangle-file \
              \"$JCONFIG_ROOT/.emacs.d/init.org\" \
              \"$SYSTEM_SETUP_SH\" \
              \"^bash-emacs-deps\$\")"
echo -e "Tangled: $SYSTEM_SETUP_SH\n"
```

- [ ] **Step 2: Add system-setup.sh sourcing to the bash config block**

Find the existing `BASH_CONF` heredoc (around line 96):

```bash
read -r -d '' BASH_CONF <<EOF
#=== Custom global configurations ==============================================

#=== Hook for pulling in my dotfiles ===========================================
source $JCONFIG_ROOT/bash/.bashrc
EOF
```

Replace with:

```bash
read -r -d '' BASH_CONF <<EOF
#=== Custom global configurations ==============================================

#=== Hook for pulling in my dotfiles ===========================================
source $JCONFIG_ROOT/bash/.bashrc

#=== System-level Emacs dependencies (tangled from init.org) ===================
if [[ -f "$HOME/.emacs.d/tangles/system-setup.sh" ]]; then
    source "$HOME/.emacs.d/tangles/system-setup.sh"
fi
EOF
```

- [ ] **Step 3: Verify bash syntax**

```bash
bash -n ~/jconfig/configure.sh && echo "PASS: syntax OK"
```

Expected: `PASS: syntax OK`

- [ ] **Step 4: Commit**

```bash
cd ~/jconfig
git add configure.sh
git commit -m "feat(configure): emacs>=30 preflight, bash-emacs-deps tangle, system-setup sourcing"
```

---

## Chunk 3: init.org — Foundation

### Task 5: Register `ert` and `bash-emacs-deps` languages

**Files:**
- Modify: `.emacs.d/init.org` (top-of-file properties + Bootstrap section)

Both languages use the same registration pattern as each other. `ert` follows the pattern established in the agent-skill-management section.

- [ ] **Step 1: Add `#+PROPERTY` lines at top of init.org**

The current top of `init.org` is:
```org
#+PROPERTY: header-args:emacs-lisp :tangle (concat (expand-file-name "~/jconfig/.emacs.d/tangles/") (file-name-base) ".el")
#+PROPERTY: header-args :mkdirp yes :comments no
```

Replace with:
```org
#+PROPERTY: header-args:emacs-lisp :tangle (concat (expand-file-name "~/jconfig/.emacs.d/tangles/") (file-name-base) ".el")
#+PROPERTY: header-args:ert :tangle (concat (expand-file-name "~/jconfig/.emacs.d/tangles/tests/") (file-name-base) "-test.el")
#+PROPERTY: header-args:bash-emacs-deps :tangle ~/.emacs.d/tangles/system-setup.sh :shebang "#!/usr/bin/env bash"
#+PROPERTY: header-args :mkdirp yes :comments no
```

Notes:
- `:shebang` on `bash-emacs-deps` appears once here — do **not** repeat it in individual blocks.
- `:mkdirp yes` (from the global property) ensures `tangles/tests/` is created automatically.

- [ ] **Step 2: Add language registrations to Bootstrap > Package Management**

Find the block ending `(require 'bind-key)\n(require 'org)` in the Package Management subsection and add after `(require 'org)`:

```org
** Custom org-babel Languages

Register ~ert~ (ERT test blocks) and ~bash-emacs-deps~ (system-dependency
blocks) as org-babel languages. Both tangle to dedicated targets set by the
file-level ~#+PROPERTY~ lines above.

- ~ert~ blocks tangle to ~tangles/tests/init-test.el~; run via ~emacs --batch~.
- ~bash-emacs-deps~ blocks tangle to ~tangles/system-setup.sh~; sourced by
  ~~/.bashrc~. ~configure.sh~ re-tangles them independently using
  ~(org-babel-tangle-file ... nil \"^bash-emacs-deps$\")~.

#+BEGIN_SRC emacs-lisp :tangle (expand-file-name "~/.emacs")
;; Register ert and bash-emacs-deps as tangle-able org-babel languages.
(add-to-list 'org-babel-tangle-lang-exts '("ert" . "el"))
(require 'ob-shell)
(add-to-list 'org-babel-tangle-lang-exts '("bash-emacs-deps" . "sh"))
(defalias 'org-babel-execute:bash-emacs-deps 'org-babel-execute:shell)
#+END_SRC
```

- [ ] **Step 3: Tangle ert blocks and verify output directory is created**

```bash
emacs --batch -l org \
  --eval "(progn \
    (add-to-list 'org-babel-tangle-lang-exts '(\"ert\" . \"el\")) \
    (org-babel-tangle-file \
      \"/home/jhaag/jconfig/.emacs.d/init.org\" nil \"ert\"))"
ls ~/jconfig/.emacs.d/tangles/tests/ 2>/dev/null && echo "PASS: tests dir exists" \
  || echo "NOTE: no ert blocks yet — dir will appear after Task 6"
```

- [ ] **Step 4: Commit**

```bash
cd ~/jconfig
git add .emacs.d/init.org
git commit -m "feat(emacs): register ert and bash-emacs-deps org-babel languages in Bootstrap"
```

---

### Task 6: Add `j/verbose` to Utilities with Test subsection

**Files:**
- Modify: `.emacs.d/init.org` (Utilities section, after existing `eval-and-compile` block)

- [ ] **Step 1: Insert Verbosity subsection into init.org**

In the `* Utilities` section, after the existing `#+END_SRC` of the `eval-and-compile` block, add:

```org
** Verbosity

~j/verbose~ controls whether buffer names include extra detail (Emacs version
from the Docker container label). Toggle with ~C-S-v~.

~C-S-v~ is unbound in GUI Emacs by default. In terminal emulators that
intercept ~C-S-v~ as paste, rebind as needed.

~j/toggle-verbose~ defers buffer rename to ~j/jconfig/buffer-name~ (defined in
[[*jconfig-workspace][jconfig-workspace]]); rename is silently skipped if that function is not yet
loaded.

#+BEGIN_SRC emacs-lisp
(defcustom j/verbose nil
  "When non-nil, enable verbose buffer naming (includes Emacs version info).
Toggle with `j/toggle-verbose' (\\[j/toggle-verbose])."
  :type 'boolean
  :group 'convenience)

(defun j/toggle-verbose ()
  "Toggle `j/verbose' and rename any live jconfig buffers to match.
Buffer rename requires `j/jconfig/buffer-name' (jconfig-workspace section)."
  (interactive)
  (setq j/verbose (not j/verbose))
  (when (fboundp 'j/jconfig/buffer-name)
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf)))
        (when (string-match
               "^\\*\\(jconfig[^: *]*\\)\\(?::[^ *]*\\)? \\([^*]+\\)\\*$"
               name)
          (let* ((container (match-string 1 name))
                 (type      (match-string 2 name))
                 (new-name  (j/jconfig/buffer-name container type)))
            (unless (string= name new-name)
              (with-current-buffer buf
                (rename-buffer new-name t))))))))
  (message "j/verbose: %s" (if j/verbose "on" "off")))

(global-set-key (kbd "C-S-v") #'j/toggle-verbose)
#+END_SRC

*** Test
:PROPERTIES:
:VISIBILITY: folded
:END:

#+BEGIN_SRC ert
;;; Tests for j/verbose and j/toggle-verbose -*- lexical-binding: t -*-

(require 'ert)

(ert-deftest j/verbose-default-nil ()
  "j/verbose defaults to nil."
  (should (eq j/verbose nil)))

(ert-deftest j/toggle-verbose-flips-value ()
  "j/toggle-verbose toggles j/verbose between nil and t."
  (let ((j/verbose nil))
    (j/toggle-verbose)
    (should (eq j/verbose t))
    (j/toggle-verbose)
    (should (eq j/verbose nil))))
#+END_SRC
```

- [ ] **Step 2: Tangle ert blocks and run tests**

```bash
emacs --batch -l org \
  --eval "(progn \
    (add-to-list 'org-babel-tangle-lang-exts '(\"ert\" . \"el\")) \
    (org-babel-tangle-file \
      \"/home/jhaag/jconfig/.emacs.d/init.org\" nil \"ert\"))"

emacs --batch \
  -l /home/jhaag/jconfig/.emacs.d/tangles/tests/init-test.el \
  --eval "(ert-run-tests-batch-and-exit)"
```

Expected:
```
Running 2 tests (2026-03-13 ...)
   passed  1/2  j/verbose-default-nil
   passed  2/2  j/toggle-verbose-flips-value

Ran 2 tests, 2 results as expected
```

- [ ] **Step 3: Commit**

```bash
cd ~/jconfig
git add .emacs.d/init.org
git commit -m "feat(emacs): add j/verbose defcustom and j/toggle-verbose (C-S-v)"
```

---

## Chunk 4: init.org — Package Sections

### Task 7: Add `** tramp` section

**Files:**
- Modify: `.emacs.d/init.org` — insert between `** symon` and `** undo-tree` (alphabetical: t < u)

- [ ] **Step 1: Insert tramp section**

Find `** undo-tree` and insert before it:

```org
** tramp

TRAMP provides transparent access to remote files and shells. Configured here
to support the ~/docker:~ method used by ~jconfig-workspace~ for connecting to
containers via ~docker exec~ — no SSH server required in the container.

~j-tramp.el~ in ~packages/~ is dead code (never loaded); this section is the
authoritative tramp configuration.

#+BEGIN_SRC emacs-lisp
(use-package tramp
  :ensure nil  ; built-in
  :custom
  (tramp-default-method "ssh")   ; general default; /docker: paths override explicitly
  (tramp-terminal-type "dumb"))  ; POSIX-standard fallback; avoid "tramp" which has
                                 ; no terminfo entry and causes remote shell failures
#+END_SRC

```

- [ ] **Step 2: Tangle and verify**

```bash
emacs --batch -l org \
  --eval "(org-babel-tangle-file \"/home/jhaag/jconfig/.emacs.d/init.org\")" \
  2>&1 | tail -3
```

Expected: no errors.

- [ ] **Step 3: Commit**

```bash
cd ~/jconfig
git add .emacs.d/init.org
git commit -m "feat(emacs): add tramp section (replaces dead j-tramp.el)"
```

---

### Task 8: Replace `** shell-pop` with `** vterm`

**Files:**
- Modify: `.emacs.d/init.org` — replace the entire `** shell-pop` section

- [ ] **Step 1: Replace shell-pop section**

Find `** shell-pop` and replace the entire section (from that heading to the final `#+END_SRC` before `** smooth-scrolling`) with:

```org
** vterm

~vterm~ is a fast, full-featured terminal emulator using libvterm, replacing
the previous ~shell-pop~ + ~ansi-term~ setup. ~vterm-toggle~ provides the same
pop-up terminal behavior, rebound from ~<f1>~ to ~C-c t t~.

See [[*jconfig-workspace][jconfig-workspace]] for container-attached vterm usage via TRAMP ~/docker:~.

System dependency — tangled to ~system-setup.sh~ by ~configure.sh~:

#+BEGIN_SRC bash-emacs-deps
# vterm: ensure libvterm and build tools are present for module compilation
dpkg -l libvterm-dev &>/dev/null \
  || sudo apt-get install -y libvterm-dev cmake libtool-bin
#+END_SRC

#+BEGIN_SRC emacs-lisp
(use-package vterm
  :ensure t
  :custom
  (vterm-max-scrollback 10000)
  (vterm-shell "/bin/bash"))
#+END_SRC

#+BEGIN_SRC emacs-lisp
(use-package vterm-toggle
  :ensure t
  :bind (("C-c t t" . vterm-toggle)
         ("C-c t n" . vterm-toggle-cd)))
#+END_SRC
```

- [ ] **Step 2: Tangle bash-emacs-deps and verify libvterm entry**

```bash
emacs --batch -l org \
  --eval "(progn \
    (require 'ob-shell) \
    (add-to-list 'org-babel-tangle-lang-exts '(\"bash-emacs-deps\" . \"sh\")) \
    (org-babel-tangle-file \
      \"/home/jhaag/jconfig/.emacs.d/init.org\" \
      \"/home/jhaag/jconfig/.emacs.d/tangles/system-setup.sh\" \
      \"^bash-emacs-deps$\"))"

grep -q "libvterm-dev" ~/jconfig/.emacs.d/tangles/system-setup.sh \
  && echo "PASS: libvterm-dev in system-setup.sh" || echo "FAIL"
```

Expected: `PASS: libvterm-dev in system-setup.sh`

- [ ] **Step 3: Commit**

```bash
cd ~/jconfig
git add .emacs.d/init.org
git commit -m "feat(emacs): replace shell-pop/ansi-term with vterm + vterm-toggle (C-c t t/n)"
```

---

### Task 9: Add `** tab-bar` section with Test subsection

**Files:**
- Modify: `.emacs.d/init.org` — insert between `** symon` and `** tramp` (alphabetical: tab-bar < tramp < undo-tree)

- [ ] **Step 1: Insert tab-bar section**

Find `** tramp` (just added) and insert before it:

```org
** tab-bar

~tab-bar-mode~ is built into Emacs 27+ and provides named tabs for managing
window configurations. Used by ~jconfig-workspace~ to create one workspace tab
per container.

#+BEGIN_SRC emacs-lisp
(use-package tab-bar
  :ensure nil  ; built-in since Emacs 27
  :custom
  (tab-bar-show t)
  (tab-bar-new-tab-choice "*scratch*")
  :config
  (tab-bar-mode 1))

(defun j/tab-bar/switch-or-create (name)
  "Switch to tab named NAME, or create and name it if absent."
  (let ((existing (seq-find (lambda (tab)
                              (string= (alist-get 'name tab) name))
                            (tab-bar-tabs))))
    (if existing
        (tab-bar-switch-to-tab name)
      (tab-bar-new-tab)
      (tab-bar-rename-tab name))))
#+END_SRC

*** Test
:PROPERTIES:
:VISIBILITY: folded
:END:

#+BEGIN_SRC ert
;;; Tests for tab-bar helpers -*- lexical-binding: t -*-

(require 'ert)

(ert-deftest j/tab-bar/switch-or-create-defined ()
  "j/tab-bar/switch-or-create is defined after config loads."
  (should (fboundp 'j/tab-bar/switch-or-create)))
#+END_SRC

```

- [ ] **Step 2: Tangle ert and run tests**

```bash
emacs --batch -l org \
  --eval "(progn \
    (add-to-list 'org-babel-tangle-lang-exts '(\"ert\" . \"el\")) \
    (org-babel-tangle-file \
      \"/home/jhaag/jconfig/.emacs.d/init.org\" nil \"ert\"))"

emacs --batch \
  -l /home/jhaag/jconfig/.emacs.d/tangles/tests/init-test.el \
  --eval "(ert-run-tests-batch-and-exit)"
```

Expected: all previous tests still pass + `j/tab-bar/switch-or-create-defined` passes.

- [ ] **Step 3: Commit**

```bash
cd ~/jconfig
git add .emacs.d/init.org
git commit -m "feat(emacs): add tab-bar section with j/tab-bar/switch-or-create"
```

---

### Task 10: Add `** docker` section

**Files:**
- Modify: `.emacs.d/init.org` — insert between `** buffer-move` and `** exec-path-from-shell` (alphabetical: b < d < e)

- [ ] **Step 1: Insert docker section**

Find `** exec-path-from-shell` and insert before it:

```org
** docker

~dockerfile-mode~ provides syntax highlighting for ~Dockerfile~ files.
~docker.el~ provides a transient-based GUI for managing Docker containers,
images, and volumes.

Note: ~C-c d~ — verify this is free in your keymap at implementation time
(commonly used for diff or dired bindings).

#+BEGIN_SRC emacs-lisp
(use-package dockerfile-mode
  :ensure t)
#+END_SRC

#+BEGIN_SRC emacs-lisp
;; C-c d: verify binding is free before relying on it
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
#+END_SRC

```

- [ ] **Step 2: Tangle and verify no errors**

```bash
emacs --batch -l org \
  --eval "(org-babel-tangle-file \"/home/jhaag/jconfig/.emacs.d/init.org\")" \
  2>&1 | tail -3
```

Expected: no errors.

- [ ] **Step 3: Commit**

```bash
cd ~/jconfig
git add .emacs.d/init.org
git commit -m "feat(emacs): add docker section (dockerfile-mode + docker.el C-c d)"
```

---

## Chunk 5: init.org — jconfig-workspace

### Task 11: Add `** jconfig-workspace` with Test subsection

**Files:**
- Modify: `.emacs.d/init.org` — insert between `** helm` (all subsections) and `** Language-Specific` (alphabetical: h < j < L)

- [ ] **Step 1: Insert jconfig-workspace section**

Find `** Language-Specific` and insert before it:

```org
** jconfig-workspace

Implements ~C-c a j~ for managing and attaching to jconfig Docker workspaces.
Creates a structured ~tab-bar~ layout per container:

- *Upper-left:* ~~/jconfig/README.md~ (host file)
- *Lower-left:* ~vterm~ attached to the container via TRAMP ~/docker:~
- *Right:* ~agent-shell~ connected to the container (experimental Docker API)

Depends on: [[*docker][docker]], [[*tab-bar][tab-bar]], [[*vterm][vterm]], [[*tramp][tramp]].

*** Container utilities

#+BEGIN_SRC emacs-lisp
(defun j/jconfig/container-name (&optional suffix)
  "Return the full Docker container name for optional SUFFIX.
Plain 'jconfig' if SUFFIX is nil or empty; 'jconfig-SUFFIX' otherwise."
  (if (or (null suffix) (string-empty-p (or suffix "")))
      "jconfig"
    (format "jconfig-%s" suffix)))

(defun j/jconfig/container-emacs-version (container)
  "Return the jconfig.emacs label of CONTAINER, or nil if unavailable."
  (let ((result
         (string-trim
           (shell-command-to-string
             (format "docker inspect %s --format \
'{{index .Config.Labels \"jconfig.emacs\"}}' 2>/dev/null"
                     (shell-quote-argument container))))))
    (if (string-empty-p result) nil result)))

(defun j/jconfig/buffer-name (container type)
  "Return a buffer name for CONTAINER and TYPE, respecting `j/verbose'.
Normal:  *jconfig vterm*
Verbose: *jconfig:30.1 vterm*  (version from jconfig.emacs Docker label)"
  (if j/verbose
      (let ((version (j/jconfig/container-emacs-version container)))
        (if version
            (format "*%s:%s %s*" container version type)
          (format "*%s %s*" container type)))
    (format "*%s %s*" container type)))
#+END_SRC

*** Container lifecycle

#+BEGIN_SRC emacs-lisp
(defun j/jconfig/container-running-p (name)
  "Return non-nil if Docker container NAME is running."
  (not (string-empty-p
         (string-trim
           (shell-command-to-string
             (format "docker ps --filter 'name=^%s$' --format '{{.Names}}' \
2>/dev/null"
                     (shell-quote-argument name)))))))

(defun j/jconfig/ensure-container (name)
  "Ensure Docker container NAME is running, starting it if necessary.
Polls every 0.5s up to 15s. Signals an error on timeout."
  (unless (j/jconfig/container-running-p name)
    (message "Starting jconfig container '%s'..." name)
    (let* ((suffix (if (string= name "jconfig") ""
                     (substring name (length "jconfig-"))))
           (args   (if (string-empty-p suffix) "start"
                     (format "start --name %s" suffix)))
           (script (expand-file-name "~/jconfig/scripts/jconfig-docker.sh")))
      (shell-command (format "%s %s"
                             (shell-quote-argument script) args))))
  (let ((deadline (+ (float-time) 15))
        (ready nil))
    (while (and (not ready) (< (float-time) deadline))
      (if (j/jconfig/container-running-p name)
          (setq ready t)
        (sleep-for 0.5)))
    (unless ready
      (error "Timed out waiting for jconfig container '%s'" name))))
#+END_SRC

*** Layout

#+BEGIN_SRC emacs-lisp
(defun j/jconfig/open-container-vterm (name)
  "Open a vterm buffer attached to container NAME via TRAMP /docker:."
  (let* ((buf-name (j/jconfig/buffer-name name "vterm"))
         (default-directory (format "/docker:%s:/root/jconfig" name)))
    (if (get-buffer buf-name)
        (switch-to-buffer buf-name)
      (vterm buf-name))))

(defun j/jconfig/setup-layout (name)
  "Set up the jconfig tab-bar workspace for container NAME.
ULC = README.md (host), BLC = vterm (container), RHS = agent-shell."
  (j/tab-bar/switch-or-create name)
  (delete-other-windows)
  (let ((left-window  (selected-window))
        (right-window (split-window-right)))
    ;; Right: agent-shell (Docker experimental — verify API against installed version)
    (select-window right-window)
    (if (fboundp 'agent-shell-start-in-container)
        (agent-shell-start-in-container name)
      (message "agent-shell Docker API unavailable — see ** TODO in Tasks"))
    ;; Left top: README.md on host
    (select-window left-window)
    (find-file (expand-file-name "~/jconfig/README.md"))
    ;; Left bottom: vterm attached to container
    (split-window-below)
    (other-window 1)
    (j/jconfig/open-container-vterm name)))
#+END_SRC

*** Entry point

#+BEGIN_SRC emacs-lisp
(defun j/jconfig/open (&optional suffix)
  "Open the jconfig workspace for container with optional SUFFIX.
SUFFIX nil/empty → container 'jconfig'.
SUFFIX \"feat-auth\" → container 'jconfig-feat-auth'."
  (interactive)
  (let ((name (j/jconfig/container-name suffix)))
    (j/jconfig/ensure-container name)
    (j/jconfig/setup-layout name)))

(defun j/jconfig/open-named ()
  "Prompt for a container name suffix and open the jconfig workspace.
Named defun (not lambda) required for use-package :bind."
  (interactive)
  (j/jconfig/open
   (read-string "Container suffix (blank for default 'jconfig'): ")))

(bind-key "C-c a j" #'j/jconfig/open)
(bind-key "C-c a J" #'j/jconfig/open-named)
#+END_SRC

*** Test
:PROPERTIES:
:VISIBILITY: folded
:END:

#+BEGIN_SRC ert
;;; Tests for jconfig-workspace pure functions -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)

(ert-deftest j/jconfig/container-name-default ()
  "No suffix yields plain 'jconfig'."
  (should (string= (j/jconfig/container-name)    "jconfig"))
  (should (string= (j/jconfig/container-name nil) "jconfig"))
  (should (string= (j/jconfig/container-name "")  "jconfig")))

(ert-deftest j/jconfig/container-name-with-suffix ()
  "A suffix produces 'jconfig-<suffix>'."
  (should (string= (j/jconfig/container-name "feat-auth") "jconfig-feat-auth"))
  (should (string= (j/jconfig/container-name "main")      "jconfig-main")))

(ert-deftest j/jconfig/buffer-name-normal-mode ()
  "In normal mode (j/verbose nil), buffer name omits version."
  (let ((j/verbose nil))
    (cl-letf (((symbol-function 'j/jconfig/container-emacs-version)
               (lambda (_) "30.1")))
      (should (string= (j/jconfig/buffer-name "jconfig" "vterm")
                       "*jconfig vterm*"))
      (should (string= (j/jconfig/buffer-name "jconfig-feat-auth" "agent")
                       "*jconfig-feat-auth agent*")))))

(ert-deftest j/jconfig/buffer-name-verbose-with-version ()
  "In verbose mode with a known version, buffer name includes version."
  (let ((j/verbose t))
    (cl-letf (((symbol-function 'j/jconfig/container-emacs-version)
               (lambda (_) "30.1")))
      (should (string= (j/jconfig/buffer-name "jconfig" "vterm")
                       "*jconfig:30.1 vterm*"))
      (should (string= (j/jconfig/buffer-name "jconfig-feat-auth" "agent")
                       "*jconfig-feat-auth:30.1 agent*")))))

(ert-deftest j/jconfig/buffer-name-verbose-no-version ()
  "In verbose mode with no version label, falls back to plain name."
  (let ((j/verbose t))
    (cl-letf (((symbol-function 'j/jconfig/container-emacs-version)
               (lambda (_) nil)))
      (should (string= (j/jconfig/buffer-name "jconfig" "vterm")
                       "*jconfig vterm*")))))
#+END_SRC

```

- [ ] **Step 2: Tangle ert blocks and run all tests**

```bash
emacs --batch -l org \
  --eval "(progn \
    (add-to-list 'org-babel-tangle-lang-exts '(\"ert\" . \"el\")) \
    (org-babel-tangle-file \
      \"/home/jhaag/jconfig/.emacs.d/init.org\" nil \"ert\"))"

emacs --batch \
  -l /home/jhaag/jconfig/.emacs.d/tangles/tests/init-test.el \
  --eval "(ert-run-tests-batch-and-exit)"
```

Expected (all tests from all chunks accumulated):
```
Running 9 tests (2026-03-13 ...)
   passed  1/9  j/verbose-default-nil
   passed  2/9  j/toggle-verbose-flips-value
   passed  3/9  j/tab-bar/switch-or-create-defined
   passed  4/9  j/jconfig/container-name-default
   passed  5/9  j/jconfig/container-name-with-suffix
   passed  6/9  j/jconfig/buffer-name-normal-mode
   passed  7/9  j/jconfig/buffer-name-verbose-with-version
   passed  8/9  j/jconfig/buffer-name-verbose-no-version
   ...

Ran 9 tests, 9 results as expected
```

- [ ] **Step 3: Commit**

```bash
cd ~/jconfig
git add .emacs.d/init.org
git commit -m "feat(emacs): add jconfig-workspace section (C-c a j / C-c a J)"
```

---

### Task 12: Update TOC and add Tasks/TODO entries

**Files:**
- Modify: `.emacs.d/init.org` (TOC block, Tasks section)

- [ ] **Step 1: Update Table of Contents**

The TOC uses `:TOC_2:noexport:`. Update it to include all new sections.

Under `- [[#bootstrap-process][Bootstrap Process]]`, add:
```org
  - [[#custom-org-babel-languages][Custom org-babel Languages]]
```

Under `- [[#utilities][Utilities]]`, add:
```org
  - [[#verbosity][Verbosity]]
```

Under `- [[#packages][Packages]]`, add in alphabetical position:
- After `** buffer-move` entry: `  - [[#docker][docker]]`
- After all helm subsections: `  - [[#jconfig-workspace][jconfig-workspace]]`
- Replace `  - [[#shell-pop][shell-pop]]` with `  - [[#vterm][vterm]]`
- After `** symon` entry: `  - [[#tab-bar][tab-bar]]`
- After `** tab-bar` entry: `  - [[#tramp][tramp]]`

- [ ] **Step 2: Add TODO entries to Tasks section**

In `* Tasks [/]`, after the existing reconcile TODO, add:

```org
** TODO Verify agent-shell Docker API for jconfig-workspace

~j/jconfig/setup-layout~ calls ~agent-shell-start-in-container~ guarded by
~fboundp~. Verify the actual function name against the installed ~agent-shell~
package once Docker support stabilises upstream.

Reference: https://github.com/xenodium/agent-shell?tab=readme-ov-file#running-agents-in-devcontainers--docker-containers-experimental

** TODO Multi-agent jconfig workspaces

Naming convention: ~jconfig-<worktree>~ maps 1:1 to a git worktree at
~~/jconfig-<worktree>~. Container name, tab-bar tab, and buffer prefix share
the same stem.

*Next steps:*
- Parameterise ~jconfig-docker.sh start~ mount source: currently always
  mounts ~~/jconfig~; multi-worktree requires ~~/jconfig-<name>~ per container
- Add ~jconfig-docker.sh list~ — enumerate running containers via
  ~docker ps --filter label=jconfig.worktree~
- Extend ~C-c a j~ to accept a worktree name wired to the correct mount
- Coordination model: agents on isolated branches; PRs/issues on the shared
  host repo are the coordination mechanism

```

- [ ] **Step 3: Final tangle + test run**

```bash
# Full emacs-lisp tangle
emacs --batch -l org \
  --eval "(org-babel-tangle-file \"/home/jhaag/jconfig/.emacs.d/init.org\")" \
  2>&1 | tail -3

# bash-emacs-deps tangle
emacs --batch -l org \
  --eval "(progn \
    (require 'ob-shell) \
    (add-to-list 'org-babel-tangle-lang-exts '(\"bash-emacs-deps\" . \"sh\")) \
    (org-babel-tangle-file \
      \"/home/jhaag/jconfig/.emacs.d/init.org\" \
      \"/home/jhaag/jconfig/.emacs.d/tangles/system-setup.sh\" \
      \"^bash-emacs-deps$\"))"

# ERT tangle + run
emacs --batch -l org \
  --eval "(progn \
    (add-to-list 'org-babel-tangle-lang-exts '(\"ert\" . \"el\")) \
    (org-babel-tangle-file \
      \"/home/jhaag/jconfig/.emacs.d/init.org\" nil \"ert\"))"

emacs --batch \
  -l /home/jhaag/jconfig/.emacs.d/tangles/tests/init-test.el \
  --eval "(ert-run-tests-batch-and-exit)"
```

Expected: no tangle errors; all ERT tests pass.

- [ ] **Step 4: Final commit**

```bash
cd ~/jconfig
git add .emacs.d/init.org
git commit -m "docs(emacs): update init.org TOC and add multi-agent + agent-shell TODOs"
```

---

## Integration Checklist (manual, requires Docker + running Emacs)

- [ ] `scripts/jconfig-docker.sh build` completes; `jconfig:latest` image exists
- [ ] `scripts/jconfig-docker.sh start` starts container named `jconfig`
- [ ] `docker ps --filter label=jconfig.worktree` shows correct labels
- [ ] `scripts/jconfig-docker.sh attach` opens bash in container
- [ ] `configure.sh` runs without error; `system-setup.sh` is created and contains libvterm-dev check
- [ ] `C-c a j` opens three-pane tab-bar layout
- [ ] `C-c t t` opens a vterm popup
- [ ] `C-c d` opens docker.el transient menu
- [ ] `C-S-v` toggles verbose buffer naming
- [ ] Running `configure.sh` again is idempotent
