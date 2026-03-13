# Agent Skill Management Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend the `agent-shell` use-package block in `init.org` to enumerate marketplace skills, prompt for install/update at startup, and expose `j/agent/sync-skills` interactively.

**Architecture:** All logic lives in the `:preface` of `use-package agent-shell`. A `defvar` list holds canonical skill names; a `defvar` alist maps tool symbols to installer functions. ERT tests live in a folded "Test" subsection using `#+BEGIN_SRC ert` blocks — a custom org-babel language bound to `emacs-lisp`, with a dedicated tangle target (`tangles/tests/init-test.el`). Tests are tangled independently via `(org-babel-tangle-file ... nil "ert")`, never requiring a full init.org retangle.

**Tech Stack:** Emacs Lisp, `cl-lib`, `subr-x`, org-babel (`ert` language alias), ERT.

---

## Chunk 1: Implementation

### Task 1: Register `ert` as an org-babel language

Add the `#+PROPERTY` header for `ert` blocks and register the language extension in the bootstrap, so both interactive editing and batch tangle understand the `ert` language.

**Files:**
- Modify: `.emacs.d/init.org` — line 1 (add PROPERTY) and Bootstrap section (add lang-exts entry)

- [ ] **Step 1: Add `#+PROPERTY` for `ert` at line 2 of init.org**

The current top of the file is:
```org
#+PROPERTY: header-args:emacs-lisp :tangle (concat (expand-file-name "~/jconfig/.emacs.d/tangles/") (file-name-base) ".el")
#+PROPERTY: header-args :mkdirp yes :comments no
```

Insert a new line after line 1:
```org
#+PROPERTY: header-args:ert :tangle (concat (expand-file-name "~/jconfig/.emacs.d/tangles/tests/") (file-name-base) "-test.el")
```

Result — first three lines of init.org:
```org
#+PROPERTY: header-args:emacs-lisp :tangle (concat (expand-file-name "~/jconfig/.emacs.d/tangles/") (file-name-base) ".el")
#+PROPERTY: header-args:ert :tangle (concat (expand-file-name "~/jconfig/.emacs.d/tangles/tests/") (file-name-base) "-test.el")
#+PROPERTY: header-args :mkdirp yes :comments no
```

- [ ] **Step 2: Add `org-babel-tangle-lang-exts` entry in the Bootstrap section**

Find the block containing `j/config/load-conf-dot-org` (around line 297 — the block that tangles to `~/.emacs`). Add one line to register the `ert` extension. The block currently starts with something like:

```elisp
#+BEGIN_SRC emacs-lisp :tangle (expand-file-name "~/.emacs")
(defun j/config/load-conf-dot-org (filename) ...
```

Add before the `defun`:
```elisp
(add-to-list 'org-babel-tangle-lang-exts '("ert" . "el"))
```

This ensures the running Emacs (after loading `~/.emacs`) understands `ert` as a tangle language producing `.el` files. The `:mkdirp yes` global PROPERTY ensures `tangles/tests/` is created automatically.

---

### Task 2: Add the agent-shell use-package block with a folded Test subsection

Replace the current minimal `agent-shell` block (lines 500–505 of `init.org`) with the full implementation, then add a "Test" subsection immediately after using `#+BEGIN_SRC ert`.

**Files:**
- Modify: `.emacs.d/init.org` — the `** agent-shell` section

- [ ] **Step 1: Replace the existing agent-shell org section**

Find:
```org
** agent-shell
...
#+BEGIN_SRC emacs-lisp
(use-package agent-shell
  :ensure t
  :bind (("C-c a a" . agent-shell)
         ("C-c a c" . agent-shell-anthropic-start-claude-code)))
#+END_SRC
```

Replace with:
```org
** agent-shell
#+BEGIN_SRC emacs-lisp
(use-package agent-shell
  :ensure t
  :preface
  (require 'subr-x)

  (defvar j/agent/skills
    '("superpowers@claude-plugins-official"
      "emacs-skills@xenodium-emacs-skills")
    "Canonical list of marketplace skills to install for all configured tools.")

  (defvar j/agent/tool-installers
    '((claude-code . j/agent/claude-code-skill))
    "Alist of (tool-symbol . installer-fn).
Each fn takes (skill action) where action is \\='check, \\='install, or \\='update.")

  (defun j/agent/claude-code-list-skills ()
    "Return list of installed Claude Code plugin names."
    (when (executable-find "claude")
      (split-string (shell-command-to-string "claude plugins list") "\n" t)))

  (defun j/agent/claude-code-skill (skill action)
    "Manage SKILL for Claude Code. ACTION is \\='check, \\='install, or \\='update."
    (pcase action
      ('check   (member skill (j/agent/claude-code-list-skills)))
      ('install (when (executable-find "claude")
                  (= 0 (call-process-shell-command
                         (format "claude plugins install %s" skill) nil 0))))
      ('update  (when (executable-find "claude")
                  (= 0 (call-process-shell-command
                         (format "claude plugins update %s" skill) nil 0))))))

  (defun j/agent/sync-skills (&optional update)
    "Install missing marketplace skills for all configured tools.
With prefix arg UPDATE, also prompt to update already-installed skills."
    (interactive "P")
    (dolist (entry j/agent/tool-installers)
      (let* ((tool    (car entry))
             (fn      (cdr entry))
             (checked (mapcar (lambda (s) (cons s (funcall fn s 'check)))
                              j/agent/skills))
             (missing (mapcar #'car (cl-remove-if #'cdr checked)))
             (present (mapcar #'car (cl-remove-if-not #'cdr checked))))
        (when (and missing
                   (y-or-n-p (format "[%s] Install %d skill(s): %s? "
                                     tool (length missing)
                                     (string-join missing ", "))))
          (dolist (skill missing) (funcall fn skill 'install)))
        (when (and update present
                   (y-or-n-p (format "[%s] Update %d skill(s): %s? "
                                     tool (length present)
                                     (string-join present ", "))))
          (dolist (skill present) (funcall fn skill 'update))))))

  (add-hook 'emacs-startup-hook (lambda () (j/agent/sync-skills t)))

  :bind (("C-c a a" . agent-shell)
         ("C-c a c" . agent-shell-anthropic-start-claude-code)
         ("C-c a s s" . j/agent/sync-skills)))
#+END_SRC

*** Test
:PROPERTIES:
:VISIBILITY: folded
:END:

#+BEGIN_SRC ert
;;; Tests for j/agent/sync-skills partition logic -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)

(defun j/test/mock-installer (installed-skills)
  "Return an installer fn that treats INSTALLED-SKILLS as already installed."
  (lambda (skill action)
    (pcase action
      ('check (member skill installed-skills))
      ('install t)
      ('update t))))

(defun j/test/partition (fn skills)
  "Partition SKILLS into (missing . present) using installer FN.
Mirrors the single-pass logic in `j/agent/sync-skills'."
  (let* ((checked (mapcar (lambda (s) (cons s (funcall fn s 'check))) skills))
         (missing (mapcar #'car (cl-remove-if #'cdr checked)))
         (present (mapcar #'car (cl-remove-if-not #'cdr checked))))
    (cons missing present)))

(ert-deftest j/test/partition-all-missing ()
  "All skills are missing when none are installed."
  (let* ((fn (j/test/mock-installer '()))
         (result (j/test/partition fn '("a@x" "b@x"))))
    (should (equal (car result) '("a@x" "b@x")))
    (should (null (cdr result)))))

(ert-deftest j/test/partition-all-present ()
  "All skills are present when all are installed."
  (let* ((fn (j/test/mock-installer '("a@x" "b@x")))
         (result (j/test/partition fn '("a@x" "b@x"))))
    (should (null (car result)))
    (should (equal (cdr result) '("a@x" "b@x")))))

(ert-deftest j/test/partition-mixed ()
  "Correctly splits installed and missing skills."
  (let* ((fn (j/test/mock-installer '("a@x")))
         (result (j/test/partition fn '("a@x" "b@x"))))
    (should (equal (car result) '("b@x")))
    (should (equal (cdr result) '("a@x")))))

(ert-deftest j/test/partition-empty ()
  "Empty skill list returns empty lists."
  (let* ((fn (j/test/mock-installer '()))
         (result (j/test/partition fn '())))
    (should (null (car result)))
    (should (null (cdr result)))))
#+END_SRC
```

---

### Task 3: Tangle tests and run ERT

Tangle only `ert` blocks from `init.org` to `tangles/tests/init-test.el`, then run ERT in batch. No full init.org retangle required.

- [ ] **Step 1: Tangle only `ert` blocks**

```bash
emacs --batch -l org --eval \
  "(progn \
     (add-to-list 'org-babel-tangle-lang-exts '(\"ert\" . \"el\")) \
     (org-babel-tangle-file \"/home/jhaag/jconfig/.emacs.d/init.org\" nil \"ert\"))"
```

Expected: exits 0. Verify output file:
```bash
ls -la /home/jhaag/jconfig/.emacs.d/tangles/tests/init-test.el
```

- [ ] **Step 2: Run ERT tests in batch**

```bash
emacs --batch \
  -l /home/jhaag/jconfig/.emacs.d/tangles/tests/init-test.el \
  --eval "(ert-run-tests-batch-and-exit)"
```

Expected:
```
Running 4 tests (2026-03-13 ...)
   passed  1/4  j/test/partition-all-missing
   passed  2/4  j/test/partition-all-present
   passed  3/4  j/test/partition-mixed
   passed  4/4  j/test/partition-empty

Ran 4 tests, 4 results as expected
```

If any test fails, fix the `ert` block in `init.org`, re-run step 1, then step 2.

---

### Task 4: Smoke test the Claude Code installer

- [ ] **Step 1: Verify `claude` is on exec-path**

```bash
emacsclient --eval '(executable-find "claude")'
```

Expected: a path string. If nil, `check` returns nil for all skills — correct behaviour.

- [ ] **Step 2: Check list-skills returns parseable output**

```bash
emacsclient --eval '(j/agent/claude-code-list-skills)'
```

Each element should be a bare plugin name like `"superpowers@claude-plugins-official"`. If the output includes version numbers or extra whitespace, update `j/agent/claude-code-list-skills` in `init.org` to filter appropriately before continuing.

- [ ] **Step 3: Check installed status of each enumerated skill**

```bash
emacsclient --eval \
  '(mapcar (lambda (s) (cons s (j/agent/claude-code-skill s (quote check)))) j/agent/skills)'
```

Expected: `(("superpowers@claude-plugins-official" . t) ("emacs-skills@xenodium-emacs-skills" . t))`.

---

### Task 5: Commit

- [ ] **Step 1: Stage and commit**

```bash
git add .emacs.d/init.org
git commit -m "feat(emacs): add agent skill management to agent-shell use-package"
```

Note: `tangles/` is gitignored; only `init.org` is committed.

---

## Notes

- **Re-running tests:** `emacs --batch -l org --eval "(progn (add-to-list 'org-babel-tangle-lang-exts '(\"ert\" . \"el\")) (org-babel-tangle-file \"/home/jhaag/jconfig/.emacs.d/init.org\" nil \"ert\"))"` then `emacs --batch -l /home/jhaag/jconfig/.emacs.d/tangles/tests/init-test.el --eval "(ert-run-tests-batch-and-exit)"`.
- **`claude plugins list` format:** If task 4 step 2 reveals unexpected output format, update `j/agent/claude-code-list-skills` before committing.
- **Adding Codex CLI:** Write `j/agent/codex-skill (skill action)`, add `(codex . j/agent/codex-skill)` to `j/agent/tool-installers`. No other changes needed.
- **`C-c a s` prefix:** Future skills sub-commands use `C-c a s <x>`.
