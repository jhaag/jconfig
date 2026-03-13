# Agent Skill Management — Design Spec

**Date:** 2026-03-13
**Status:** Approved
**Branch:** `tmux-to-emacs`

---

## Overview

Extend the `agent-shell` `use-package` block in `.emacs.d/init.org` to automatically install and update enumerated marketplace skills/plugins for configured agentic coding tools. Emacs prompts the user for interactive approval before any install or update. The canonical skill list is defined once; an alist maps tools to their installer functions, making it easy to add new tools later.

---

## Scope

- **In scope:** Marketplace plugins (e.g. `superpowers@claude-plugins-official`). Claude Code is the initial tool. Codex CLI deferred.
- **Out of scope:** Local skills in `.claude/skills/` — managed separately via version control.

---

## Data Structures

Two `defvar`s and all helper functions are declared in `:preface` so they are available at macro-expansion time, before any `:if` condition is tested and regardless of whether `agent-shell` itself is installed. This is intentional: `j/agent/sync-skills` only depends on `cl-lib` and `subr-x`, not on the `agent-shell` package.

```elisp
(defvar j/agent/skills
  '("superpowers@claude-plugins-official"
    "emacs-skills@xenodium-emacs-skills")
  "Canonical list of marketplace skills to install for all configured tools.")

(defvar j/agent/tool-installers
  '((claude-code . j/agent/claude-code-skill))
  "Alist of (tool-symbol . installer-fn).
Each fn takes (skill action) where action is 'check, 'install, or 'update.")
```

Adding a new tool requires one `cons` cell in `j/agent/tool-installers` and a corresponding installer function.

---

## Installer Function Contract

Each installer function has signature `(skill action)`:

| `action`    | Behaviour                                              | Return value                        |
|-------------|--------------------------------------------------------|-------------------------------------|
| `'check`    | Is `skill` installed? Returns nil if tool not on PATH. | non-nil / nil                       |
| `'install`  | Install `skill`. No-op (returns nil) if tool missing.  | t on success, nil on failure        |
| `'update`   | Update `skill`. Undefined if skill not installed — callers must only call `'update` on skills that passed `'check`. | t on success, nil on failure |

---

## Claude Code Installer

```elisp
(require 'subr-x)  ; for string-join, in :preface

(defun j/agent/claude-code-list-skills ()
  "Return list of installed Claude Code plugin names."
  (when (executable-find "claude")
    (split-string (shell-command-to-string "claude plugins list") "\n" t)))

(defun j/agent/claude-code-skill (skill action)
  "Manage SKILL for Claude Code. ACTION is 'check, 'install, or 'update."
  (pcase action
    ('check   (member skill (j/agent/claude-code-list-skills)))
    ('install (when (executable-find "claude")
                (= 0 (call-process-shell-command
                       (format "claude plugins install %s" skill) nil 0))))
    ('update  (when (executable-find "claude")
                (= 0 (call-process-shell-command
                       (format "claude plugins update %s" skill) nil 0))))))
```

`call-process-shell-command` is called with destination `0` to discard shell output rather than inserting it into the current buffer.

**Known unknown:** The exact output format of `claude plugins list` is not yet verified. `j/agent/claude-code-list-skills` may need adjustment after observing real output.

---

## Sync Function

To avoid calling `'check` twice per skill (once to find missing, once to find present), a single pass partitions skills by cached check results:

```elisp
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
```

---

## use-package Integration

```elisp
(use-package agent-shell
  :ensure t
  :preface
  (require 'subr-x)
  (defvar j/agent/skills '(...))
  (defvar j/agent/tool-installers '((claude-code . j/agent/claude-code-skill)))
  (defun j/agent/claude-code-list-skills () ...)
  (defun j/agent/claude-code-skill (skill action) ...)
  (defun j/agent/sync-skills (&optional update) ...)
  (add-hook 'emacs-startup-hook (lambda () (j/agent/sync-skills t)))
  :bind (("C-c a a" . agent-shell)
         ("C-c a c" . agent-shell-anthropic-start-claude-code)
         ("C-c a s s" . j/agent/sync-skills)))
```

**Hook placement:** `add-hook` is in `:preface` (not `:init` or `:config`) because `j/agent/sync-skills` does not depend on `agent-shell` loading — it only needs `cl-lib` and `subr-x`. `emacs-startup-hook` fires after `init.el` completes, so the hook is registered before it fires.

**Startup behaviour:** The lambda unconditionally passes `t` (update mode). This means both install and update prompts appear at startup. This is intentional — if you want to suppress update prompts at startup you must edit the hook directly.

**Keybindings:** `C-c a s` is a one-character prefix namespace for skill-related commands. `C-c a s s` is sync. Future candidates: `C-c a s l` (list), `C-c a s i` (install one). Binding `C-c a s s` automatically makes `C-c a s` a prefix key; no explicit `define-prefix-command` is needed.

**`C-u M-x j/agent/sync-skills`** also triggers update mode interactively.

---

## Extensibility

To add Codex CLI (or any other tool):

1. Write `j/agent/codex-skill (skill action)` implementing the three actions
2. Add `(codex . j/agent/codex-skill)` to `j/agent/tool-installers`

No changes to `j/agent/skills` or `j/agent/sync-skills` are required.
