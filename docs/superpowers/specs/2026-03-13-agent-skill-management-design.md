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

Two `defvar`s in the `:preface` of `use-package agent-shell`:

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

| `action`    | Behaviour                                      | Return value        |
|-------------|------------------------------------------------|---------------------|
| `'check`    | Is `skill` already installed?                  | non-nil / nil       |
| `'install`  | Install `skill`                                | t on success        |
| `'update`   | Update `skill`                                 | t on success        |

---

## Claude Code Installer

```elisp
(defun j/agent/claude-code-list-skills ()
  "Return list of installed Claude Code plugin names."
  (split-string (shell-command-to-string "claude plugins list") "\n" t))

(defun j/agent/claude-code-skill (skill action)
  "Manage SKILL for Claude Code. ACTION is 'check, 'install, or 'update."
  (pcase action
    ('check   (member skill (j/agent/claude-code-list-skills)))
    ('install (= 0 (call-process-shell-command
                    (format "claude plugins install %s" skill))))
    ('update  (= 0 (call-process-shell-command
                    (format "claude plugins update %s" skill))))))
```

**Note:** The exact output format of `claude plugins list` is unknown until runtime. `j/agent/claude-code-list-skills` may need adjustment after observing real output.

---

## Sync Function

```elisp
(defun j/agent/sync-skills (&optional update)
  "Install missing marketplace skills for all configured tools.
With prefix arg UPDATE, also prompt to update already-installed skills."
  (interactive "P")
  (dolist (entry j/agent/tool-installers)
    (let* ((tool    (car entry))
           (fn      (cdr entry))
           (missing (cl-remove-if (lambda (s) (funcall fn s 'check))
                                  j/agent/skills))
           (present (cl-remove-if-not (lambda (s) (funcall fn s 'check))
                                      j/agent/skills)))
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
  (defvar j/agent/skills '(...))
  (defvar j/agent/tool-installers '((claude-code . j/agent/claude-code-skill)))
  (defun j/agent/claude-code-list-skills () ...)
  (defun j/agent/claude-code-skill (skill action) ...)
  (defun j/agent/sync-skills (&optional update) ...)
  :init
  (add-hook 'emacs-startup-hook (lambda () (j/agent/sync-skills t)))
  :bind (("C-c a a" . agent-shell)
         ("C-c a c" . agent-shell-anthropic-start-claude-code)
         ("C-c a s s" . j/agent/sync-skills)))
```

- `C-c a s` acts as a prefix for skills-related commands
- `C-c a s s` triggers `j/agent/sync-skills`
- At startup, the lambda passes `t` so both install and update prompts run
- `C-u M-x j/agent/sync-skills` also triggers update mode manually

---

## Extensibility

To add Codex CLI (or any other tool):

1. Write `j/agent/codex-skill (skill action)` implementing the three actions
2. Add `(codex . j/agent/codex-skill)` to `j/agent/tool-installers`

No changes to `j/agent/skills` or `j/agent/sync-skills` are required.
