;; This file contains all package management for my emacs configuration
;; (done with use-package)

;;=== Git-Gutter+ ==============================================================
(use-package git-gutter+
  :ensure t
  :init (use-package git-gutter-fringe+
          :ensure t)
  :bind (("C-x g g" . git-gutter+-mode)
         :map git-gutter+-mode-map
              ("C-x n"   . git-gutter+-next-hunk)
              ("C-x p"   . git-gutter+-previous-hunk)
              ("C-x v =" . git-gutter+-show-hunk)
              ("C-x r"   . git-gutter+-revert-hunks)
              ("C-x t"   . git-gutter+-stage-hunks)
              ("C-x c"   . git-gutter+-commit)
              ("C-x C"   . git-gutter+-stage-and-commit)
              ("C-x C-y" . git-gutter+-stage-and-commit-whole-buffer)
              ("C-x U"   . git-gutter+-unstage-whole-buffer))
  :config (progn
            (setq git-gutter+-hide-gutter t)
            (add-hook 'prog-mode-hook 'git-gutter+-mode))
  :diminish (git-gutter+-mode . "gg+"))

;;=== Haskell Mode =============================================================
;; TODO: Actually learn how to use this, and also get a really nice haskell
;;       setup going
(use-package haskell
  :ensure haskell-mode)

;;=== Solarized ================================================================
(use-package solarized
  :ensure solarized-theme
  :init (load-theme 'solarized-dark t))

;;=== SSH ======================================================================
(use-package ssh
  :ensure t
  :config (progn
	    (custom-set-variables
	     '(ssh-directory-tracking-mode t))
	    (add-hook 'ssh-mode-hook (lambda ()
				       (shell-dirtrack-mode t)
				       (setq dirtrackp nil)))))
