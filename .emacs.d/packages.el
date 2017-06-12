;; This file contains all package management for my emacs configuration
;; (done with use-package)

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
