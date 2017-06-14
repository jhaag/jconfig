;; This file contains all package management for my emacs configuration
;; (done with use-package)

;;=== Git-Gutter+ ==============================================================
(use-package git-gutter+
  :ensure t
  :init (add-hook 'prog-mode-hook 'git-gutter+-mode)
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
            (use-package git-gutter-fringe+
              :ensure t
              :config (progn
                        ;; Currently hard-coded to be the solarized-yellow color
                        (set-face-attribute 'git-gutter-fr+-modified nil :foreground "#b58900")
                        (fringe-helper-define 'git-gutter-fr+-modified nil
                          "........"
                          "........"
                          "..XX...X"
                          ".XXXX.XX"
                          "XX.XXXX."
                          "X...XX.."
                          "........"
                          "........")))
            (custom-set-variables
             '(git-gutter+-hide-gutter t)))
  :diminish (git-gutter+-mode . "gg+"))

;;=== Git Modes ================================================================
(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitattributes-mode
  :disabled t
  :defer t)

;;=== Haskell Mode =============================================================
;; TODO: Actually learn how to use this, and also get a really nice haskell
;;       setup going
(use-package haskell
  :ensure haskell-mode
  :defer t)

;;=== Shell Pop ================================================================
(use-package shell-pop
  :ensure t
  :defer t
  :bind ("M-+" . shell-pop)
  :config (progn
            (custom-set-variables
             '(shell-pop-shell-type "ansi-term"))
            (add-hook 'shell-pop-out-hook (lambda ()
                                            (delete-process "*shell-1*")))))

;;=== Smooth Scrolling =========================================================
(use-package smooth-scrolling
  :ensure t
  :config (smooth-scrolling-mode 1))

;;=== Solarized ================================================================
(use-package solarized
  :ensure solarized-theme
  :init (load-theme 'solarized-dark t))

;;=== SSH ======================================================================
(use-package ssh
  :defer t
  :config (progn
	    (custom-set-variables
	     '(ssh-directory-tracking-mode t))
	    (add-hook 'ssh-mode-hook (lambda ()
				       (shell-dirtrack-mode t)
				       (setq dirtrackp nil)))))

;;=== Symon ====================================================================
(use-package symon
  :if window-system
  :ensure t
  :config (progn
            (custom-set-variables
             '(symon-monitors '(symon-current-time-monitor
                                symon-linux-battery-monitor
                                symon-linux-cpu-monitor
                                symon-linux-memory-monitor
                                symon-linux-network-rx-monitor
                                symon-linux-network-tx-monitor))
             '(symon-delay 1)
             '(symon-refresh-rate 0.5)
             '(symon-history-size 25)
             '(symon-sparkline-type 'bounded))
             (symon-mode)))

;;=== Undo Tree ================================================================

;;=== Wttrin (Weather In) ======================================================
(use-package wttrin
  :if window-system
  :ensure t
  :defer t
  :bind ("C-? w" . wttrin)
  :config (custom-set-variables
           '(wttrin-default-cities '("" "02148" "02139" "Malden" "Lexington" "Cambridge"))
           '(wttrin-default-accept-language '("Accept-Language" . "en"))))
