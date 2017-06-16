;; This file contains all package management for my emacs configuration
;; (done with use-package)

;;=== Buffer Move ==============================================================
(use-package buffer-move
  :ensure t
  :preface (progn
             (defun j/2-windows-vertical-to-horizontal ()
               "Swap from a vertical to a horizontal orientation"
               (let ((buffers (mapcar 'window-buffer (window-list))))
                 (when (= 2 (length buffers))
                   (delete-other-windows)
                   (set-window-buffer (split-window-horizontally) (cadr buffers)))))

             (defun j/2-windows-vertical-to-horizontal-interactive ()
               "Interactive wrapper around j/2-windows-vertical-to-horizontal"
               (interactive)
               (j/2-windows-vertical-to-horizontal)))
  :init (progn
          (add-hook 'emacs-startup-hook 'j/2-windows-vertical-to-horizontal)
          (global-unset-key (kbd "C-x o")))
  :bind (("M-<up>"             . buf-move-up)
         ("M-<down>"           . buf-move-down)
         ("M-<left>"           . buf-move-left)
         ("M-<right>"          . buf-move-right)
         ("M-<return>"         . j/2-windows-vertical-to-horizontal-interactive)
         ("C-<tab>"            . other-window)
         ("C-S-<iso-lefttab>"  . previous-multiframe-window)))

;;=== Git-Gutter+ ==============================================================
(use-package git-gutter+
  :ensure t
  :init (progn
          (use-package gitignore-mode
            :ensure t
            :defer t)
          (use-package gitconfig-mode
            :ensure t
            :defer t)
          (use-package gitattributes-mode
            :disabled t
            :defer t)
          (add-hook 'prog-mode-hook 'git-gutter+-mode))
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

;;=== Helm =====================================================================
(use-package helm
  :init (progn
          (require 'helm-config)
          (require 'helm-grep)
          ;; To fix error at compile:
          ;; Error (bytecomp): Forgot to expand macro with-helm-buffer in
          ;; (with-helm-buffer helm-echo-input-in-header-line)
          (if (version< "26.0.50" emacs-version)
              (eval-when-compile (require 'helm-lib)))

          (defun helm-hide-minibuffer-maybe ()
            (when (with-helm-buffer helm-echo-input-in-header-line)
              (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
                (overlay-put ov 'window (selected-window))
                (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                        `(:background ,bg-color :foreground ,bg-color)))
                (setq-local cursor-type nil))))

          (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

          ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
          ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
          ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
          (global-set-key (kbd "C-c h") 'helm-command-prefix)
          (global-unset-key (kbd "C-x c"))

          (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
          (define-key helm-map (kbd "C-i")   'helm-execute-persistent-action) ; TAB for terminal
          (define-key helm-map (kbd "C-z")   'helm-select-action)

          (define-key helm-grep-mode-map (kbd "<return>") 'helm-grep-mode-jump-other-window)
          (define-key helm-grep-mode-map (kbd "n")        'helm-grep-mode-jump-other-window-forward)
          (define-key helm-grep-mode-map (kbd "p")        'helm-grep-mode-jump-other-window-backward)

          (when (executable-find "curl")
            (cutsomize-set-variable helm-google-suggest-use-curl-p t))

          (custom-set-variables
           '(helm-scroll-amount 4)             ; scroll 4 lines in the other window use M-<next>/M-<prev>
           ;; '(helm-quick-update t)           ; do not display invisible candidates
           '(helm-ff-search-library-in-sexp t) ; search for library in `require' and `declare-function' sexp.
           
           ;; you can customize helm-do-grep to execute ack-grep
           ;; '(helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f")
           ;; '(helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f")
           '(helm-split-window-in-side-p t)    ; open helm buffer inside current window instead of occupying the other window

           '(helm-echo-input-in-header-line t)

           ;; '(helm-candidate-number-limit 500) ; limit the number of displayed candidates
           '(helm-ff-file-name-history-use-recentf t)
           '(helm-move-to-line-cycle-in-source t) ; move to end or beginning of source when reaching top or bottom or source
           '(helm-buffer-skip-remote-checking t)

           '(helm-mode-fuzzy-match t)

           '(helm-buffers-fuzzy-matching t) ; fuzzy matching buffer names when non-nil; useful in helm-mini that lists buffers

           '(helm-org-headings-fontify t)
           ;; '(helm-find-files-sort-directories t)
           ;; '(ido-use-virtual-buffers t)
           '(helm-semantic-fuzzy-match t)
           '(helm-M-x-fuzzy-match t)
           '(helm-imenu-fuzzy-match t)
           '(helm-lisp-fuzzy-completion t)
           ;; '(helm-apropos-fuzzy-match t)
           '(helm-buffer-skip-remote-checking t)
           '(helm-locate-fuzzy-match t)
           '(helm-display-header-line nil))
          
          (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

          (global-set-key (kbd "M-x")     'helm-M-x)
          (global-set-key (kbd "M-y")     'helm-show-kill-ring)
          (global-set-key (kbd "C-x b")   'helm-buffers-list)
          (global-set-key (kbd "C-x C-f") 'helm-find-files)
          (global-set-key (kcd "C-c r")   'helm-recentf)
          (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
          (global-set-key (kbd "C-c h o") 'helm-occur)

          (global-set-key (kbd "C-c h w") 'helm-wikipedia-suggest)
          (global-set-key (kbd "C-c h g") 'helm-google-suggest)

          (global-set-key (kbd "C-c h x") 'helm-register)
          ;; (global-set-key (kbd "C-x r j") 'jump-to-register

          (define-key 'help-command (kbd "C-f") 'helm-apropos)
          (define-key 'help-command (kbd "r")   'helm-info-emacs)
          (define-ley 'help-command (kbd "C-l") 'helm-locate-library)

          ;; use helm to list eshell history
          (add-hook 'eshell-mode-hook
                    #'(lambda ()
                        (define-key eshell-mode-map (kbd "M-l") 'helm-eshell-history)))

          ;; Save current position to mark ring
          (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

          ;; show minibuffer history with Helm
          (define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
          (define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

          (define-key global-map [remap find-tag] 'helm-etags-select)

          (define-key global-map [remap list-buffers] 'helm-buffers-list)

          ;;=== Helm Swoop =====================================================
          (use-package helm-swoop
            :bind (("C-c h o" . helm-swoop)
                   ("C-c s"   . helm-multi-swoop-all))
            :config (progn
                      ;; when doing isearch, hand the word over to helm-swoop
                      (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

                      ;; from helm-swoop to helm-multi-swoop-all
                      (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

                      (custom-set-variables
                       '(helm-multi-swoop-edit-save t)             ; save buffer when edit complete
                       '(helm-swoop-split-with-multiple-windows t) ; split window inside current window if t
                       '(helm-swoop-split-direction 'split-window-vertically)
                       '(helm-swoop-speed-or-color t))))              ; if nil, boost speed by removing color

          (helm-mode 1)

          ;;=== Helm Projectile ================================================
          (use-package helm-projectile
            :init (progn
                    (helm-projectile-on)
                    (custom-set-variables
                     '(projectile-completion-system 'helm)
                     '(projectile-indexing-method 'alien))))))

;;=== Multiple Cursors =========================================================
(use-package multiple-cursors
  :ensure t
  :bind ("C-C C-C" . mc/edit-lines))

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
(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode)
  :diminish undo-tree-mode)

;;=== Wttrin (Weather In) ======================================================
(use-package wttrin
  :if window-system
  :ensure t
  :defer t
  :bind ("M-? w" . wttrin)
  :config (custom-set-variables
           '(wttrin-default-cities '("" "02148" "02139" "Malden" "Lexington" "Cambridge"))
           '(wttrin-default-accept-language '("Accept-Language" . "en"))))
