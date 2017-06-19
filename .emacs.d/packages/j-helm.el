;;=== Core Setup ===============================================================
(use-package helm ; Helm
  :ensure t
  :bind (:map helm-map
              ("C-i" . helm-execute-persistent-action)
              ("C-z" . helm-select-action))
  :config (progn
            (custom-set-variables
             '(helm-move-to-line-cycle-in-source t) ; Cycle helm candidates
             '(helm-scroll-amount 8)                ; Scroll 8 lines at a time
             '(helm-display-header-line nil))       ; Cleaner helm interface

            (helm-adaptive-mode 1)   ; Adaptive sorting in all sources
            (helm-autoresize-mode 1) ; Auto-resize helm-buffer
            
            (add-to-list 'helm-sources-using-default-as-input
                         'helm-source-man-pages)))

(use-package helm-mode ; Helm-Mode
  :ensure helm
  :init (helm-mode 1)
  :config (custom-set-variables ; Fuzzy matching
           '(helm-mode-fuzzy-match t)
           '(helm-completion-in-region-fuzzy-match t)
           '(helm-split-window-in-side-p t))
  :diminish helm-mode)

(use-package helm-config ; Applications library for `heml.el'
  :ensure helm
  :after helm
  :config (progn
            (bind-key "C-c h" helm-command-prefix)
            (unbind-key "C-x c")))

(use-package helm-command ; M-x in helm
  :ensure helm
  :bind ([remap execute-extended-command] . helm-M-x)
  :config (custom-set-variables
           '(helm-M-x-fuzzy-match t)))

(use-package helm-files ; Find files with helm
  :ensure helm
  :bind (([remap find-file] . helm-find-files)
         ("C-x C-r"         . helm-recentf)
         :map helm-find-files-map
         ("C-k" . helm-ff-persistent-delete))
  :config (custom-set-variables
           '(helm-recentf-fuzzy-match t)
           '(helm-ff-file-name-history-use-recentf t)
           '(helm-ff-newfile-prompt-p nil) ; don't prompt for a new buffer for new files
           '(helm-input-idle-delay 0.1)
           '(helm-ff-skip-boring-files t)
           '(helm-ff-search-library-in-sexp t)
           '(helm-ff-auto-update-intial-value t)))

(use-package helm-misc ; Misc helm commands
  :ensure helm
  :bind ([remap switch-to-buffer] . helm-mini))

(use-package helm-buffers ; Manage buffers with helm
  :ensure helm
  :bind (("C-c C-b" . helm-buffers-list)
         :map helm-buffer-map
         ("C-k" . helm-buffer-run-kill-persistent))
  :config (custom-set-variables
           '(helm-buffers-fuzzy-matching t)))

(use-package helm-ring ; Helm commands for rings
  :ensure helm
  :bind (([remap yank-pop] . helm-show-kill-ring)
         ("C-h SPC"    . helm-all-mark-rings)))

;;=== Projects & Navigation ====================================================
(use-package helm-imenu ; Imenu through helm
  :ensure helm
  :bind (("C-c n i" . helm-imenu-in-all-buffers)
         ("C-c n t" . helm-imenu))
  :config (custom-set-variables
           '(helm-imenu-fuzzy-match t)
           '(helm-imeu-execute-action-at-once-if-one nil)))

(use-package helm-register ; Display registers with helm
  :ensure helm
  :bind ([remap insert-register] . helm-register))

(use-package helm-bookmarks ; List bookmarks with Helm
  :ensure helm
  :bind ("C-x r l" . helm-filtered-bookmarks))

(use-package helm-pages ; Move between pages in current buffer with helm
  :ensure t
  :bind ("C-c n P" . helm-pages))

(use-package helm-projectile ; Projectile through helm
  :ensure t
  :after projectile
  :init (helm-projectile-on)
  :config (custom-set-variables
           '(projectile-switch-project-action #'helm-projectile)))

(use-package ace-jump-helm-line ; Ace-jump to a candidate
  :ensure t
  :after helm
  :bind ("C-'" . ace-jump-helm-line)
  :config (custom-set-variables
           '(ace-jump-helm-line-default-action 'select)))

;;=== Programming ==============================================================
(use-package helm-eval ; Evaluate expressions with helm
  :ensure helm
  :bind (("C-c h M-:" . helm-eval-expression-with-eldoc)
         ("C-c h *"   . helm-calcul-expression)))

(use-package helm-elisp ; Helm commands for Emacs Lisp
  :ensure helm
  :bind (("C-c h a" . helm-apropos)
         ("C-c h l" . helm-locate-library))
  :config (custom-set-variables
           '(helm-apropos-fuzzy-match t)
           '(helm-lisp-fuzzy-completion t)))

(use-package helm-flycheck ; Show flycheck errors with helm
  :ensure t
  :bind ("C-c h h f" . helm-flycheck))

;;=== Help =====================================================================
(use-package helm-info ; Helm tools for Info
  :ensure helm
  :bind (("C-c h h e" . helm-info-emacs)
         ("C-c h h i" . helm-info-at-point)))

(use-package helm-man ; Man pages through helm
  :ensure helm
  :bind ("C-c h m" . helm-man-woman))

(use-package helm-descbinds ; Describing keybindings through helm
  :ensure t
  :bind ("C-c h h d" . helm-descbinds))

(use-package helm-descbinds-modes ; Describing modes through helm
  :ensure t
  :bind ("C-c h d" . helm-describe-modes))

;;=== Search ===================================================================
(use-package helm-regex ; Helm regex tools
  :ensure helm
  :bind (("C-c M-o" . helm-occur)
         ([remap occur] . helm-occur)
         ("C-c h o" . helm-multi-occur)))

(use-package helm-swoop ; List matching lines in another buffer
  :ensure t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :config (progn
            ;; When doing isearch, hand the word over to helm-swoop
            (bind-key "M-i" #'helm-swoop-from-isearch isearch-mode-map)
            ;; From helm-swoop to helm-multi-swoop-all
            (bind-key "M-i" #'helm-multi-swoop-all-from-helm-swoop
                      helm-swoop-map)

            ;; Move up and down like isearch
            (bind-keys :map helm-swoop-map
                       ("C-r" . helm-previous-line)
                       ("C-s" . helm-next-line))

            (bind-keys :map helm-multi-swoop-map
                       ("C-r" . helm-previous-line)
                       ("C-s" . helm-next-line))

            (custom-set-variables
             '(helm-multi-swoop-edit-save t) ; save when done with editing
             '(helm-swoop-split-with-multiple-windows nil)
             '(helm-swoop-split-direction 'split-window-vertically)
             '(helm-swoop-speed-or-color nil)
             '(helm-swoop-move-to-line-cycle t)
             '(helm-swoop-use-line-number-face t))

            (defvar helm-c-source-swoop-match-functions
              '(helm-mm-exact-match
                helm-mm-match
                helm-fuzzy-match))

            (custom-set-variables
             `(helm-c-source-swoop-search-functions (helm-mm-exact-search
                                                     helm-mm-search
                                                     helm-candidates-in-buffer-search-default-fn
                                                     helm-fuzzy-search)))))

(use-package helm-ag ; Helm frontend for ag
  :if (executable-find "ag")
  :bind ("C-c h s" . helm-do-ag)
  :config (custom-set-variables
           '(helm-ag-fuzzy-match t)
           '(helm-ag-insert-at-point 'symbol)))

;;=== Completion ===============================================================
(use-package helm-company ; Show company candidates through helm
  :ensure t
  :after company
  :config (progn
            ;; Use company for completion
            (bind-key [remap completion-at-point] #'helm-company company-mode-map)
            (bind-key "C-:" #'helm-company company-mode-map)
            (bind-key "C-:" #'helm-company company-active-map)))

(use-package helm-c-yasnippet ; Helm source for Yasnippet
  :ensure t
  :after yasnippet
  :init (bind-key "C-c h y" #'helm-yas-complete)
  :config (custom-set-variables '(helm-yas-space-match-any-greedy t)))

;;=== Languages ================================================================
(use-package helm-flyspell ; Use flyspell with helm
  :ensure t
  :bind ("C-c h f" . helm-flyspell-correct))

(use-package helm-ispell ; Complete word using Ispell with helm
  :ensure t
  :bind (([remap ispell-word] . helm-ispell)
         ("C-c h i"           . helm-ispell)))

;;=== Input ====================================================================
(use-package helm-color ; Input colors with helm
  :ensure helm
  :bind ("C-c h c" . helm-colors))

(use-package helm-unicode ; Browse unicode characters by name
  :ensure t
  :bind ("C-c h 8" . helm-unicode))

;;=== Shell Integration ========================================================
(use-package helm-shell ; Manage shells/terms with helm
  :ensure helm
  :defer t
  :init (progn
          ;; Shell history
          (add-hook 'eshell-mode-hook
                    (lambda ()
                      (bind-key "C-c C-l"
                                #'helm-eshell-history
                                eshell-mode-map)))

          (bind-key "C-c C-l" #'helm-comint-input-ring shell-mode-map)

          ;; Completion with helm
          (add-hook 'eshell-mode-hook
                    (lambda ()
                      (bind-key [remap eshell-pcomplete]
                                'helm-esh-pcomplete
                                eshell-mode-map)))))

;;=== VC Tools =================================================================
(use-package helm-gitignore ; Generate .gitignore files with gitignore.io
  :ensure t
  :bind ("C-c h v g" . helm-gitignore))

;;=== Extra Utilities ==========================================================
(use-package helm-external ; Run external commands through helm
  :ensure helm
  :bind ("C-c h x" . helm-run-external-command))

(provide 'j-helm)
