(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(git-gutter+-hide-gutter t)
 '(helm-M-x-fuzzy-match t t)
 '(helm-ag-fuzzy-match t t)
 '(helm-ag-insert-at-point (quote symbol) t)
 '(helm-apropos-fuzzy-match t t)
 '(helm-buffers-fuzzy-matching t)
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-display-header-line nil)
 '(helm-ff-auto-update-initial-value t)
 '(helm-ff-auto-update-intial-value t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-newfile-prompt-p nil)
 '(helm-ff-search-library-in-sexp t)
 '(helm-ff-skip-boring-files t)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-prefix-key "g")
 '(helm-gtags-pulse-at-cursor t)
 '(helm-gtags-suggested-key-mapping t)
 '(helm-gtags-use-input-at-cursor t)
 '(helm-imenu-execute-action-at-once-if-one nil t)
 '(helm-imenu-fuzzy-match t t)
 '(helm-input-idle-delay 0.1)
 '(helm-lisp-fuzzy-completion t t)
 '(helm-mode-fuzzy-match t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-multi-swoop-edit-save t t)
 '(helm-multi-swoop-split-with-multiple-windows nil t)
 '(helm-recentf-fuzzy-match t t)
 '(helm-scroll-amount 8)
 '(helm-split-window-in-side-p t)
 '(helm-split-window-inside-p t)
 '(helm-swoop-move-to-line-cycle t t)
 '(helm-swoop-speed-or-color t t)
 '(helm-swoop-split-direction (quote split-window-vertically) t)
 '(helm-swoop-use-fuzzy-match t t)
 '(helm-swoop-use-line-number-face t t)
 '(helm-yas-space-match-any-greedy t)
 '(indent-tabs-mode nil)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(js-indent-level 4 t)
 '(js2-basic-offset 4 t)
 '(kill-whole-line t)
 '(mouse-yank-at-point t)
 '(org-agenda-files
   (quote
    ("~/dev/exorust/xo/xo.org" "/home/jhaag/Dropbox/org/diary.org" "/home/jhaag/Dropbox/org/refile.org" "/home/jhaag/Dropbox/org/todo.org")))
 '(org-hide-emphasis-markers t)
 '(org-stick-header-full-path (quote full) t)
 '(org-stick-header-outline-path-separator "|" t)
 '(org-sticky-header-always-show-header t t)
 '(org-sticky-header-full-path (quote full) t)
 '(org-sticky-header-heading-star "λ" t)
 '(org-sticky-header-outline-path-separator "|" t)
 '(org-sticky-header-prefix nil t)
 '(org-super-agenda-groups
   (quote
    ((:name "Habits" :habit t :order 0)
     (:order-multi
      (1
       (:name "Today's Work" :deadline past :and
              (:deadline today :time-grid t :not
                         (:todo
                          ("DONE" "CANCELLED" "PHONE" "MEETING"))))
       (:name "Today's Progress" :log close)))
     (:name "Important" :priority "A" :order 2)
     (:todo "NEXT" :order 3)
     (:name "DEADLINES" :and
            (:deadline t :not
                       (:todo
                        ("DONE" "CANCELLED" "PHONE" "MEETING")))
            :order 4)
     (:priority<= "B" :order 5)
     (:name "Catchall" :not
            (:todo
             ("DONE" "CANCELLED" "PHONE" "MEETING"))
            :order 6))) t)
 '(org-todo-keyword-faces
   (quote
    (("TODO" :foreground "lightblue" :weight bold)
     ("NEXT" :foreground "red" :weight bold)
     ("DONE" :foreground "forest green" :weight bold)
     ("WAITING" :foreground "orange" :weight bold)
     ("HOLD" :foreground "magenta" :weight bold)
     ("CANCELLED" :foreground "forest green" :weight bold)
     ("MEETING" :foreground "forest green" :weight bold)
     ("PHONE" :foreground "forest green" :weight bold))))
 '(package-selected-packages
   (quote
    (toc-org org-sticky-header rust-mode xref-js2 js2-refactor haskell-mode wttrin vagrant-tramp undo-tree symon sr-speedbar solarized-theme smooth-scrolling shell-pop eterm-256color multiple-cursors helm-gitignore helm-unicode helm-ispell helm-flyspell helm-c-yasnippet helm-company helm-swoop helm-describe-modes helm-descbinds helm-gtags helm-flycheck ace-jump-helm-line helm-projectile helm-pages git-gutter-fringe+ helm gitconfig-mode gitignore-mode git-gutter+ buffer-move use-package diminish)))
 '(projectile-switch-project-action (function helm-projectile))
 '(shell-pop-autocd-to-working-dir nil t)
 '(shell-pop-default-directory "/home/jhaag" t)
 '(shell-pop-full-span t t)
 '(shell-pop-window-position "bottom" t)
 '(shell-pop-window-size 40 t)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(sr-speedbar-right-side nil)
 '(symon-delay 1)
 '(symon-history-size 25 t)
 '(symon-monitors
   (quote
    (symon-current-time-monitor symon-linux-battery-monitor symon-linux-cpu-monitor symon-linux-memory-monitor symon-linux-network-rx-monitor symon-linux-network-tx-monitor)) t)
 '(symon-refresh-rate 0.5 t)
 '(symon-sparkline-type (quote bounded) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Inconsolata" :slant normal :weight normal :height 1.0 :width normal))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-done ((t (:foreground "Cyan" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-locked-comment-face fixed-pitch)))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weigth bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "Source Sans Pro" :height 150 :weight normal)))))
