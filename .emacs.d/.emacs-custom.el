(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(coq-prog-args (quote ("-emacs" "-R" "~/dev/coq/cpdt/src" "Cpdt")))
 '(git-gutter+-hide-gutter t)
 '(helm-M-x-fuzzy-match t t)
 '(helm-ag-fuzzy-match t t)
 '(helm-ag-insert-at-point (quote symbol) t)
 '(helm-apropos-fuzzy-match t)
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
 '(helm-lisp-fuzzy-completion t)
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
 '(mu4e-attachment-dir "~/Downloads")
 '(mu4e-compose-format-flowed t)
 '(mu4e-compose-signature "~Jasper")
 '(mu4e-confirm-quit nil)
 '(mu4e-headers-date-format "%Y-%m-%d %H:%M")
 '(mu4e-headers-fields
   (quote
    ((:human-date . 25)
     (:flags . 6)
     (:from-or-to . 22)
     (:subject))))
 '(mu4e-maildir "~/Maildir")
 '(mu4e-update-interval 180)
 '(mu4e-user-mail-address-list (quote ("jasperhaag16@gmail.com" "jhaag@mit.edu")))
 '(mu4e-view-prefer-html t)
 '(mu4e-view-show-addresses t)
 '(org-hide-emphasis-markers t)
 '(org-mu4e-link-query-in-headers-mode nil t)
 '(org-stick-header-full-path (quote full) t)
 '(org-stick-header-outline-path-separator "|" t)
 '(org-sticky-header-always-show-header t t)
 '(org-sticky-header-full-path (quote full) t)
 '(org-sticky-header-heading-star "λ" t)
 '(org-sticky-header-outline-path-separator "|" t)
 '(org-sticky-header-prefix nil t)
 '(org-super-agenda-groups
   (quote
    ((:name "PROGRESS" :and
            (:deadline today :todo
                       ("DONE" "CANCELLED" "PHONE" "MEETING"))
            :order 0)
     (:order-multi
      (99
       (:name "EXORUST" :and
              (:tag "exorust" :not
                    (:todo
                     ("DONE" "CANCELLED" "PHONE" "MEETING"))
                    :not
                    (:tag
                     ("HOLD" "CANCELLED"))))
       (:name "HOLD" :tag "HOLD")))
     (:order-multi
      (0
       (:name "AGENDA" :time-grid t)
       (:name "HABITS" :habit t)))
     (:name "LATE" :deadline past :order 1)
     (:todo "NEXT" :order 2)
     (:name "TODAY" :and
            (:deadline today :not
                       (:todo
                        ("DONE" "CANCELLED" "PHONE" "MEETING")))
            :order 4)
     (:name "UPCOMING" :and
            (:deadline t :not
                       (:todo
                        ("DONE" "CANCELLED" "PHONE" "MEETING")))
            :order 5)
     (:order-multi
      (6
       (:name "IMPORTANT" :priority "A")
       (:name "LESS IMPORTANT" :priority<= "B")))
     (:name "CATCHALL" :todo t :order 8))) t)
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
    (org toc-org org-sticky-header rust-mode xref-js2 js2-refactor haskell-mode wttrin vagrant-tramp undo-tree symon sr-speedbar solarized-theme smooth-scrolling shell-pop eterm-256color multiple-cursors helm-gitignore helm-unicode helm-ispell helm-flyspell helm-c-yasnippet helm-company helm-swoop helm-describe-modes helm-descbinds helm-gtags helm-flycheck ace-jump-helm-line helm-projectile helm-pages git-gutter-fringe+ helm gitconfig-mode gitignore-mode git-gutter+ buffer-move use-package diminish)))
 '(projectile-switch-project-action (function helm-projectile))
 '(proof-electric-terminator-enable t)
 '(proof-keep-response-history t)
 '(proof-splash-enable nil)
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
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weigth bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "Source Sans Pro" :height 150 :weight normal)))))
