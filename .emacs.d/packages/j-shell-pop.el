; Add support for 256 colors to ansi-term
(use-package eterm-256color
  :ensure t
  :config
  (add-hook 'term-mode-hook #'eterm-256color-mode))

(use-package shell-pop
  :ensure t
  :after (xterm-color eterm-256color)
  :bind ([f1] . shell-pop)
  :custom
  (shell-pop-window-size 40)
  (shell-pop-full-span t)
  (shell-pop-window-position "bottom")
  (shell-pop-default-directory (getenv "HOME"))
  (shell-pop-autocd-to-working-dir nil)
  :config
  (shell-pop--set-shell-type 'ansi-term '("ansi-term"
                                          "*ansi-term*"
                                          (lambda ()
                                            (ansi-term shell-pop-term-shell))))
  (add-hook 'shell-pop-in-hook
            (lambda ()
              (setq shell-pop-default-directory default-directory)))
  (add-hook 'shell-pop-in-after-hook
            (lambda ()
              (set-process-query-on-exit-flag (get-process "*ansi-term*") nil))))

(provide 'j-shell-pop)
