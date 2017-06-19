(use-package shell-pop
  :ensure t
  :defer t
  :bind ("M-+" . shell-pop)
  :config (progn
            (custom-set-variables
             '(shell-pop-shell-type "ansi-term"))
            (add-hook 'shell-pop-out-hook (lambda ()
                                            (delete-process "*shell-1*")))))

(provide 'j-shell-pop)
