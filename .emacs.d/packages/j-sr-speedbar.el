(use-package sr-speedbar
  :ensure t
  :init (custom-set-variables
         '(sr-speedbar-right-side nil))
  :bind ("C-M-? p" . sr-speedbar-toggle))

(provide 'j-sr-speedbar)
