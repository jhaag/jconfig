(use-package nael
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.lean\\'" . nael-mode))
  :hook
  ((nael-mode . abbrev-mode)
   (nael-mode . eglot-ensure))
  :config
  ;; Custom keybindings can be added here
  ;; Example: (keymap-set nael-mode-map "C-c ." #'highlight-symbol-at-point)
  )

(provide 'j-lean)
