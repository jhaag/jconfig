;;=== Custom Major Mode Associations ===========================================
(add-to-list 'auto-mode-alist '("^\\.\\w.*$" . prog-mode))

;;=== Haskell Mode =============================================================
;; TODO: Actually learn how to use this, and also get a really nice haskell
;;       setup going
(use-package haskell
  :ensure haskell-mode
  :defer t)
