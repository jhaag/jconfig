;;=== Custom Major Mode Associations ===========================================
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(add-to-list 'auto-mode-alist '("^\\.\\w.*$" . prog-mode))

(require 'j-asm)
(require 'j-haskell)
(require 'j-org)
(require 'j-js)
(require 'j-rust)
