(use-package git-gutter+
  :ensure t
  :init (progn
	  (use-package git-modes
	    :ensure t
	    :defer t)
          ;;(use-package gitignore-mode
          ;;  :ensure t
          ;;  :defer t)
          ;;(use-package gitconfig-mode
          ;;  :ensure t
          ;;  :defer t)
          ;;(use-package gitattributes-mode
          ;;  :disabled t
          ;;  :defer t)
          (add-hook 'prog-mode-hook 'git-gutter+-mode))
  :bind (("C-x g g" . git-gutter+-mode)
         :map git-gutter+-mode-map
              ("C-x n"   . git-gutter+-next-hunk)
              ("C-x p"   . git-gutter+-previous-hunk)
              ("C-x v =" . git-gutter+-show-hunk))
  ;;:config (progn
  ;;          (use-package git-gutter-fringe+
  ;;            :ensure t
  ;;            :config (progn
  ;;                      ;; Currently hard-coded to be the solarized-yellow color
  ;;                      (set-face-attribute 'git-gutter-fr+-modified nil :foreground "#b58900")
  ;;                      (fringe-helper-define 'git-gutter-fr+-modified nil
  ;;                        "........"
  ;;                        "........"
  ;;                        "..XX...X"
  ;;                        ".XXXX.XX"
  ;;                        "XX.XXXX."
  ;;                        "X...XX.."
  ;;                        "........"
  ;;                        "........")))
  ;;          (custom-set-variables
  ;;           '(git-gutter+-hide-gutter t)))
  :diminish (git-gutter+-mode . "gg+"))

(provide 'j-git-gutter+)
