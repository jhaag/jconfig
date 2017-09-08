(use-package function-args
  :disabled t
  :diminish function-args
  :config (progn
            (custom-set-variables
             '(moo-select-method 'helm-fuzzy)
             '(fa-insert-method 'name-and-parens-and-hint))
            (fa-config-default)
            (unbind-key "M-o"   function-args-mode-map)
            (unbind-key "M-i"   function-args-mode-map)
            (unbind-key "M-n"   function-args-mode-map)
            (unbind-key "M-h"   function-args-mode-map)
            (unbind-key "M-u"   function-args-mode-map)
            (unbind-key "M-j"   function-args-mode-map)
            (unbind-key "C-M-j" function-args-mode-map)
            (unbind-key "C-M-k" function-args-mode-map)
            (bind-key   "C-j" 'moo-jump-local function-args-mode-map)))

(provide 'j-function-args)
