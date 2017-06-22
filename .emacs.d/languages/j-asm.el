(require 'asm-mode)

(add-hook 'asm-mode-hook (lambda ()
                           (electric-indent-mode -1)
                           (setq tab-stop-list (number-sequence 2 120 2))))

(provide 'j-asm)
