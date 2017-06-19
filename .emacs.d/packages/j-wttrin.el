(use-package wttrin
  :if window-system
  :ensure t
  :defer t
  :bind ("M-? w" . wttrin)
  :config (custom-set-variables
           '(wttrin-default-cities '("" "02148" "02139" "Malden" "Lexington" "Cambridge"))
           '(wttrin-default-accept-language '("Accept-Language" . "en"))))

(provide 'j-wttrin)
