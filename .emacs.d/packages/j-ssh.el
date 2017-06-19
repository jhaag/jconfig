(use-package ssh
  :defer t
  :config (progn
	    (custom-set-variables
	     '(ssh-directory-tracking-mode t))
	    (add-hook 'ssh-mode-hook (lambda ()
				       (shell-dirtrack-mode t)
				       (setq dirtrackp nil)))))

(provide 'j-ssh)
