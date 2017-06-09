;;=== Loading dependencies =======================================
;; Add custom themes to path
(load "~/jconfig/.emacs.d/packages/load-directory.el")
(load-directory "~/jconfig/.emacs.d/packages/")
(load-directory "~/jconfig/.emacs.d/utilities/")
(add-to-list 'custom-theme-load-path "~/jconfig/.emacs.d/themes/")

;;=== General Configurations =====================================
(global-linum-mode t)
(column-number-mode t)

;;=== Backups ====================================================
;; Place all backup and autosave files in ~/.emacs.d/
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 '(initial-frame-alist '((fullscreen . maximized)))
 '(inhibit-startup-screen t))

;; Create the autosave and backup dirs if necessary, since emacs won't
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)

;; Purge old backups over time to save space
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files "~/.emacs.d/backups/" t))
    (when (and (backup-file-name-p file)
	       (> (- current (float-time (fifth (file-attributes file))))
		  week))
      (message "%s" file)
      (delete-file file))))
