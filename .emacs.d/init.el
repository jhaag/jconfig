;;=== Package Management Setup =================================================
;; Don't autoload packages and initialize the package authorities
;; we want to pull from
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; install use-package if it isn't already
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; at compile time, add use package, because we can be sure that
;; it is installed at that point
(eval-when-compile
  (require 'use-package))
;; Add the following line if using :diminish with use-package
;;(require 'diminish)
;; Add the following line if using :bind with use-package
;;(require 'bind-key)

;;=== Loading Local Dependencies ===============================================
;; Add all local .el files to the emacs load path
(load "~/jconfig/.emacs.d/packages/load-directory.el")
(load-directory "~/jconfig/.emacs.d/packages/")
(load-directory "~/jconfig/.emacs.d/utilities/")

;;=== Packages =================================================================
(load "~/jconfig/.emacs.d/packages.el")

;;=== General Configurations ===================================================
(global-linum-mode t)
(column-number-mode t)

;;=== Backups ==================================================================
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
	       (> (- current (float-time (nth 5 (file-attributes file))))
		  week))
      (message "%s" file)
      (delete-file file))))
