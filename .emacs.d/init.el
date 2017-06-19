;;=== Pre Initialization =======================================================
(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq message-log-max 16384)

(eval-and-compile
  (defconst jconfig-path "~/jconfig/.emacs.d/")

  (mapc #'(lambda (path)
            (add-to-list 'load-path
                         (expand-file-name path jconfig-path)))
        '("packages" "languages" "")))

;;=== Package Management Setup =================================================
;; Don't autoload packages
;; initialize the package authorities we want to pull from
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
(require 'diminish)
(require 'bind-key)

;;=== Loading Organized Configurations =========================================
(load "packages")
(load "languages")

;;=== General Configurations ===================================================
;; Remove 'beginner' emacs stuff from appearing
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

;; Setup column numbers and line numbers
(add-hook 'prog-mode-hook 'linum-mode)
(column-number-mode t)

;; Setup better parentheses
(custom-set-variables '(show-paren-mode t)
                      '(show-paren-delay 0))

;; Setup inferential tab-replacement
(defun infer-indentation-style ()
  ;; if the current source file uses tabs, use tabs
  ;; if the current source file uses spaces, use spaces
  ;; otherwise, default to the current indent-tabs-mode
  (let ((space-count (how-many "^ "  (point-min) (point-max)))
	(tab-count   (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (custom-set-variables '(indent-tabs-mode nil)))
    (if (> tab-count space-count) (custom-set-variables '(indent-tabs-mode t)))))

(add-hook 'prog-mode-hook (lambda ()
			    (custom-set-variables '(indent-tabs-mode nil))
			    (infer-indentation-style)))

;;=== Backups ==================================================================
;; Place all backup and autosave files in ~/.emacs.d/
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 '(initial-frame-alist '((fullscreen . maximized))))

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

;;=== Post Initialization ======================================================
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
