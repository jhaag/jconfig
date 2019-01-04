;;; Rather than tangling a .org file into a .el file, and then copying it into
;;; my ~/.emacs.d/ folder, I simply have a stub which will load my .org files
;;; directly using `org-babel-load-file'.
;;;
;;; For documentation, refer to my init.org file in ~/jconfig

;; This tweak fixes garbage collection issues during initialization which speeds startup up.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defconst startup/emacs-start-time (current-time))
(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil
      message-log-max 16384)

(defun startup/revert-file-name-handler-alist ()
    (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1)
  (garbage-collect))

;; These hooks will revert our tweaks as soon as emacs starts up. It will also
;; garbage collect to clean up after startup.
(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc t) ; t passed to ensure this runs last

;; This hook will run as soon as emacs starts; we exclude the last argument to
;; ensure that this is the first hook that is run upon startup.
(add-hook 'emacs-startup-hook
          `(lambda ()
             (message "Emacs ready in %s with %d garbage collections."
                      (format "%.2f seconds"
                              (float-time
                               (time-subtract (current-time) startup/emacs-start-time)))
                      gcs-done)))

(require 'package)
(require 'cl)

(setq package-enable-at-startup nil)

(eval-when-compile
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/"))
  ;;(add-to-list 'package-archives
  ;;             ("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("marmalade" . "https://marmalade-repo.org/packages/"))
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives
               '("gnu" . "https://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives
               '("org" . "https://orgmode.org/elpa/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t
	use-package-always-defer t))
(require 'diminish)

(setq load-prefer-newer t)
(use-package auto-compile
  :defer nil
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(when (file-readable-p "~/jconfig/.emacs.d/org-confs/init.org")
  (org-babel-load-file (expand-file-name "~/jconfig/.emacs.d/org-confs/init.org")))
