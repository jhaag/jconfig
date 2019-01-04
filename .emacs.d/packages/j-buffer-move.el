(use-package buffer-move
  :ensure t
  :preface (progn
             (defun j/2-windows-vertical-to-horizontal ()
               "Swap from a vertical to a horizontal orientation"
               (let ((buffers (mapcar 'window-buffer (window-list))))
                 (when (= 2 (length buffers))
                   (delete-other-windows)
                   (set-window-buffer (split-window-horizontally) (cadr buffers)))))

             (defun j/2-windows-horizontal-to-vertical ()
               "Swap from a horizontal to a vertical orientation"
               (let ((buffers (mapcar 'window-buffer (window-list))))
                 (when (= 2 (length buffers))
                   (delete-other-windows)
                   (set-window-buffer (split-window-vertically) (cadr buffers)))))

             (defun j/2-windows-switch-orientation ()
               "Swap from a vertical to a horizontal orientation, or vice versa"
               (interactive)
               (if (or (window-in-direction 'above)
                       (window-in-direction 'below))
                   (j/2-windows-vertical-to-horizontal)
                 (j/2-windows-horizontal-to-vertical)))

             (defun j/buf-send-up ()
               "Send the currently selected buffer up, and retain the current focus"
               (interactive)
               (buf-move-up)
               (select-window (windmove-find-other-window 'down)))

             (defun j/buf-send-down ()
               "Send the currently selected buffer down, and retain the current focus"
               (interactive)
               (buf-move-down)
               (select-window (windmove-find-other-window 'up)))

             (defun j/buf-send-left ()
               "Send the currently selected buffer left, and retain the current focus"
               (interactive)
               (buf-move-left)
               (select-window (windmove-find-other-window 'right)))

             (defun j/buf-send-right ()
               "Send the currently selected buffer right, and retain the current focus"
               (interactive)
               (buf-move-right)
               (select-window (windmove-find-other-window 'left))))
  :init (progn
          (add-hook 'emacs-startup-hook 'j/2-windows-vertical-to-horizontal)
          (global-unset-key (kbd "C-x o")))
  :bind (("C-x <up>"          . buf-move-up)
         ("C-x <down>"        . buf-move-down)
         ("C-x <left>"        . buf-move-left)
         ("C-x <right>"       . buf-move-right)
         ;("M-<up>"            . j/buf-send-up)
         ;("M-<down>"  	      . j/buf-send-down)
         ;("M-<left>"          . j/buf-send-left)
         ;("M-<right>"         . j/buf-send-right)
         ;("M-<return>"        . j/2-windows-switch-orientation)
         ("C-<tab>"           . other-window)
         ("C-S-<iso-lefttab>" . previous-multiframe-window)))

(provide 'j-buffer-move)
