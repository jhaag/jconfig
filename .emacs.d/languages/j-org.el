(use-package org
  :ensure t
  :delight org-mode "Org"
  :preface
  (defun j/org-src-buffer-name (org-buffer-name language)
    "Construct the buffer name for a source editing buffer. See `org-src--construct-edit-buffer-name'."
    (format "*%s" org-buffer-name))

  ;; Remove empty LOGBOOK drawers on clock out
  (defun j/remove-empty-drawer-on-clock-out ()
    (interactive)
    (save-excursion
      (beginning-of-line 0)
      (org-remove-empty-drawer-at "LOGBOOK" (point))))

  ;; Refile filtering
  ;; exclude DONE state tasks from refile targets
  (defun j/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  ;; === Focusing on Shit ======================================================
  (defun j/org-todo (arg)
    (interactive "p")
    (if (equal arg 4)
        (save-restriction
          (j/narrow-to-org-subtree)
          (org-show-todo-tree nil))
      (j/narrow-to-org-subtree)
      (org-show-todo-tree nil)))

  (defun j/narrow-to-org-subtree ()
    (widen)
    (org-narrow-to-subtree)
    (save-restriction
      (org-agenda-set-restriction-lock)))

  (defun j/narrow-to-subtree ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-with-point-at (org-get-at-bol 'org-hd-marker)
            (j/narrow-to-org-subtree))
          (when org-agenda-sticky
            (org-agenda-redo)))
      'j/narrow-to-org-subtree))

  (defun j/narrow-up-one-org-level ()
    (widen)
    (save-excursion
      (outline-up-heading 1 'invisible-ok)
      (j/narrow-to-org-subtree)))

  (defun j/get-pom-from-agenda-restriction-or-point ()
    (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
        (org-get-at-bol 'org-hd-marker)
        (and (equal major-mode 'org-mode) (point))
        org-clock-marker))

  (defun j/narrow-up-one-level ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-with-point-at (j/get-pom-from-agenda-restriction-or-point)
            (j/narrow-up-one-org-level))
          (org-agenda-redo))
      (j/narrow-up-one-org-level)))

  (defun j/narrow-to-org-project ()
    (widen)
    (save-excursion
      (j/find-project-task)
      (j/narrow-to-org-subtree)))

  (defun j/narrow-to-project ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-with-point-at (j/get-pom-from-agenda-restriction-or-point)
            (j/narrow-to-org-project)
            (save-excursion
              (j/find-project-task)
              (org-agenda-set-restriction-lock)))
          (org-agenda-redo)
          (beginning-of-buffer))
      (j/narrow-to-org-project)
      (save-restriction
        (org-agenda-set-restriction-lock))))

  (defvar j/project-list nil)

  (defun j/view-next-project ()
    (interactive)
    (let (num-project-left current-project)
      (unless (marker-position org-agenda-restrict-begin)
        (goto-char (point-min))
        ;; clear all of the existing markers on the list
        (while j/project-list
          (set-marker (pop j/project-list) nil))
        (re-search-forward "Tasks to Refile")
        (forward-visible-line 1))

      ;; build a new project marker list
      (unless j/project-list
        (while (< (point) (point-max))
          (while (and (< (point) (point-max))
                      (or (not (org-get-at-bol 'org-hd-marker))
                          (org-with-point-at (org-get-at-bol 'org-hd-marker)
                            (or (not (j/is-project-p))
                                (j/is-project-subtree-p)))))
            (forward-visible-line 1))
          (when (< (point) (point-max))
            (add-to-list 'j/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
          (forward-visible-line 1)))

      ;; Pop off the first marker on the list and display
      (setq current-project (pop j/project-list))
      (when current-project
        (org-with-point-at current-project
          (setq j/hide-scheduled-and-waiting-next-tasks nil)
          (j/narrow-to-project))
        ;; remove the marker
        (setq current-project nil)
        (org-agenda-redo)
        (beginning-of-buffer)
        (setq num-projects-left (length j/project-list))
        (if (> num-projects-left 0)
            (message "%s projects left to view" num-projects-left)
          (beginning-of-buffer)
          (setq j/hide-scheduled-and-waiting-next-tasks t)
          (error "All projects viewed.")))))
  
  (defun j/restrict-to-file-or-follow (arg)
    "Set agenda restriction to 'file (or with argument invoke follow mode).
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both."
    (interactive "p")
    (if (equal arg 4)
        (org-agenda-follow-mode)
      (widen)
      (j/set-agenda-restriction-lock 4)
      (org-agenda-redo)
      (beginning-of-buffer)))
  
  (defun j/widen ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-agenda-remove-restriction-lock)
          (when org-agenda-stick
            (org-agenda-redo)))
      (widen)))
  ;; === Agenda stuff ==========================================================
  (defun j/is-project-p ()
    "Any task with a todo keyword subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task has-subtask))))

  (defun j/is-project-subtree-p ()
    "Any task with a todo keyword that is in a project subtree. Callers of this function already widen the buffer view."
    (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                (point))))
      (save-excursion
        (j/find-project-task)
        (if (equal (point) task)
            nil
          t))))

  (defun j/is-task-p ()
    "Any task with a todo keyword and no subtask."
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task (not has-subtask)))))

  (defun j/is-subproject-p ()
    "Any task which is a subtask of another project."
    (let ((is-subproject)
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (while (and (not is-subproject) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq is-subproject t))))
      (and is-a-task is-subproject)))

  (defun j/list-sublevels-for-projects-indented ()
    "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
This is normally used by skipping functions where this variable is already local to the agenda."
    (if (marker-buffer org-agenda-restrict-begin)
        (setq org-tags-match-list-sublevels 'indented)
      (setq org-tags-match-list-sublevels nil))
    nil)

  (defun j/list-sublevels-for-projects ()
    "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
This is normally used by skipping functions where this variable is already local to the agenda."
    (if (marker-buffer org-agenda-restrict-begin)
        (setq org-tags-match-list-sublevels t)
      (setq org-tags-match-list-sublevels nil))
    nil)

  (defvar j/hide-scheduled-and-waiting-next-tasks t)

  (defun j/toggle-next-task-display ()
    (interactive)
    (setq j/hide-scheduled-and-waiting-next-tasks (not j/hide-scheduled-and-waiting-next-tasks))
    (when (equal major-mode 'org-agenda-mode)
      (org-agenda-redo))
    (message "%s WAITING and SCHEDULED NEXT Tasks" (if j/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

  (defun j/skip-stuck-projects ()
    "Skip trees that are stuck projects"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (j/is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next ))
              (save-excursion
                (forward-line 1)
                (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                  (unless (member "WAITING" (org-get-tags-at))
                    (setq has-next t))))
              (if has-next
                  nil
                next-headline)) ; a stuck project, has subtasks but no next task
          nil))))

  (defun j/skip-non-stuck-projects ()
    "Skip trees that are not stuck projects"
    ;; (j/list-sublevels-for-projects-indented)
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (j/is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next ))
              (save-excursion
                (forward-line 1)
                (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                  (unless (member "WAITING" (org-get-tags-at))
                    (setq has-next t))))
              (if has-next
                  next-headline
                nil)) ; a stuck project, has subtasks but no next task
          next-headline))))

  (defun j/skip-non-projects ()
    "Skip trees that are not projects"
    ;; (j/list-sublevels-for-projects-indented)
    (if (save-excursion (j/skip-non-stuck-projects))
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((j/is-project-p)
              nil)
             ((and (j/is-project-subtree-p) (not (j/is-task-p)))
              nil)
             (t
              subtree-end))))
      (save-excursion (org-end-of-subtree t))))

  (defun j/skip-non-tasks ()
    "Show non-project tasks. Skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((j/is-task-p)
          nil)
         (t
          next-headline)))))

  (defun j/skip-project-trees-and-habits ()
    "Skip trees that are projects"
    (save-restriction
      (widen)
      (let ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((j/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         (t
          nil)))))

  (defun j/skip-projects-and-habits-and-single-tasks ()
    "Skip trees that are projects, tasks that are habits, single non-project tasks"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((org-is-habit-p)
          next-headline)
         ((and j/hide-scheduled-and-waiting-next-tasks
               (member "WAITING" (org-get-tags-at)))
          next-headline)
         ((j/is-project-p)
          next-headline)
         ((and (j/is-task-p) (not (j/is-project-subtree-p)))
          next-headline)
         (t
          nil)))))

  (defun j/skip-project-tasks-maybe ()
    "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (next-headline (save-excursion (or (outline-next-heading) (point-max))))
             (limit-to-project (marker-buffer org-agenda-restrict-begin)))
        (cond
         ((j/is-project-p)
          next-headline)
         ((org-is-habit-p)
          subtree-end)
         ((and (not limit-to-project)
               (j/is-project-subtree-p))
          subtree-end)
         ((and limit-to-project
               (j/is-project-subtree-p)
               (member (org-get-todo-state) (list "NEXT")))
          subtree-end)
         (t
          nil)))))

  (defun j/skip-project-tasks ()
    "Show non-project tasks. Skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((j/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         ((j/is-project-subtree-p)
          subtree-end)
         (t
          nil)))))

  (defun j/skip-non-project-tasks ()
    "Show project tasks. Skip project and sub-project tasks, habits, and loose non-project tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((j/is-project-p)
          next-headline)
         ((org-is-habit-p)
          subtree-end)
         ((and (j/is-project-subtree-p)
               (member (org-get-todo-state) (list "NEXT")))
          subtree-end)
         ((not (j/is-project-subtree-p))
          subtree-end)
         (t
          nil)))))

  (defun j/skip-projects-and-habits ()
    "Skip trees that are projects and tasks that are habits"
    (save-restriction
      (widen)
      (let ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((j/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         (t
          nil)))))

  (defun j/skip-non-subprojects ()
    "Skip trees that are not projects"
    (let ((next-headline (save-excursion (outline-next-heading))))
      (if (j/is-subproject-p)
          nil
        next-headline)))
  ;; === Archiving =============================================================
  (defun j/skip-non-archivable-tasks ()
    "Skip trees that are not available for archiving"
    (save-restriction
      (widen)
      ;; consider only tasks with done todo headings as archivable candidates
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
            (subtree-end (save-excursion (org-end-of-subtree t))))
        (if (member (org-get-todo-state) org-todo-keywords-1)
            (if (member (org-get-todo-state) org-done-keywords)
                (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                       (a-month-ago (* 60 60 24 (+ daynr 1)))
                       (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                       (this-month (format-time-string "%Y-%m-" (current-time)))
                       (subtree-is-current (save-excursion
                                             (forward-line 1)
                                             (and (< (point) subtree-end)
                                                  (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                  (if subtree-is-current
                      subtree-end ; Has a date in this month or last month, skip it
                    nil)) ; available to archive
              (or subtree-end (point-max)))
          next-headline))))
  ;; === Clocking ==============================================================
  (defun j/clock-in-to-next (kw)
    "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
    (when (not (and (boundp 'org-capture-mode) org-capture-mode))
      (cond
       ((and (member (org-get-todo-state) (list "TODO"))
             (j/is-task-p))
        "NEXT")
       ((and (member (org-get-todo-state) (list "NEXT"))
             (j/is-project-p))
        "TODO"))))

  (defun j/find-project-task ()
    "Move point to the parent (project) task if any"
    (save-restriction
      (widen)
      (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (goto-char parent-task)
        parent-task)))

  (defun j/punch-in (arg)
    "Start continuous clocking and set the default task to the selected task.
If no task is selected set the Organization task as the default task."
    (interactive "p")
    (setq j/keep-clock-running t)
    (if (equal major-mode 'org-agenda-mode)
        ;;
        ;; We're in the agenda
        ;;
        (let* ((marker (org-get-at-bol 'org-hd-marker))
               (tags (org-with-point-at marker (org-get-tags-at))))
          (if (and (eq arg 4) tags)
              (org-agenda-clock-in '(16))
            (j/clock-in-organization-task-as-default)))
      ;;
      ;; We're not in the agenda
      ;;
      (save-restriction
        (widen)
        ;; Find the tags on the current task
        (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
            (org-clock-in '(16))
          (j/clock-in-organization-task-as-default)))))

  (defun j/punch-out ()
    (interactive)
    (setq j/keep-clock-running nil)
    (when (org-clock-is-active)
      (org-clock-out))
    (org-agenda-remove-restriction-lock))

  (defun j/clock-in-default-task ()
    (save-excursion
      (org-with-point-at org-clock-default-task
        (org-clock-in))))

  (defun j/clock-in-parent-task ()
    "Move point to the parent (project) task if any and clock in"
    (let ((parent-task))
      (save-excursion
        (save-restriction
          (widen)
          (while (and (not parent-task) (org-up-heading-safe))
            (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
              (setq parent-task (point))))
          (if parent-task
              (org-with-point-at parent-task
                (org-clock-in))
            (when j/keep-clock-running
              (j/clock-in-default-task)))))))

  (defvar j/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

  (defun j/clock-in-organization-task-as-default ()
    (interactive)
    (org-with-point-at (org-id-find j/organization-task-id 'marker)
      (org-clock-in '(16))))

  (defun j/clock-out-maybe ()
    (when (and j/keep-clock-running
               (not org-clock-clockin-in)
               (marker-buffer org-clock-default-task)
               (not org-clock-resolving-clocks-due-to-idleness))
      (j/clock-in-parent-task)))
  ;; === Misc ==================================================================
  (defun j/hide-other ()
    (interactive)
    (save-excursion
      (org-back-to-heading 'invisible-ok)
      (hide-other)
      (org-cycle)
      (org-cycle)
      (org-cycle)))

  (defun j/set-truncate-lines ()
    "Toggle value of truncate-lines and refresh window display."
    (interactive)
    (setq truncate-lines (not truncate-lines))
    ;; now refresh window display (an idiom from simple.el):
    (save-excursion
      (set-window-start (selected-window)
                        (window-start (selected-window)))))

  (defun j/org-files-list ()
    (delq nil
          (mapcar (lambda (buffer)
                    (buffer-file-name buffer))
                  (org-buffer-list 'files t))))
  
  (defun j/make-org-scratch ()
    (interactive)
    (find-file "/tmp/publish/scratch.org")
    (gnus-make-directory "/tmp/publish"))

  (defun j/switch-to-scratch ()
    (interactive)
    (switch-to-buffer "*scratch*"))
  :hook
  ((org-mode                . org-sticky-header-mode)
   (org-mode                . toc-org-enable)
   (org-mode                . org-indent-mode)
   (org-mode                . visual-line-mode)
   (org-agenda-mode         . (lambda ()
                                (progn
                                  (org-defkey org-agenda-mode-map
                                              "F"
                                              'j/restrict-to-file-or-follow)
                                  (org-defkey org-agenda-mode-map
                                              "N"
                                              'j/narrow-to-subtree)
                                  (org-defkey org-agenda-mode-map
                                              "P"
                                              'j/narrow-to-project)
                                  (org-defkey org-agenda-mode-map
                                              "W"
                                              (lambda ()
                                                (interactive)
                                                (setq j/hide-scheduled-and-waiting-next-tasks t)
                                                (j/widen)))
                                  (org-defkey org-agenda-mode-map
                                              "U"
                                              'j/narrow-up-one-level)
                                  (org-defkey org-agenda-mode-map
                                              "V"
                                              'j/view-next-project))))
   (org-clock-out           . j/remove-empty-drawer-on-clock-out)
   (org-clock-out           . j/clock-out-maybe)
   (org-babel-after-execute . org-display-inline-images))
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))
  :bind
  (("<f12>"     . 'org-agenda)
   ("<f5>"      . 'j/org-todo)
   ("S-<f5>"    . 'j/widen)
   ("<f7>"      . 'j/set-truncate-lines)
   ("<f8>"      . 'org-cycle-agenda-files)
   ("<f9> <f9>" . 'j/show-org-agenda)
   ("<f9> c"    . 'calendar)
   ("<f9> h"    . 'j/hide-other)
   ("<f9> n"    . 'j/toggle-next-task-display)
   
   ("<f9> I"    . 'j/punch-in)
   ("<f9> O"    . 'j/punch-out)
   
   ("<f9> o"    . 'j/make-org-scratch)
   ("<f9> s"    . 'j/switch-to-scratch)
   
   ("<f9> t"    . 'j/insert-inactive-timestamp)
   ("<f9> T"    . 'j/toggle-insert-inactive-timestamp)

   ("<f9> v"    . 'visible-mode)
   ("<f9> l"    . 'org-toggle-link-display)
   ("C-<f9>"    . 'previous-buffer)
   ("M-<f9>"    . 'org-toggle-inline-images)
   ("C-x n r"   . 'narrow-to-region)
   ("C-<f10>"   . 'next-buffer)
   ("<f11>"     . 'org-clock-goto)
   ("C-<f11>"   . 'org-clock-in)
   
   ("C-c c"     . 'org-capture)
   ("C-c l"     . 'org-store-link))
  :config
  (setq-default
   ;; Archive Stuff
   org-archive-mark-done nil
   org-archive-location "%s_archive::* Archived Tasks"
   ;; Agenda Stuff
   org-agenda-files (quote ("~/Dropbox/org"))
   org-agenda-window-setup 'current-window

   ;; I can look at the week view deliberately if I want
   org-agenda-span 'day
   org-agenda-dim-blocked-tasks nil
   org-agenda-compact-blocks t

   ;; disable standard 'stuck project' view
   org-stuck-projects (quote ("" nil nil ""))

   org-agenda-custom-commands (quote (("N" "Notes" tags "NOTE"
                                       ((org-agenda-overriding-header "Notes")
                                        (org-tags-match-list-sublevels t)))
                                      ("h" "Habits" tags-todo "STYLE=\"habit\""
                                       ((org-agenda-overriding-header "Habits")
                                        (org-agenda-sorting-strategy
                                         '(todo-state-down effort-up category-keep))))
                                      (" " "Agenda"
                                       ((agenda "" nil)
                                        (tags "REFILE"
                                              ((org-agenda-overriding-header "Tasks to Refile")
                                               (org-tags-match-list-sublevels nil)))
                                        (tags-todo "-CANCELLED/!"
                                                   ((org-agenda-overriding-header "Stuck Projects")
                                                    (org-agenda-skip-function 'j/skip-non-stuck-projects)
                                                    (org-agenda-sorting-strategy '(category-keep))))
                                        (tags-todo "-HOLD-CANCELLED/!"
                                                   ((org-agenda-overriding-header "Projects")
                                                    (org-agenda-skip-function 'j/skip-non-projects)
                                                    (org-tags-match-list-sublevels 'indented)
                                                    (org-agenda-sorting-strategy '(category-keep))))
                                        (tags-todo "-CANCELLED/!NEXT"
                                                   ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                                          (if j/hide-scheduled-and-waiting-next-tasks
                                                                                              ""
                                                                                            " (including WAITING and SCHEDULED tasks)")))
                                                    (org-agenda-skip-function 'j/skip-projects-and-habits-and-single-tasks)
                                                    (org-tags-match-list-sublevels t)
                                                    (org-agenda-todo-ignore-scheduled j/hide-scheduled-and-waiting-next-tasks)
                                                    (org-agenda-todo-ignore-deadlines j/hide-scheduled-and-waiting-next-tasks)
                                                    (org-agenda-todo-ignore-with-date j/hide-scheduled-and-waiting-next-tasks)
                                                    (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
                                        (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                                                   ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                                          (if j/hide-scheduled-and-waiting-next-tasks
                                                                                              ""
                                                                                            " (including WAITING and SCHEDULED tasks)")))
                                                    (org-agenda-skip-function 'j/skip-non-project-tasks)
                                                    (org-agenda-todo-ignore-scheduled j/hide-scheduled-and-waiting-next-tasks)
                                                    (org-agenda-todo-ignore-deadlines j/hide-scheduled-and-waiting-next-tasks)
                                                    (org-agenda-todo-ignore-with-date j/hide-scheduled-and-waiting-next-tasks)
                                                    (org-agenda-sorting-strategy '(category-keep))))
                                        (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                                                   ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                                          (if j/hide-scheduled-and-waiting-next-tasks
                                                                                              ""
                                                                                            " (including WAITING and SCHEDULED tasks)")))
                                                    (org-agenda-skip-function 'j/skip-project-tasks)
                                                    (org-agenda-todo-ignore-scheduled j/hide-scheduled-and-waiting-next-tasks)
                                                    (org-agenda-todo-ignore-deadlines j/hide-scheduled-and-waiting-next-tasks)
                                                    (org-agenda-todo-ignore-with-date j/hide-scheduled-and-waiting-next-tasks)
                                                    (org-agenda-sorting-strategy '(category-keep))))
                                        (tags-todo "-CANCELLED+WAITING|HOLD/!"
                                                   ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                                          (if j/hide-scheduled-and-waiting-next-tasks
                                                                                              ""
                                                                                            " (including WAITING and SCHEDULED tasks)")))
                                                    (org-agenda-skip-function 'j/skip-non-tasks)
                                                    (org-tags-match-list-sublevels nil)
                                                    (org-agenda-todo-ignore-scheduled j/hide-scheduled-and-waiting-next-tasks)
                                                    (org-agenda-todo-ignore-deadlines j/hide-scheduled-and-waiting-next-tasks)))
                                        (tags "-REFILE/"
                                              ((org-agenda-overriding-header "Tasks to Archive")
                                               (org-agenda-skip-function 'j/skip-non-archivable-tasks)
                                               (org-tags-match-list-sublevels nil))))
                                       nil)))


                                     
   ;; Todo Stuff
   org-capture-templates (quote (("t" "todo" entry (file "~/Dropbox/org/refile.org")
                                  "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                                 ("r" "respond" entry (file "~/Dropbox/org/refile.org")
                                  "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                                 ("n" "note" entry (file "~/Dropbox/org/refile.org")
                                  "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                                 ("j" "Journal" entry (file+datetree "~/Dropbox/org/diary.org")
                                  "* %?\n%U\n" :clock-in t :clock-resume t)
                                 ("m" "Meeting" entry (file "~/Dropbox/org/refile.org")
                                  "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                                 ("p" "Phone call" entry (file "~/Dropbox/org/refile.org")
                                  "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                                 ("h" "Habit" entry (file "~/Dropbox/org/refile.org")
                                  "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n)")))
   org-log-done 'time
   org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)"
                                       "|"
                                       "DONE(d)")
                             (sequence "WAITING(w@/!)" "HOLD(h@/!)"
                                       "|"
                                       "CANCELLED(c@/!)" "PHONE" "MEETING")))
   org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                  ("NEXT" :foreground "blue" :weight bold)
                                  ("DONE" :foreground "forest green" :weight bold)
                                  ("WAITING" :foreground "orange" :weight bold)
                                  ("HOLD" :foreground "magenta" :weight bold)
                                  ("CANCELLED" :foreground "forest green" :weight bold)
                                  ("MEETING" :foreground "forest green" :weight bold)
                                  ("PHONE" :foreground "forest green" :weight bold)))
   org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t))
                                        ("WAITING" ("WAITING" . t))
                                        ("HOLD" ("WAITING") ("HOLD" . t))
                                        (done ("WAITING") ("HOLD"))
                                        ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                        ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                        ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
   org-treat-S-cursor-todo-selection-as-state-change nil
   org-use-fast-todo-selection t ; this allows for C-c C-t KEY to be used for choosing the state
   ;; Refile stuff
   ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
   org-refile-targets (quote ((nil :maxlevel . 9)
                              (j/org-files-list :maxlevel . 9) ; to refile to open files.
                              (org-agenda-files :maxlevel . 9)))
   org-refile-use-outline-path t
   ;; needed to use helm for completion
   org-outline-path-complete-in-steps nil
   org-refile-allow-creating-parent-nodes (quote confirm)
   org-completion-use-ido nil
   org-refile-target-verify-function 'j/verify-refile-target
   ;; Babel stuff
   org-src-window-setup 'current-window
   org-src-fontify-natively t
   org-src-tab-acts-natively t
   org-confirm-babel-evaluate nil
   ;; Clocking stuff
   ;; Show lots of clocking history so it's easy to pick items off the C-F11 list
   org-clock-history-length 23
   org-clock-in-resume t
   org-clock-in-switch-to-state 'j/clock-in-to-next
   org-drawers (quote ("PROPERTIES" "LOGBOOK"))
   org-clock-into-drawer t
   org-clock-out-remove-zero-time-clocks t
   org-clock-out-when-done t
   org-clock-persist t
   org-clock-persist-query-resume nil
   org-clock-auto-clock-resolution (quote when-no-clock-is-running)
   org-clock-report-include-clocking-task t
   j/keep-clock-running nil
   ;; Misc stuff
   org-directory "~/Dropbox/org"
   org-default-notes-file "~/Dropbox/org/refile.org"
   org-ellipsis "…"
   org-return-follows-link t
   org-imenu-depth 3
   org-startup-folded nil
   org-startup-truncated nil)
  (advice-add 'org-src--construct-edit-buffer-name :override #'j/org-src-buffer-name)
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate))

(use-package org-sticky-header
  :ensure t
  :config
  (setq-default
   org-sticky-header-full-path 'full
   org-sticky-header-outline-path-separator " | "))

(use-package toc-org
  :ensure t
  :after org)

(provide 'j-org)
