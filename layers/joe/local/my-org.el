;;; my-org.el --- customization for org-mode

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'core-keybindings)
(require 'dash)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-clock)
(require 'org-habit)
(require 'org-drill)
(require 's)

;; Custom bindings
(spacemacs/set-leader-keys
  "aoj" 'org-clock-goto)

(setq org-directory "~/gdrive/org")

(setq org-default-notes-file "~/gdrive/org/refile.org")

(setq org-agenda-files '("~/gdrive/org/"))

(defvar my:org-journal-file "~/gdrive/org/journal.org"
  "The location of the journal file.")

(when (file-exists-p "~/gdrive/gorg")
  (add-to-list 'org-agenda-files "~/gdrive/gorg"))
(setq org-log-done t)

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)

;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; Change tasks to NOW when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks
;; with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

(setq org-clock-out-when-done '("DONE" "CANX" "WAIT"))

;; Save the running clock and all clock history when exiting Emacs, load it on
;; startup
(setq org-clock-persist t)

;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)

;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

;; We use a smarter definition for stuck projects
(setq org-stuck-projects (quote ("" nil nil "")))

(setq org-image-actual-width '(600))


(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (timeline . "  % s")
        ;; TODO: why doesn't the timestamp show up?
        (todo . " %i %-12:c %-4e %?-12t")
        (tags . " %i %-12:c %-4e %?-12t")
        (search . " %i %-12:c")))

;; Switch key bindings for org-agenda and org-attach.  It's easier to hit C-c
;; C-a rapidly.
(global-set-key (kbd "C-c C-a") #'org-agenda)
(defun my:org-keybindings ()
  (local-set-key (kbd "C-c C-a") #'org-agenda)
  (local-set-key (kbd "C-c a") #'org-attach))
(add-hook 'org-mode-hook 'my:org-keybindings)
(add-hook 'org-agenda-mode-hook 'my:org-keybindings)

(defvar bh/keep-clock-running nil)

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking.
ARG is the prefix argument.  One press means set the current task
as default.  Set the default task to the selected task.  If no
task is selected set the Organization task as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          ;; set it as default with 2 prefix args
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  "Stop clocking for the day."
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  "Clock in at the default task."
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in."
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
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun bh/clock-in-organization-task-as-default ()
  "Clock into the organization task."
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  "Maybe clock out."
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

;; Fontify code in code blocks
(setq org-src-fontify-natively t)


(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "NOW(o)"
                  "|" "DONE(d)")
        ;; The @/! means log a note when entering this state and log just
        ;; a timestamp when leaving this state
        (sequence "WAIT(w@/!)" "HOLD(h@/!)"
                  "|" "CANX(c@/!)")))

(setq org-tag-alist
      '(;; Elements of a group are mutually exclusive

        ;; Where type tags.
        (:startgroup . nil)
        ("comp" . ?c)
        ("errand" . ?e)
        ("home" . ?h)
        ("sandlot" . ?s)
        ("work" . ?w)
        (:endgroup . nil)

        ;; Blocked tags.
        (:startgroup . nil)
        ("waiting" . ?W)
        ("hold" . ?H)
        ("cancelled" . ?C)
        (:endgroup . nil)

        ;; Extra category tags
        (:startgroup . nil)
        ("prod" . ?p)
        ("grow" . ?g)
        (:endgroup . nil)

        ("goognet" . ?n)
        ("googmac" . ?a)
        ("emacs" . ?m)
        ))

;; The reason we have a waiting tag is for projects so we can identify the next
;; task.  The next task might be waiting, but if we set the todo state to
;; WAIT, then we lose information on what the next task is.
(setq org-todo-state-tags-triggers
      '(
        ;; Moving a task to CANCELLED adds a CANCELLED tag.
        ("CANX" ("canx" . t))
        ("WAIT" ("wait" . t))

        ;; Moving a task to HOLD removes a WAIT tag and adds a HOLD tag
        ("HOLD" ("wait") ("hold" . t))
        ;; done means any done state, the one's after the "|" in
        ;; `org-todo-keywords'
        (done ("wait") ("hold"))
        ("TODO" ("wait") ("canx") ("hold"))
        ("NEXT" ("wait") ("canx") ("hold"))
        ("NOW" ("wait") ("canx") ("hold"))
        ("DONE" ("wait") ("canx") ("hold"))))

(setq org-agenda-compact-blocks t)


;; The Agenda

;; Customize my agenda.  Don't use `add-to-list' because if we re-evaluate each
;; expression, it would append it to the list.  Instead we replace entries using
;; their shortcut keystroke as the key into the alist.

(defun my:org-agenda-add (key description agenda-list &optional settings files)
  "Add new agenda view to `org-agenda-custom-commands' intelligently.
KEY is the shortcut key in the agenda view.  DESCRIPTION is the
description that shows up in the agenda selection.  AGENDA-LIST
is a list of views to use for the agenda.

Replaces entries in `org-agenda-custom-commands' that already
exist by comparing the KEY.

SETTINGS A list of option settings, similar to that in a let
form, so like this: ((opt1 val1) (opt2 val2) ...).  The values
will be evaluated at the moment of execution, so quote them when
needed.

FILES a list of files file to write the produced agenda buffer to
with the command ‘org-store-agenda-views’.  If a file name ends
in \".html\", an HTML version of the buffer is written out.  If it
ends in \".ps\", a postscript version is produced.  Otherwise, only
the plain text is written to the file."
  (declare (indent 2))
  (my:replace-or-add-to-alist
   'org-agenda-custom-commands
   (list key description agenda-list settings files)))

(defun my:org-agenda-add-prefix (prefix-key description)
  "Add new agenda view to `org-agenda-custom-commands' intelligently.
PREFIX-KEY is the shortcut key in the agenda view.  DESCRIPTION is the
description that shows up in the agenda selection.
Replaces entries in `org-agenda-custom-commands' that already
exist by comparing the KEY."
  (declare (indent 2))
  (my:replace-or-add-to-alist
   'org-agenda-custom-commands
   (cons prefix-key description)))

(defvar my:org-agenda-standalone-tasks
  '(tags-todo "-REFILE-CANX-WAIT-HOLD/!"
              ((org-agenda-overriding-header "Unscheduled Standalone Tasks")
               (org-agenda-skip-function 'bh/skip-project-tasks)
               (org-agenda-tags-todo-honor-ignore-options t)))
  "Agenda definition for standalone tasks.
A standalone task is one that is not part of any project.")


;; Agenda Views

(my:org-agenda-add-prefix " " "  AWESOME")

(my:org-agenda-add " d" "TODAY"
  '((agenda "" ((org-agenda-span 'day))))
  '((org-agenda-tag-filter-preset '("-habit"))))


;; All Task commands.
(my:org-agenda-add-prefix "t" "Tasks")

(my:org-agenda-add "ta" "All Tasks"
  '((alltodo)))

(my:org-agenda-add "to" "All NOW Tasks"
  '((todo "NOW")))

(my:org-agenda-add "tn" "All NEXT Tasks"
  '((todo "NEXT"
          ((org-agenda-tags-todo-honor-ignore-options t)
           (org-agenda-todo-ignore-scheduled 'future)))))

(my:org-agenda-add "tt" "All TODO Tasks"
  '((todo "TODO")))

(my:org-agenda-add "tr" "Tasks to Refile"
  '((tags "refile"
          ((org-agenda-overriding-header "Unfiled tasks")
           (org-tags-match-list-sublevels nil)))))

(my:org-agenda-add "tR" "Non-work Tasks to Refile"
  '((tags "refile-work"
          ((org-agenda-overriding-header "Unfiled tasks")
           (org-tags-match-list-sublevels nil)))))

(my:org-agenda-add "ts" "Standalone Tasks"
  (list my:org-agenda-standalone-tasks)
  '((org-agenda-todo-ignore-scheduled 'future)))

(my:org-agenda-add "ta" "Archivable Standalone Tasks"
  '((todo "CANCELLED|DONE"
          ((org-agenda-overriding-header "Archivable Standalone Tasks")
           (org-agenda-skip-function 'bh/skip-project-tasks)
           (org-agenda-todo-ignore-scheduled 'future)
           (org-agenda-tags-todo-honor-ignore-options t)))))

(my:org-agenda-add "te" "Errands"
  '((tags-todo "errand")))

(my:org-agenda-add "tw" "Wating Tasks"
  '((tags-todo "/WAIT"
               ((org-agenda-overriding-header
                 (concat "Waiting and Postponed Tasks"
                         (if bh/hide-scheduled-and-waiting-next-tasks
                             ""
                           " (including WAIT and SCHEDULED tasks)")))
                (org-agenda-skip-function 'bh/skip-non-tasks)
                (org-tags-match-list-sublevels nil)
                (org-agenda-todo-ignore-scheduled
                 bh/hide-scheduled-and-waiting-next-tasks)
                (org-agenda-todo-ignore-deadlines
                 bh/hide-scheduled-and-waiting-next-tasks)))))

(my:org-agenda-add "tc" "Tasks with no context"
  '((tags-todo "-home-comp-work-sandlot-errand-habit")))


;; Home commands.
(my:org-agenda-add-prefix "h" "Home")

(my:org-agenda-add "hr" "Home Routine"
  '((tags-todo "home"
               ((org-agenda-overriding-header "Home Routine")
                ;; We need this to ignore scheduled items.
                (org-agenda-tags-todo-honor-ignore-options t)
                (org-agenda-todo-ignore-scheduled 'future)
                (org-agenda-files '("~/gdrive/org/habits.org"))))))

(my:org-agenda-add "ha" "Tasks at Home"
  '((tags-todo "home")))

(my:org-agenda-add "ho" "@Home NOW"
  '((tags "home/NOW")))

(my:org-agenda-add "hn" "@Home NEXT"
  '((tags-todo "home/NEXT"
               ((org-agenda-tags-todo-honor-ignore-options t)
                (org-agenda-todo-ignore-scheduled 'future)))))

(my:org-agenda-add "ht" "@Home TODO"
  '((tags "home/TODO")))


(my:org-agenda-add "he" "Evening tasks"
  '((tags-todo "home+evening"
               ((org-agenda-overriding-header "Home Evening")
                ;; We need this to ignore scheduled items.
                (org-agenda-tags-todo-honor-ignore-options t)
                (org-agenda-todo-ignore-scheduled 'future)
                (org-agenda-files '("~/gdrive/org/habits.org"))))))


;; Work commands.
(my:org-agenda-add-prefix "w" "work")

(my:org-agenda-add "wr" "Work Routine"
  '((tags-todo "work"
               ((org-agenda-overriding-header "Work Routine")
                ;; We need this to ignore scheduled items.
                (org-agenda-tags-todo-honor-ignore-options t)
                (org-agenda-todo-ignore-scheduled 'future)
                (org-agenda-files '("~/gdrive/org/habits.org"))))))

(my:org-agenda-add "wa" "All Tasks at Work"
  '((tags-todo "work")))

(my:org-agenda-add "wo" "@Work NOW"
  '((tags "work/NOW")))

(my:org-agenda-add "wn" "@Work NEXT"
  '((tags-todo "work/NEXT"
               ((org-agenda-tags-todo-honor-ignore-options t)
                (org-agenda-todo-ignore-scheduled 'future)))))

(my:org-agenda-add "wt" "@Work TODO"
  '((tags "work/TODO")))

(my:org-agenda-add "ws" "Work - Standalone"
  (list my:org-agenda-standalone-tasks)
  '((org-agenda-todo-ignore-scheduled 'future)
    (org-agenda-files '("~/gdrive/gorg/goog.org"))))


;; Sandlot commands.
(my:org-agenda-add-prefix "s" "sandlot")

(my:org-agenda-add "sa" "All Tasks at Sandlot"
 '((tags-todo "sandlot")))

(my:org-agenda-add "so" "@Sandlot NOW"
  '((tags "sandlot/NOW")))

(my:org-agenda-add "sn" "@Sandlot NEXT"
  '((tags-todo "sandlot/NEXT"
               ((org-agenda-tags-todo-honor-ignore-options t)
                (org-agenda-todo-ignore-scheduled 'future)))))

(my:org-agenda-add "st" "@Sandlot TODO"
  '((tags "sandlot/TODO")))

(my:org-agenda-add "ss" "Sandlot - Standalone"
  (list my:org-agenda-standalone-tasks)
  '((org-agenda-todo-ignore-scheduled 'future)
    (org-agenda-files '("~/gdrive/gorg/sandlot.org"))))


;; Comp commands.
(my:org-agenda-add-prefix "c" "comp")

(my:org-agenda-add "ca" "All Tasks at Comp"
  '((tags-todo "comp")))

(my:org-agenda-add "co" "@Comp NOW"
  '((tags "comp/NOW")))

(my:org-agenda-add "cn" "@Comp NEXT"
  '((tags-todo "comp/NEXT"
               ((org-agenda-tags-todo-honor-ignore-options t)
                (org-agenda-todo-ignore-scheduled 'future)))))

(my:org-agenda-add "ct" "@Comp TODO"
  '((tags "comp/TODO")))

(my:org-agenda-add "cs" "Comp - Standalone"
  (list my:org-agenda-standalone-tasks)
  '((org-agenda-todo-ignore-scheduled 'future)
    (org-agenda-files '("~/gdrive/gorg/comp.org"))))


;; Projects
(my:org-agenda-add-prefix "p" "Projects")

(my:org-agenda-add "ps" "Stuck Projects"
  '((tags-todo "-CANX/!"
               ((org-agenda-overriding-header "Stuck Projects")
                (org-agenda-skip-function 'bh/skip-non-stuck-projects)))))

(my:org-agenda-add "pa" "Archivable Projects"
  '((tags "LEVEL=2/DONE|CANX"
          ((org-agenda-overriding-header "Archivable Projects")
           (org-agenda-skip-function 'bh/skip-incomplete-projects)))))

(my:org-agenda-add "pp" "All Projects"
  '((tags-todo "-slice/!"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-skip-function 'bh/skip-non-projects)
                (org-agenda-sorting-strategy
                 '(category-keep))))))


;; Review
(my:org-agenda-add-prefix "r" "Review")

(my:org-agenda-add "rd" "Day in Review"
  '((agenda ""))
  '((org-agenda-span 'day)
    (org-agenda-overriding-header "Day in Review")
    (org-agenda-show-all-dates t)
    (org-agenda-log-mode-items '(clock closed))
    (org-agenda-start-with-log-mode t)
    (org-agenda-start-with-clockreport-mode t)
    (org-agenda-archives-mode t)
    ;; I don't care if an entry was archived
    (org-agenda-hide-tags-regexp
     (concat org-agenda-hide-tags-regexp
             "\\|ARCHIVE"))))

(my:org-agenda-add "rw" "Week in Review"
  '((agenda ""))
  '((org-agenda-span 'week)
    (org-agenda-overriding-header "Week in Review")
    (org-agenda-show-all-dates t)
    (org-agenda-start-with-log-mode t)
    (org-agenda-start-with-clockreport-mode t)
    (org-agenda-archives-mode t)
    ;; I don't care if an entry was archived
    (org-agenda-hide-tags-regexp
     (concat org-agenda-hide-tags-regexp
             "\\|ARCHIVE"))))


(defun my:org-pick-smart-context (clipboard-content)
  "If CLIPBOARD-CONTENT is a URL use it, else return empty string."
  (if (string-match-p "https?://.*" clipboard-content)
      clipboard-content
    ""))

(defvar my:org-work-refile "~/gdrive/org/work-refile.org"
  "Where to save todos for work related tasks.")

(setq org-capture-templates
      `(("t" "todo" entry (file ,org-default-notes-file)
         "* TODO %?\n%U\n%(my:org-pick-smart-context \"%x\")\n")

        ("T" "todo-work" entry (file ,my:org-work-refile)
         "* TODO %?\n%U\n%(my:org-pick-smart-context \"%x\")\n")

        ("r" "respond" entry (file ,org-default-notes-file)
         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
         :clock-in t :clock-resume t :immediate-finish t)

        ("n" "note" entry (file ,org-default-notes-file)
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)

        ("j" "Journal" entry (file+datetree "~/gdrive/org/journal.org")
         "* %?\n%U\nReview day, winning friends influencing people, gratitudes"
         :clock-in t :clock-resume t)

        ("s" "Work Snippet" entry (file+datetree "~/gdrive/org/journal.org")
         ,(concat (format "**** Work Journal %59s\n" ":work:")
                  "%U\n\n%?")
         :clock-in t :clock-resume t)

        ("S" "Weekly Work Snippet" entry
         (file+datetree "~/gdrive/org/journal.org")
         ,(concat (format "**** Weekly Work Snippets %51s\n" ":work:snippets:")
                  "%U\n\n%?%(my:org-get-snippets-for-current-week)")

         :clock-in t :clock-resume t)
        ("w" "org-protocol" entry (file ,org-default-notes-file)
         "* TODO Review %c\n%U\n" :immediate-finish t)

        ("m" "Meeting" entry (file ,org-default-notes-file)
         "* MEETING with %? :meeting:\n%U" :clock-in t :clock-resume t)

        ("p" "Phone call" entry (file ,org-default-notes-file)
         "* phone %? :phone:\n%U" :clock-in t :clock-resume t)

        ("h" "Habit" entry (file ,org-default-notes-file)
         "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

(defun my:remove-empty-drawer-on-clock-out ()
  "Remove empty LOGBOOK drawers on clock out."
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(add-hook 'org-clock-out-hook 'my:remove-empty-drawer-on-clock-out 'append)

;; Don't do any normla logging if changing todo state with Shift-Right or
;; shift-left.  Useful for fixing incorrect todo states.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; (setq org-refile-targets
;;       '(("gtd.org" :maxlevel . 1)
;;         ("someday.org" :level . 2)))

(setq org-refile-targets
      '((nil :maxlevel . 9) ; nil means current buffer
        (org-agenda-files :maxlevel . 9)))

;; Allow paths for refiling like Projects/Setup Ubuntu
(setq org-refile-use-outline-path t)

;; Show all subtrees when refiling
(setq org-outline-path-complete-in-steps nil)

;; save all the agenda files after each capture and when agenda mode is
;; open
(add-hook 'org-capture-after-finalize-hook 'org-save-all-org-buffers)
(add-hook 'org-agenda-mode-hook 'org-save-all-org-buffers)
;; This seems buggy
;; (advice-add #'org-refile :after #'org-save-all-org-buffers)
;; (advice-remove #'org-refile #'org-save-all-org-buffers)

;; Add C-c C-c keybinding to exit org-edit-src to mirror Magit's commit
;; buffer.
(with-eval-after-load 'org-src
  (define-key org-src-mode-map "\C-c\C-c" 'org-edit-src-exit))

(defun swift-plaques-compile (&optional force)
  "Compile the swift-plaques project.
If FORCE is non-nil, force recompilation even if files haven't changed."
  (interactive)
  (org-publish "swift-plaques" t))

(defun my:make-org-link-cite-key-visible (&rest _)
  "Make the org-ref cite link visible in descriptive links."
  (when (string-prefix-p "cite:" (match-string 1))
    (remove-text-properties (+ (length "cite:") (match-beginning 1))
                            (match-end 1)
                            '(invisible))))

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "yk" 'org-priority-up
  "yj" 'org-priority-down
  "oa" 'org-attach-open
  )

(with-eval-after-load 'ox-latex
  (let* ((text-spacing
          (s-join
           "\n"
           '("\\ifxetex"
             "  \\newcommand{\\textls}[2][5]{%"
             "  \\begingroup\\addfontfeatures{LetterSpace=#1}#2\\endgroup"
             "}"
             "\\renewcommand{\\allcapsspacing}[1]{\\textls[15]{#1}}"
             "\\renewcommand{\\smallcapsspacing}[1]{\\textls[10]{#1}}"
             "\\renewcommand{\\allcaps}[1]{\\textls[15]{\\MakeTextUppercase{#1}}}"
             "\\renewcommand{\\smallcaps}[1]{\\smallcapsspacing{\\scshape\\MakeTextLowercase{#1}}}"
             "\\renewcommand{\\textsc}[1]{\\smallcapsspacing{\\textsmallcaps{#1}}}"
             "\\fi")))
         (multitoc "\\usepackage[toc]{multitoc}")
         (tufte-handout-class
          `("tufte-handout"
            ,(s-join "\n"
                     `("\\documentclass{tufte-handout}[notoc]"
                       "[DEFAULT-PACKAGES]"
                       "[EXTRA]"
                       ,text-spacing
                       "% http://tex.stackexchange.com/questions/200722/"
                       ,multitoc
                       "[PACKAGES]"))
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")))
         (tufte-book-class
          `("tufte-book"
            ,(s-join "\n"
                     `("\\documentclass{tufte-book}"
                       "[DEFAULT-PACKAGES]"
                       "[EXTRA]"
                       ,text-spacing
                       "% http://tex.stackexchange.com/questions/200722/"
                       ,multitoc
                       "[PACKAGES]"))
            ("\\chapter{%s}" . "\\chapter*{%s}")
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}"))))
    (my:replace-or-add-to-alist 'org-latex-classes tufte-book-class)
    (my:replace-or-add-to-alist 'org-latex-classes tufte-handout-class)))

(defun bh/is-task-p ()
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

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask."
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

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project."
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/clock-in-to-next (current-todo-state)
  "Set tasks TODO state to NOW when clocking in.
If CURRENT-TODO-STATE is TODO or NEXT then change to NOW.  Skips
capture tasks, projects, and subprojects.  Sets subprojects from
NOW back to TODO to indicate they are stuck."
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member current-todo-state (list "TODO" "NEXT"))
           (bh/is-task-p))
      "NOW")
     ((and (member current-todo-state (list "NOW"))
           (bh/is-project-p))
      "TODO"))))

(defun my/org-agenda-match-tags (tags)
  "Match entries that have all TAGS."
  (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
         (current-headline (or (and (org-at-heading-p)
                                    (point))
                               (save-excursion (org-back-to-heading))))
         (current-tags (org-get-tags-at current-headline)))

    (if (-any-p (lambda (tag) (not (member tag current-tags))) tags)
        next-headline
      nil)))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels to list all sub tasks.
Applies when restricted to a subtree we list all subtasks.  This
is normally used by skipping functions where this variable is
already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  "Toggle the value of `bh/hide-scheduled-and-waiting-next-tasks'."
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks
        (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAIT and SCHEDULED NEXT Tasks"
           (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are stuck projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion
                           (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end)
                          (re-search-forward "^\\*+ \\(NEXT\\|NOW\\) "
                                             subtree-end t))
                (unless (member "WAIT" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun my:is-sandlot-buffer ()
  "Return t if buffer is sandlot.org."
  (equal (file-name-nondirectory (buffer-file-name))
         "sandlot.org"))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects.
A stuck project is one that lacks any of the following
- There is a NEXT item.
- There is a WAIT item scheduled in the future."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (and (bh/is-project-p)
               (not (my:is-sandlot-buffer)))
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next nil))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end))

                (if (re-search-forward "^\\*+ \\(NEXT\\|NOW\\|WAIT\\) " subtree-end t)
                    (cond ((or (equal (match-string 1) "NOW")
                               (equal (match-string 1) "NEXT"))
                           (setq has-next t))

                          ((equal (match-string 1) "WAIT")
                           (-when-let (scheduled-time (org-get-scheduled-time (point)))
                             (if (time-less-p (org-time-today) scheduled-time)
                                 (setq has-next t)
                               )))))
                (goto-char subtree-end)))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects."
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-incomplete-projects ()
  "Skip trees of projects that are not complete."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (bh/is-project-p)
        ;; There's a todo entry that's not done or cancelled
        (if (org-map-entries #'point "/!-DONE&-CANCELLED" 'tree)
            ;; Skip this project because there's still work to do.
            subtree-end
          ;; Keep this project because it'd completely done.
          nil)
      ;; Not a project so skip it
      subtree-end)))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "waiting" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks,
habits, NEXT tasks, and loose tasks.  When not restricted, skip
project and sub-project tasks, habits, and project related
tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects."
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(defun my:replace-or-add-to-alist (alist-var elem)
  "Replace in ALIST-VAR the first entry whose `car' `equal's (car ELEM).
Replace the cdr of the found item in ALIST-VAR with ELEM.
ALIST-VAR must be a symbol.  If no \(car entry\) in ALIST-VAR
equals the `car' of ELEM, then prepend ELEM to ALIST-VAR.

\(my:replace-or-add-to-alist 'an-alist '(\"key\" \"data\")\)"
  (let ((alist (symbol-value alist-var)))
    (if (assoc (car elem) alist)
        (setcdr (assoc (car elem) alist)
                (cdr elem))
      (set alist-var (cons elem alist)))))

(defun my:create-refile-entry-from-url (url)
  "Create an org entry for URL."
  (with-current-buffer (find-file "~/gdrive/org/refile.org")
    (save-excursion
      (goto-char (point-max))
      (insert "\n* TODO " url "\n[" (format-time-string "%Y-%m-%d %a %H:%m") "]"))))

(defun my:convert-review-entry-to-org (file-path)
  "Convert a FILE-PATH with a URL to an org entry."
  (find-file file-path)
  (goto-char (point-min))
  (my:create-refile-entry-from-url
   (buffer-substring-no-properties (point-min) (end-of-line)))
  ;; (delete-file file-path)
  )

(defun my:convert-review-entries-to-org ()
  "Convert entries saved in ~/gdrive/Review to org."
  (mapc #'my:convert-review-entry-to-org
        (directory-files "~/gdrive/Review" 'full-path nil 'no-sort)))

(defun my:org-get-clocked-in-headline ()
  "Get the headline of the currently clocked in headline.
If no headline is clocked in, then return an empty string."
  (interactive)
  (if (not (org-clocking-p))
      ""
    (with-current-buffer (marker-buffer org-clock-marker)
      (save-excursion
        (save-restriction
          (when (or (< org-clock-marker (point-min)) (> org-clock-marker (point-max)))
            (widen))

          (goto-char org-clock-marker)
          (or (org-no-properties (org-get-heading 'no-tags 'no-todo))
              ""))))))

(defvar my:org-clocked-in-file-path "/tmp/org-currently-clocked-in-task"
  "Where to save the currently clocked in task for all to see.")

(defun my:org-save-clocked-in-entry-to-file ()
  "Save currently clocked-in task to a file."
  (let ((last-message (current-message))
        ;; Suppress echo area to see clock out information.  Doesn't seem to
        ;; work, so we'll just re-display last-message.
        (inhibit-message nil))
    (with-temp-buffer
      (insert (my:org-get-clocked-in-headline))
      (write-region (point-min) (point-max) my:org-clocked-in-file-path))
    (message last-message)))

(add-hook 'org-clock-in-hook #'my:org-save-clocked-in-entry-to-file)
(add-hook 'org-clock-out-hook #'my:org-save-clocked-in-entry-to-file)
(add-hook 'org-clock-cancel-hook #'my:org-save-clocked-in-entry-to-file)


(defun my:org-paragraph-overrides ()
  "Reset `paragraph-start' and `paragraph-end' to default values"
  (setq-local paragraph-start (default-value 'paragraph-start))
  (setq-local paragraph-separate (default-value 'paragraph-separate)))

(add-hook 'org-mode-hook #'my:org-paragraph-overrides)

(add-hook 'org-mode-hook #'auto-fill-mode)

(defun my:org-get-work-snippet-on-date (filename date)
  "Returns point of work journal entry from FILENAME on DATE.
DATE must be a form like 2016-10-22"
  (let ((journal-headings
         (helm-org--get-candidates-in-file filename))
        (work-journal-regexp (format "%s [A-Z][a-z]\\{2,5\\}day/Work Journal$"
                                     date)))
    (cl-loop for (helm-org-heading . marker-position) in journal-headings
             when (string-match work-journal-regexp helm-org-heading)
             return marker-position)))

(defun my:org-get-work-snippets-between-dates (filename start-date num-days)
  "Returns list of points of work journal entries between dates.
Searches FILENAME for entries between START-DATE and START-DATE +
\(NUM-DAYS - 1\) inclusive.  At most, NUM-DAYS points will be returned."
  (remove nil
          (cl-loop for date in (my:generate-date-range start-date num-days)
                   collect (my:org-get-work-snippet-on-date filename date))))

(defun my:org-delete-standalone-times ()
  "Deletes timestamps like [2016-10-17 Mon 11:39] from current buffer."
  (save-excursion
    (let (timestamp-start timestamp-end)
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^ *\\[20[012][0-9]-[0-9][0-9]-[0-9][0-9]"
                      " [A-Z][a-z][a-z]"
                      " [0-9][0-9]:[0-9][0-9]"
                      "\\]") nil t)
        (setq timestamp-start (line-beginning-position))
        (setq timestamp-end (1+ (line-end-position)))
        (delete-region timestamp-start timestamp-end)
        ))))

(defun my:org-delete-logbook ()
  "Deletes all logbook entries in current buffer."
  (save-excursion
    (let (logbook-start logbook-end)
      (goto-char (point-min))
      (while (re-search-forward "^ *:LOGBOOK:" nil t)
        (setq logbook-start (line-beginning-position))
        (re-search-forward "^ *:END:")
        ;; Add 1 for newline.
        (setq logbook-end (1+ (line-end-position)))
        (delete-region logbook-start logbook-end)
        ))))

(defun my:org-cleanup-work-snippets (string)
  "Remove org-headings and meta information from STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    ;; Delete work journal entries
    (while (re-search-forward "^\\*+ Work Journal" nil t)
      (delete-region (line-beginning-position) (line-end-position))
      (when (looking-at "\n")
        (delete-char 1)))
    (my:org-delete-logbook)
    (my:org-delete-standalone-times)
    (goto-char (point-min))
    ;; Remove any leading newlines at beginning of buffer.
    (while (looking-at "\n")
      (delete-char 1))
    (buffer-string)))

(defun my:org-collect-work-snippets-between-dates (filename start-date num-days)
  "Get a string of work snippets in FILENAME between START-DATE and + NUM-DAYS."
  (let ((snippet-locations (my:org-get-work-snippets-between-dates
                            filename start-date num-days))
        ;; Suppress messages.
        (message-log-max nil))

    (with-current-buffer (pcase filename
                           ((pred bufferp) filename)
                           ((pred stringp) (find-file-noselect filename)))
      (save-excursion
        (save-restriction
          (widen)
          ;; Add a dummy string to the kill ring that we can append entries too.
          (when (> (length snippet-locations) 0)
            (kill-new "")
            (cl-loop for snippet-marker in snippet-locations
                     with subtree = nil
                     do
                     (progn
                       (goto-char (marker-position snippet-marker))
                       (org-copy-subtree)
                       ;; Get the org-copy-subtree off of the kill ring.
                       (setq subtree (current-kill 0))
                       ;; Remove all the properties.
                       (set-text-properties 0 (length subtree) nil subtree)
                       ;; Remove the org subtree from the kill ring.
                       (setq kill-ring (cdr-safe kill-ring))
                       (kill-append subtree nil))
                     ))))))
  (my:org-cleanup-work-snippets (pop kill-ring)))

(defun my:org-get-snippets-for-current-week ()
  "Return a string of all snippets for the current week."
  (interactive)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (monday (my:get-previous-monday today)))
    (my:org-collect-work-snippets-between-dates
     my:org-journal-file monday 5)))

(defun my:increment-date (date-string)
  "Add one day to DATE-STRING.
DATE-STRING must be parseable by `org-parse-time-string'."
  (format-time-string
   "%Y-%m-%d"
   (time-add (apply #'encode-time (org-parse-time-string date-string))
             (days-to-time 1))))

(defun my:decrement-date (date-string)
  "Subtract one day to DATE-STRING.
DATE-STRING must be parseable by `org-parse-time-string'."
  (format-time-string
   "%Y-%m-%d"
   (time-add (apply #'encode-time (org-parse-time-string date-string))
             (days-to-time -1))))

(defun my:get-previous-monday (date-string)
  "Get the Monday before DATE-STRING as a date string."
  (cl-loop repeat 8
           with date = (apply #'encode-time
                              (org-parse-time-string date-string))

           do (if (string-equal (format-time-string "%u" date) "1")
                  (return (format-time-string "%Y-%m-%d" date))
                (setq date (time-add date (days-to-time -1))))))

(defun my:generate-date-range (start-date num-days)
  "Generates a list of dates between START-DATE and NUM-DAYS in the future.
The list returned is NUM-DAYS long.  The dates are generated in
the form 2016-10-22."
  (cl-loop
   repeat num-days
   with date = start-date
   collect date
   do (setq date (my:increment-date date))))

(provide 'my-org)
;;; my-org.el ends here
