(require 'org)
(require 'org-agenda)

;; Custom bindings
(spacemacs/set-leader-keys
  "aoj" 'org-clock-goto)

(setq org-directory "~/Dropbox/org")

(setq org-default-notes-file "~/Dropbox/org/refile.org")

(setq org-agenda-files '("~/Dropbox/org/"))

(setq org-log-done t)

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)

;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)

;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))

;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)

;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

;; Fontify code in code blocks
(setq org-src-fontify-natively t)


(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)"
                  "|" "DONE(d)")
        ;; The @/! means log a note when entering this state and log just
        ;; a timestamp when leaving this state
        (sequence "WAITING(w@/!)" "HOLD(h@/!)"
                  "|" "CANCELLED(c)")))

(setq org-tag-alist
      '(;; Elements of a group are mutually exclusive
        (:startgroup . nil)
        ("work" . ?w) ("home" . ?h) ("comp" . ?c) ("errand" . ?e)
        (:endgroup . nil)

        (:startgroup . nil)
        ("start" . ?s) ("mid" . ?m) ("end" . ?n)
        (:endgroup . nil)

        (:startgroup . nil)
        ("daily" . ?d) ("weekly" . ?k) 
        (:endgroup . nil)

        ))

(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("WAITING" ("WAITING" . t))
        ("HOLD" ("WAITING") ("HOLD" . t))
        ;; done means any done state, the one's after the "|" in
        ;; `org-todo-keywords'
        (done ("WAITING") ("HOLD"))
        ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
        ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
        ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

(setq org-agenda-compact-blocks t)

;; Customize my agenda.  Don't use `add-to-list' because if we re-evaluate each
;; expression, it would append it to the list.  Instead we replace entries using
;; their shortcut keystroke as the key into the alist.

(defun my:org-agenda-add (cmd)
  "Add CMD to `org-agenda-custom-commands' intelligently.
Replace CMDs that already exist by comparing the shortuct keystroke."
  (my:replace-or-add-to-alist
   'org-agenda-custom-commands
   cmd))

(my:org-agenda-add
 '("h" "Office and Home Lists"
   ((agenda)
    (tags-todo "work")
    (tags-todo "home")
    (tags-todo "comp")
    (tags-todo "read"))))

(my:replace-or-add-to-alist
 'org-agenda-custom-commands
 '("d" . "Daily"))

(my:replace-or-add-to-alist
 'org-agenda-custom-commands
 '("dd" "All daily"
   ((agenda "" ((org-agenda-ndays 1)))
    (tags-todo "start+mid+end"))))

(my:replace-or-add-to-alist
 'org-agenda-custom-commands
 '("ds" "Daily Start"
   ((agenda ""
            ((org-agenda-ndays 1)
             (org-agenda-skip-function '(my/org-agenda-match-tags (list "start"))))))))

(my:replace-or-add-to-alist
 'org-agenda-custom-commands
 '("dm" "Daily Mid"
   ((agenda ""
            ((org-agenda-ndays 1)
             (org-agenda-skip-function '(my/org-agenda-match-tags (list "mid"))))))))

(my:replace-or-add-to-alist
 'org-agenda-custom-commands
 '("dn" "Daily End"
   ((agenda ""
            ((org-agenda-ndays 1)
             (org-agenda-skip-function '(my/org-agenda-match-tags (list "end"))))))))

(my:replace-or-add-to-alist
 'org-agenda-custom-commands
 '("tr" "Refile"
   ((tags "LEVEL=1"
          ((org-agenda-overriding-header "Unfiled tasks")
           (org-agenda-files (list ,org-default-notes-file)))))))

(my:replace-or-add-to-alist
 'org-agenda-custom-commands
 '("tt" "Today"
   (
    ;; Events
    (agenda ""
            ((org-agenda-entry-types '(:timestamp :sexp))
             (org-agenda-overriding-header
              (concat "CALENDAR Today "
                      (format-time-string "%a %d" (current-time))))
             (org-agenda-span 'day)))

    )))

(my:replace-or-add-to-alist
 'org-agenda-custom-commands
 '("ts" "Standalone Tasks"
   (
    ;; Events
    (agenda ""
            ((org-agenda-ndays 1)))

    ;; Unscheduled New Tasks
    (tags-todo "LEVEL=2"
               ((org-agenda-overriding-header "Unscheduled tasks")
                (org-agenda-files (list ,org-default-notes-file)))))))

(my:replace-or-add-to-alist
 'org-agenda-custom-commands
 '("rN" "Next"
   ((tags-todo "TODO<>{SDAY}"))
   ((org-agenda-overriding-header "List of all TODO entries with no due date (no SDAY)")
    (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
    (org-agenda-sorting-strategy '(priority-down)))))

(my:replace-or-add-to-alist
 'org-agenda-custom-commands
 '("E" "Errands" tags-todo "errand"))

(setq org-capture-templates
      `(("t" "todo" entry (file ,org-default-notes-file)
         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)

        ("r" "respond" entry (file ,org-default-notes-file)
         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
         :clock-in t :clock-resume t :immediate-finish t)

        ("n" "note" entry (file ,org-default-notes-file)
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)

        ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
         "* %?\n%U\n" :clock-in t :clock-resume t)

        ("w" "org-protocol" entry (file ,org-default-notes-file)
         "* TODO Review %c\n%U\n" :immediate-finish t)

        ("m" "Meeting" entry (file ,org-default-notes-file)
         "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)

        ("p" "Phone call" entry (file ,org-default-notes-file)
         "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)

        ("h" "Habit" entry (file ,org-default-notes-file)
         "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

(defun my/remove-empty-drawer-on-clock-out ()
  "Remove empty LOGBOOK drawers on clock out."
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(add-hook 'org-clock-out-hook 'my/remove-empty-drawer-on-clock-out 'append)

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

;; Add C-c C-c keybinding to exit org-edit-src to mirror Magit's commit
;; buffer.
(with-eval-after-load 'org-src
  (define-key org-src-mode-map "\C-c\C-c" 'org-edit-src-exit))

(require 'org-drill)
(defun swift-plaques-compile (&optional force)
  "Compile the swift-plaques project.
If FORCE is non-nil, force recompilation even if files haven't changed."
  (interactive)
  (org-publish "swift-plaques" t))
(with-eval-after-load 'ox-publish
  (dolist (project
           `(("swift-plaques"
              :author "Joe Schafer"
              :base-directory "~/prog/swift-plaques-business-plan"
              :publishing-directory "~/prog/swift-plaques-business-plan"
              :publishing-function org-latex-publish-to-pdf
              :base-extension "org"
              )))
    (my:replace-or-add-to-alist 'org-publish-project-alist project))
  (joe/set-leader-keys
   "cs" 'swift-plaques-compile))

(defun my:work-around-org-window-drill-bug ()
  "Comment out a troublesome line in `org-toggle-latex-fragment'.
See https://bitbucket.org/eeeickythump/org-drill/issues/30 for
details."
  (save-excursion
    (let ((org-library-location (concat
                                 (locate-library "org" 'nosuffix)
                                 ".el")))
      (with-current-buffer (find-file-noselect org-library-location)
        (goto-char (point-min))
        (search-forward "(set-window-start nil window-start)")
        (back-to-indentation)
        (if (looking-at ";; ")
            (message "Already modified `org-toggle-latex-fragment' for `org-drill'")
          (insert ";; ")
          (save-buffer)
          (byte-compile-file org-library-location)
          (elisp--eval-defun)
          (message "Modified `org-toggle-latex-fragment' for `org-drill'"))))))

(my:work-around-org-window-drill-bug)
(defun my:make-org-link-cite-key-visible (&rest _)
  "Make the org-ref cite link visible in descriptive links."
  (when (string-prefix-p "cite:" (match-string 1))
    (remove-text-properties (+ (length "cite:") (match-beginning 1))
                            (match-end 1)
                            '(invisible))))
(defun my:org-set-tag-as-drill ()
  (interactive)
  (org-toggle-tag "drill"))
(defun my:org-drill-create-template ()
  (interactive)
  (insert "*** Item                                      :drill:\n\n")
  (insert "Question\n\n")
  (insert "**** Answer\n\n")
  (insert "Answer\n")
  (search-backward "Item")
  (forward-word)
  (forward-char))
(defun my:org-drill-create-template-cloze ()
  (interactive)
  (insert "*** Item                                      :drill:\n")
  (insert ":PROPERTIES:\n:DRILL_CARD_TYPE: hide1cloze\n:END:\n\n")
  (insert "[Question] and [Answer]\n\n")
  (search-backward "Item")
  (forward-word)
  (forward-char))
(joe/set-leader-keys
 "dd" 'my:org-set-tag-as-drill
 "dt" 'my:org-drill-create-template
 "dc" 'my:org-drill-create-template-cloze)
(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "yk" 'org-priority-up
  "yj" 'org-priority-down)
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
  "Any task with a todo keyword and no subtask"
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

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
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

