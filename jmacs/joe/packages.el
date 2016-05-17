;;; packages.el --- joe Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org


;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(defvar joe-packages
  '(
    ;; auto-yasnippet
    ;; (doc-popup
    ;; :location local)
    ;; ebib
    camcorder
    ;; emacs-lisp
    evil
    evil-escape
    framemove
    ;; gradle-mode
    ;; helm-bibtex
    ;; help-fns+
    ;; hydra
    ;; jinja2-mode
    ;; key-chord
    magit
    mu4e
    overseer
    ;; openwith
    org
    ;; (org-ref :location local)
    ;; ;; (otb :location local)
    ;; persistent-scratch
    ;; pos-tip
    ;; ;; request
    ;; s
    ;; sx
    ;; typescript
    )
  "List of all packages to install and/or initialize.
Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar joe-excluded-packages '()
  "List of packages to exclude.")

(defun joe/post-init-auto-yasnippet ()
  "Init auto-yasnippet."
  (use-package auto-yasnippet
    :config
    (progn
      (setq aya-persist-snippets-dir "~/.dotfiles/snippets")
      (add-to-list 'yas-snippet-dirs "~/.dotfiles/snippets")
      (setq yas-snippet-dirs (delete "~/.emacs.d/snippets" yas-snippet-dirs))
      (setq yas-snippet-dirs (delete
                              (expand-file-name "~/.emacs.d/private/snippets/")
                              yas-snippet-dirs))
      (yas-reload-all))))

(defun joe/init-camcorder ()
  (use-package camcorder
    :config
    (progn)))

(defun joe/init-doc-popup ()
  "Init doc-popup."
  (use-package doc-popup
    :config
    (progn
      (defvar evil-normal-state-map)
      (define-key evil-normal-state-map "gh" 'doc-popup-show-at-point))))

(defun joe/init-ebib ()
  "Init ebib."
  (use-package ebib))

(defun joe/init-framemove ()
  "Init framemove."

  (framemove-default-keybindings)
  (setq framemove-hook-into-windmove t))

(defun joe/init-helm-bibtex ()
  "Init helm-bibtex."
  (use-package helm-bibtex
    :defer t))

(defun joe/init-key-chord ()
  "Init key-chord."
  (use-package key-chord))

(defun joe/init-jinja2-mode ()
  "Init jinja2-mode."
  (use-package jinja2
    :defer t
    :init
    (progn
      (defun my-jinja2-block (id action context)
        (insert " ")
        (save-excursion
          (insert " ")))

      (require 'smartparens)
      (defvar sp-navigate-consider-stringlike-sexp)
      (add-to-list 'sp-navigate-consider-stringlike-sexp
                   'jinja2-mode)

      ;; Remove curly brace binding because it prevents
      ;; a binding for Jinja constructs.
      (sp-local-pair 'jinja2-mode "{" "}" :actions nil)
      (sp-local-pair 'jinja2-mode "{%" "%}"
                     :post-handlers '(:add my-jinja2-block)
                     :trigger "jjb")
      (sp-local-pair 'jinja2-mode "{{" "}}"
                     :post-handlers '(:add my-jinja2-block)
                     :trigger "jji")))
  :config
  (progn
    (add-hook 'jinja2-mode-hook 'smartparens-mode)))

(defun joe/post-init-evil ()
  "Init evil."
  (use-package evil
    :config
    (progn
      (eval-when-compile
        (require 'evil-macros))

      (evil-define-motion my:evil-next-visual-line-5 (count)
        "Move the cursor 5 lines up."
        :type line
        (let (line-move-visual)
          (evil-next-visual-line (* 5 (or count 1)))))

      (evil-define-motion my:evil-previous-visual-line-5 (count)
        "Move the cursor 5 lines up."
        :type line
        (let (line-move-visual)
          (evil-previous-visual-line (* 5 (or count 1)))))

      (define-key evil-normal-state-map "\M-k" 'spacemacs/evil-smart-doc-lookup)
      (define-key evil-normal-state-map "K" 'my:evil-previous-visual-line-5)
      (cl-loop for (key . func) in
               `(("J" . my:evil-next-visual-line-5)
                 ("K" . my:evil-previous-visual-line-5)
                 ("gj" . evil-join)
                 ("H" . my:back-to-indentation-or-beginning)
                 ("L" . evil-end-of-line)
                 ("\C-j" . scroll-up-command)
                 ("\C-k" . scroll-down-command))
               do
               (define-key evil-normal-state-map key func)
               (define-key evil-visual-state-map key func)
               (define-key evil-motion-state-map key func))
      ;; Make movement keys work on visual lines instead of acutal lines.
      ;; This imitates Emacs behavior rather than Vim behavior.
      (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>")
        'evil-next-visual-line)
      (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>")
        'evil-previous-visual-line)
      (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>")
        'evil-next-visual-line)
      (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>")
        'evil-previous-visual-line)

      ;; We need to add text before we can edit it.
      (add-to-list 'evil-insert-state-modes 'git-commit-mode)

      (unless window-system
        ;; C-i is the same as tab in the terminal
        (setq evil-want-C-i-jump nil)
        ;; I'm not sure why the above variable isn't respected. I think it's evil's
        ;; fault. I didn't see any key rebinding in spacemacs.
        (define-key evil-motion-state-map "\C-i" nil)))))

(defun joe/post-init-evil-escape ()
  "Init evil-escape."
  (use-package evil-escape
    :config
    (progn
      (setq evil-escape-unordered-key-sequence t))))

(defun joe/init-hydra ()
  (use-package hydra
    :config
    (progn
      )))

(defun joe/post-init-org ()
  "Init org."
  (use-package org
    :config
    (progn


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
              ("start" . ?s) ("mid" . ?m) ("end" . ?e)
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

      (setq org-agenda-custom-commands
            '(("h" "Office and Home Lists"
               ((agenda)
                (tags-todo "work")
                (tags-todo "home")
                (tags-todo "comp")
                (tags-todo "read")))

              ("d" . "Daily")
              ("ds" "Daily Start"
               ((agenda ""
                        ((org-agenda-ndays 1)
                         (org-agenda-skip-function '(my/org-agenda-match-tags (list "daily" "start")))))))

              ("E" "Errands" tags-todo "errand")

              ;; (" " "Agenda"
              ;;  ((agenda "" ((org-agenda-ndays 1)
              ;;               (org-agenda-skip-function '(my/org-agenda-skip-without-match "daily"))))
              ;;   ;; refile tag is file level special org comment in refile.org
              ;;   (tags "refile" ((org-agenda-overriding-header "Tasks to Refile")
              ;;                   (org-tags-match-list-sublevels nil)))

              ;;   (tags-todo "start+daily+home"
              ;;              ((org-agenda-overriding-header "Daily Tasks")
              ;;               ))
              ;;   (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
              ;;              ((org-agenda-overriding-header (concat "Standalone Tasks"
              ;;                                                     (if bh/hide-scheduled-and-waiting-next-tasks
              ;;                                                         ""
              ;;                                                       " (including WAITING and SCHEDULED tasks)")))
              ;;               (org-agenda-skip-function 'bh/skip-project-tasks)
              ;;               (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
              ;;               (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
              ;;               (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
              ;;               (org-agenda-sorting-strategy
              ;;                '(category-keep))))
              ;;   )
              ;;  nil)

              ))

      (setq org-capture-templates
            `(("t" "todo" entry (file ,org-default-notes-file)
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)

              ("r" "respond" entry (file ,org-default-notes-file)
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
               :clock-in t :clock-resume t :immediate-finish t)

              ("n" "note" entry (file ,org-default-notes-file)
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)

              ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
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
          (my:replace-or-add-to-alist 'org-latex-classes tufte-handout-class))))))

(defun joe/init-persistent-scratch ()
  "Init persistent-scratch."
  (use-package persistent-scratch
    :config
    (progn
      (persistent-scratch-autosave-mode 1)
      ;; Don't clog up .emacs.d
      (setq persistent-scratch-save-file "~/.emacs-persistent-scratch")
      ;; Ensure file exists
      (unless (file-exists-p persistent-scratch-save-file)
        (write-region "" nil persistent-scratch-save-file))
      (with-current-buffer "*scratch*"
        (emacs-lisp-mode)
        (lisp-interaction-mode)
        (if (= (buffer-size) 0)
            (persistent-scratch-restore)
          (save-excursion
            (goto-char (point-max))
            (insert "\n\n;; Old Scratch\n\n"))
          (with-temp-buffer
            (insert-file-contents persistent-scratch-save-file)
            (append-to-buffer "*scratch*" (point-min) (point-max)))))
      (defun joe--advise-write-file-for-scratch (orig-fun &rest args)
        (if (eq (current-buffer) (get-buffer "*scratch*"))
            (progn (persistent-scratch-save)
                   (message "Wrote *scratch* to %s." persistent-scratch-save-file))
          (apply orig-fun args)))
      (advice-add 'spacemacs/write-file :around
                  #'joe--advise-write-file-for-scratch))))

(defun joe/init-typescript-mode ()
  "Init typescript-mode."
  (use-package typescript
    :init
    (progn
      (with-eval-after-load 'compile
        (add-to-list 'compilation-error-regexp-alist 'typescript)
        (add-to-list 'compilation-error-regexp-alist-alist
                     '(typescript "^\\(.+?\\)(\\([[:digit:]]+\\),\\([[:digit:]]+\\)): \\(.*\\)$"
                                  1 2 3 nil 1))
        (add-to-list 'compilation-error-regexp-alist 'typescript-lint)
        ;; ornament/static/js/main.ts[176, 34]: expected parameter: 'error' to have a typedef
        (add-to-list 'compilation-error-regexp-alist-alist
                     '(typescript-lint "^\\(.+?\\)\\[\\([[:digit:]]+\\), \\([[:digit:]]+\\)\\]: \\(.*\\)$"
                                       1 2 3 nil 1))))))

(defun joe/post-init-magit ()
  "Init magit."
  (use-package magit
    :config
    (progn
      (require 'smerge-mode)
      (setq smerge-refine-ignore-whitespace nil))))

(defun joe/post-init-mu4e ()
  (use-package mu4e
    :config
    (progn
      (require 'smtpmail)

      (setq message-send-mail-function 'smtpmail-send-it
            starttls-use-gnutls t
            smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
            smtpmail-auth-credentials (expand-file-name "~/.netrc.gpg")
            smtpmail-smtp-server "smtp.gmail.com"
            smtpmail-smtp-service 587)

      (setq user-mail-address "joe.schafer@delta46.us"
            user-full-name  "Joe Schafer"
            message-signature
            (concat
             "Joe Schafer"
             "\n"))

      ;; Set default options which we customize per account in
      ;; `mu4e-account-alist'
      (setq mu4e-maildir "~/.mail"
            mu4e-trash-folder "/joesmoe10/trash"
            mu4e-refile-folder "/joesmoe10/archive"
            mu4e-get-mail-command "mbsync -a"
            mu4e-update-interval nil
            mu4e-compose-signature-auto-include nil
            mu4e-view-show-images t
            mu4e-view-show-addresses t)

      (setq mu4e-account-alist
            '(("joesmoe10"
               ;; About me
               (user-full-name "Joe Schafer")
               (user-mail-address "joesmoe10@gmail.com")
               (mu4e-compose-signature "--\nJoe Schafer")

               ;; Under each account, set the account-specific variables you want.
               (mu4e-sent-messages-behavior delete)
               (mu4e-sent-folder "/joesmoe10/sent")
               (mu4e-refile-folder "/joesmoe10/archive")
               (mu4e-drafts-folder "/joesmoe10/drafts")

               ;; SMTP
               (smtpmail-stream-type starttls)
               (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
               (smtpmail-smtp-user "joesmoe10")
               (smtpmail-smtp-server "smtp.gmail.com")
               (smtpmail-smtp-service 587))

              ("delta46"
               ;; About me
               (user-full-name "Joe Schafer")
               (user-mail-address "joe.schafer@delta46.us")
               (mu4e-compose-signature "--\nJoe Schafer")

               ;; Under each account, set the account-specific variables you want.
               (mu4e-sent-messages-behavior delete)
               (mu4e-sent-folder "/delta46/sent")
               (mu4e-refile-folder "/delta46/archive")
               (mu4e-drafts-folder "/delta46/drafts")

               ;; SMTP
               (smtpmail-stream-type starttls)
               (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
               (smtpmail-smtp-user "joe.schafer@delta46.us")
               (smtpmail-smtp-server "smtp.gmail.com")
               (smtpmail-smtp-service 587))))

      (require 'mu4e-contrib)

      ;; Pandoc and html2markdown interpret HTML tables literally which is less
      ;; than ideal.  `mu4e-shr2text' works well, but is a bit too literal.
      (setq mu4e-html2text-command "html2markdown --bypass-tables --ignore-links")
      (setq mu4e-html2text-command "w3m -T text/html")
      (setq mu4e-html2text-command 'mu4e-shr2text)

      (setq mu4e-html2text-command "html2text -style pretty")

      (setq mu4e-change-filenames-when-moving t)

      (mu4e/mail-account-reset))))


(defun joe/init-otb ()
  "Init otb."
  (use-package otb
    :config
    (progn
      (joe/set-leader-keys
       "cb" 'joe-blog-compile
       "cB" '(lambda () (interactive) (joe-blog-compile 'force))
       "cp" 'joe-blog-publish
       "cP" 'joe-blog-purge-everything))
    ))

(defun joe/init-org-ref ()
  "Init org-ref."
  (use-package org-ref
    :config
    (progn
      ;; optional but very useful libraries in org-ref
      ;; (require 'doi-utils)
      ;; (require 'jmax-bibtex)
      ;; (require 'pubmed)
      ;; (require 'arxiv)
      ;; (require 'sci-id)
      ;; (require 'bibtex)
      ;; (require 'reftex-cite)
      ;; (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib")
      ;;       org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
      ;;       org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
      ;;       org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/"
      ;;       helm-bibtex-bibliography "~/Dropbox/bibliography/references.bib"
      ;;       helm-bibtex-library-path "~/Dropbox/bibliography/bibtex-pdfs"
      ;;       helm-bibtex-notes-path "~/Dropbox/bibliography/helm-bibtex-notes"
      ;;       bibtex-file-path ".:~/Dropbox/bibliography/"
      ;;       )
      )))

(defun joe/init-openwith ()
  (use-package openwith
    :config
    (progn
      (setq openwith-associations
            (list
             '("\\.pdf\\'" "zathura" (file))))
      )))

(defun joe/init-overseer ()
  (use-package overseer
    :config
    (progn
      (add-to-list 'exec-path (expand-file-name "~/.cask/bin"))
              )))

(defun joe/post-init-s ()
  "Init s ()."
  (use-package s
    :config
    ))

(defun joe/init-sx ()
  (use-package sx
    :config
    (progn
      (evil-set-initial-state 'sx-question-mode 'emacs)
      (evil-set-initial-state 'sx-question-list-mode 'emacs)
      (evil-leader/set-key "xss" 'sx-search)
      )))

(provide 'packages)
;;; packages.el ends here
