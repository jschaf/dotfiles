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
    auto-dim-other-buffers
    auto-yasnippet
    bbdb
    ;; (doc-popup :location local)
    evil
    evil-escape
    framemove
    helm
    js2-mode
    magit
    magit-filenotify
    ;; mu4e
    overseer ; ERT-runner integration
    org
    org-agenda
    org-autolist
    (org-download :location built-in)
    (org-drill :location built-in)
    (ox-publish :location built-in)
    projectile
    smartparens
    typescript
    )
  "List of all packages to install and/or initialize.
Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar joe-excluded-packages '()
  "List of packages to exclude.")

(defun joe/init-auto-dim-other-buffers ()
  "Init auto-dim-other-buffers."
  (use-package auto-dim-other-buffers
    :init
    (progn
      (defun my:reset-auto-dim-face (&rest args)
        "Adjust `auto-dim-other-buffers-face' to the current background color.
ARGs is unused and are only for when this function is used as advice."
        (interactive)
        (let* ((percent-to-darken 3.5)
               (current-background-color (face-background 'default))
               (new-auto-dim-background-color
                (color-darken-name current-background-color percent-to-darken)))
          (set-face-background 'auto-dim-other-buffers-face
                               new-auto-dim-background-color)))
      (add-hook 'after-init-hook #'auto-dim-other-buffers-mode)
      (add-hook 'after-init-hook #'my:reset-auto-dim-face)
      (advice-add 'load-theme :after 'my:reset-auto-dim-face)
      )))

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

(defun joe/init-bbdb ()
  "Init bbdb."
  (use-package bbdb
    :config
    (progn
      (setq bbdb-file "~/gdrive/contacts/bbdb")

      (defvar my:bbdb-asynk-host "gc_joesmoe10")
      (defvar my:bbdb-asynk-name "joesmoe10")
      (defvar my:bbdb-asynk-path (file-truename "~/prog/ASynK/asynk.py"))
      (defvar my:bbdb-asynk-user-dir (file-truename "~/.asynk"))

      (defun my:bbdb-asynk-sync ()
        "Sync bbdb with ASynK."
        (interactive)
        (require 'netrc)
        (with-temp-buffer
          (let* ((netrc (netrc-parse "~/.netrc.gpg"))
                 (hostentry (netrc-machine netrc my:bbdb-asynk-host))
                 (default-directory my:bbdb-asynk-user-dir))
            (unless hostentry (error "Could not find %s in ~/.netrc.gpg"
                                     my:bbdb-asynk-host))
            (message "Running AsynK...")
            (insert (netrc-get hostentry "login")
                    "\n"
                    (netrc-get hostentry "password")
                    "\n")
            (let ((proc (start-process "ASynK" "*ASynK*"
                                       "python2" my:bbdb-asynk-path
                                       "--op" "sync"
                                       "--user-dir" my:bbdb-asynk-user-dir
                                       "--name" my:bbdb-asynk-name)))
              (set-process-sentinel proc (lambda (p s)
                                           (if (equal s "finished\n")
                                               (message "ASynK %s" s)
                                             (display-buffer (process-buffer p))
                                             (error "ASynK: %s" s))))
              (insert (netrc-get hostentry "login")
                      "\n"
                      (netrc-get hostentry "password")
                      "\n")
              (with-current-buffer (process-buffer proc)
                (erase-buffer))
              ;; (display-buffer (process-buffer proc))
              (process-send-region proc (point-min) (point-max))
              (process-send-eof proc)
              (or (not (buffer-live-p bbdb-buffer))
                  (bbdb-revert-buffer nil 'noconfirm))))))

      (with-eval-after-load 'mu4e
        (advice-add 'mu4e-quit :after 'my:bbdb-asynk-sync))

      )))

(defun joe/init-doc-popup ()
  "Init doc-popup."
  (use-package doc-popup
    :config
    (progn
      (defvar evil-normal-state-map)
      (define-key evil-normal-state-map "gh" 'doc-popup-show-at-point))))

(defun joe/init-framemove ()
  "Init framemove."
  (use-package framemove
    :config
    (progn
      (framemove-default-keybindings)
      (setq framemove-hook-into-windmove t))))

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

      ;; http://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard
      (fset 'evil-visual-update-x-selection 'ignore)

      ;; We need to add text before we can edit it.
      (add-to-list 'evil-insert-state-modes 'git-commit-mode)
      )))

(defun joe/post-init-evil-escape ()
  "Init evil-escape."
  (use-package evil-escape
    :config
    (progn
      (setq evil-escape-unordered-key-sequence t))))

(defun joe/post-init-helm ()
  "Post init helm."
  (use-package helm
    :config
    (progn

      (defun my:get-zsh-history-string ()
        (with-temp-buffer
          (insert-file-contents "~/.zsh_history")
          (let ((lines (split-string (buffer-string) "\n" 'omit-nulls))
                (remove-zsh-prefix (lambda (line)
                                     (nth 1 (s-split-up-to ";" line 1)))))
            (message "%s" lines)
            (reverse (-map remove-zsh-prefix lines)))))

      (defun my:helm-zsh-history ()
        "Insert a command from the bash history."
        (interactive)

        (let ((candidates (my:get-zsh-history-string)))
          (helm :sources `((name . "zsh history")
                           (candidates . ,candidates)
                           (action . insert))
                :candidate-number-limit 10000)))

      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
      (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
      (define-key helm-map (kbd "C-z")  'helm-select-action)
      ;; This is way more handy than original eval-expression
      (global-set-key (kbd "M-:") 'eval-expression)
      ;; Overrides magit-diff, but I never use that.

      (define-key lisp-mode-shared-map [C-tab] #'helm-lisp-completion-at-point)

      (defun my:helm-org-agenda-files-and-archive-headings ()
        "Preconfigured helm for org headings with archives."
        (interactive)
        (helm :sources (helm-source-org-headings-for-files (org-agenda-files t t))
              :candidate-number-limit 99999
              :truncate-lines helm-org-truncate-lines
              :buffer "*helm org headings*"))

      (spacemacs/set-leader-keys
        "gd" #'helm-semantic-or-imenu
        "ha" #'helm-apropos
        "ho" #'helm-org-agenda-files-headings
        "hO" #'my:helm-org-agenda-files-and-archive-headings
        "hhb" #'my:helm-zsh-history

        "hr" #'helm-regexp)
      ;; To re-override magit
      (with-eval-after-load 'magit
        (spacemacs/set-leader-keys
          "gd" #'helm-semantic-or-imenu))
      (setq helm-semantic-fuzzy-match t)
      (setq helm-imenu-fuzzy-match t)
      )))

(defun joe/post-init-js2-mode ()
  "Init js2-mode."
  (use-package js2-mode
    :config
    (progn
      (setq-default js2-basic-offset 2)
      (setq-default js2-strict-trailing-comma-warning nil)
      (setq-default js2-mode-show-strict-warnings nil)

      (defun my:set-js2-mode-name-to-js2 ()
        (when (string-equal mode-name "Javascript-IDE")
          (setq mode-name "JS")))

      (add-hook 'js2-mode-hook #'my:set-js2-mode-name-to-js2)

      (defun eslint-fix-file ()
        (interactive)
        (message "eslint fixing the file" (buffer-file-name))
        (shell-command (concat "eslint --fix " (buffer-file-name))))

      (defun eslint-fix-file-and-revert ()
        (interactive)
        (eslint-fix-file)
        (revert-buffer t t))

      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "rf" #'eslint-fix-file-and-revert)

      (setq js2-jsdoc-typed-tag-regexp
            (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
                    (regexp-opt
                     '("enum"
                       "extends"
                       "field"
                       "id"
                       "implements"
                       "lends"
                       "mods"
                       "requires"
                       "return"
                       "returns"
                       "throw"
                       "throws"))
                    "\\)\\)\\s-*\\({[^}]+}\\)?"))

      (setq js2-jsdoc-arg-tag-regexp
            (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
                    (regexp-opt
                     '("alias"
                       "augments"
                       "borrows"
                       "callback"
                       "bug"
                       "base"
                       "config"
                       "default"
                       "define"
                       "exception"
                       "func"
                       "function"
                       "member"
                       "memberOf"
                       "method"
                       "module"
                       "name"
                       "namespace"
                       "since"
                       "suppress"
                       "this"
                       "throws"
                       "type"
                       "version"

                       ;; Added from Annotation.java in jscomp.
                       "idGenerator"
                       "meaning"  ;; Looks like it's for localization.
                       "modifies" ;; Only for externs.
                       "template"
                       "typedef"
                       "visibility" ;; Controls blaze build visibility.
                       ))
                    "\\)\\)\\s-+\\([^ \t]+\\)"))

      (setq js2-jsdoc-empty-tag-regexp
            (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
                    (regexp-opt
                     '("addon"
                       "author"
                       "class"
                       "const"
                       "constant"
                       "constructor"
                       "constructs"
                       "deprecated"
                       "desc"
                       "description"
                       "event"
                       "example"
                       "exec"
                       "export"
                       "fileoverview"
                       "final"
                       "func"
                       "function"
                       "hidden"
                       "ignore"
                       "implicitCast"
                       "inheritDoc"
                       "inner"
                       "interface"
                       "license"
                       "method"
                       "noalias"
                       "noshadow"
                       "notypecheck"
                       "override"
                       "owner"
                       "preserve"
                       "preserveTry"
                       "private"
                       "protected"
                       "public"
                       "static"
                       "supported"

                       ;; Added from Annotation.java in jscomp.
                       "ngInject"
                       "abstract"
                       "copyright"
                       "disposes"
                       "externs"
                       "record"
                       "jaggerInject"
                       "jaggerModule"
                       "jaggerProvidePromise"
                       "jaggerProvide"
                       "nocollapse"
                       "nosideeffects"
                       "nocompile"
                       "package" ;; Indicates package-private.
                       "polymerBehavior"
                       "struct"
                       "unrestricted" ;; Mark class that's not a @struct or @dict.
                       "wizaction"
                       ))
                    "\\)\\)\\s-*"))

      (evil-define-key 'normal js2-mode-map
        (kbd "C-c C-a") #'org-agenda)

      (evil-define-key 'normal js2-mode-map
        "za" 'js2-mode-toggle-element
        "ze" 'js2-mode-toggle-element)

      )))

(defun joe/post-init-org ()
  "Init org."
  (use-package org
    :config
    (progn

      (load "~/.dotfiles/layers/joe/local/my-org.el")
      (when (file-exists-p "~/.dotfiles/layers/joe/local/buggy.el")
        (load "~/.dotfiles/layers/joe/local/buggy.el"))
      )))

(defun joe/post-init-org-agenda ()
  "Init org-agenda."
  (use-package org-agenda
    :config
    (progn
      (defun my:org-agenda-refile-to-goog ()
        (interactive)
        (org-agenda-refile nil
                           '("Work Tasks/ (goog.org)"
                             "~/gdrive/gorg/goog.org"
                             "^\\(\\*+\\)\\(?: +\\(CANCELLED\\|DONE\\|HOLD\\|NEXT\\|TODO\\|WAITING\\)\\)?\\(?: +\\(\\[#.\\]\\)\\)?\\(?: +\\(?:\\[[0-9%/]+\\] *\\)*\\(Work Tasks\\)\\(?: *\\[[0-9%/]+\\]\\)*\\)\\(?:[ 	]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[ 	]*$"
                             1)))

      (defun my:org-agenda-refile-to-gtd ()
        (interactive)
        (org-agenda-refile nil
                           ("Tasks/ (gtd.org)"
                            "/Users/jschaf/gdrive/org/gtd.org"
                            "^\\(\\*+\\)\\(?: +\\(CANCELLED\\|DONE\\|HOLD\\|NEXT\\|TODO\\|WAITING\\)\\)?\\(?: +\\(\\[#.\\]\\)\\)?\\(?: +\\(?:\\[[0-9%/]+\\] *\\)*\\(Tasks\\)\\(?: *\\[[0-9%/]+\\]\\)*\\)\\(?:[ 	]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[ 	]*$"
                            179)))
      (spacemacs/set-leader-keys-for-major-mode
        'org-agenda-mode
        "rr" 'org-agenda-refile
        "rg" 'my:org-agenda-refile-to-gtd
        "rw" 'my:org-agenda-refile-to-goog
        ))))

(defun joe/init-org-autolist ()
  "Init org-autolist."
  (use-package org-autolist
    :config
    (progn
      (add-hook 'org-mode-hook 'org-autolist-mode)
      ))
  )

(defun joe/init-org-download ()
  "Init org-download."
  (use-package org-download
    :config
    (progn
      (setq-default org-download-image-dir "~/gdrive/org/images"))))

(defun joe/post-init-magit ()
  "Init magit."
  (use-package magit
    :config
    (progn
      (require 'smerge-mode)
      (setq smerge-refine-ignore-whitespace nil))))

(defun joe/init-magit-filenotify ()
  "Init org-download."
  (use-package magit-filenotify
    :config
    (progn
      (with-eval-after-load 'magit
        (add-hook 'magit-status-mode-hook 'magit-filenotify-mode)))))

(defun joe/post-init-mu4e ()
  "Init mu4e."
  (use-package mu4e
    :config
    (progn
      (require 'smtpmail)
      (require 'org-mu4e)

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

      ;;send mail using postfix
      (setq send-mail-function 'sendmail-send-it)
      (setq message-send-mail-function 'message-send-mail-with-sendmail)

      (setq mu4e-maildir-shortcuts
            '(("/joesmoe10/inbox" . ?i)
              ("/delta46/inbox" . ?I)
              ("/joesmoe10/archive" . ?e)
              ("/delta46/archive" . ?E)))

      (require 'mu4e-contrib)

      ;; Pandoc and html2markdown interpret HTML tables literally which is less
      ;; than ideal.  `mu4e-shr2text' works well, but is a bit too literal.
      ;; (setq mu4e-html2text-command "html2markdown --bypass-tables --ignore-links")
      ;; (setq mu4e-html2text-command "w3m -T text/html")
      ;; (setq mu4e-html2text-command 'mu4e-shr2text)
      ;; (setq mu4e-html2text-command "html2text -utf8 -width 72 -style pretty")
      (setq mu4e-html2text-command "html2text -nobs -utf8")

      (setq mu4e-change-filenames-when-moving t)
      (require 'bbdb)
      (setq bbdb-mail-user-agent 'message-user-agent)
      (add-to-list 'mu4e-view-mode-hook #'bbdb-mua-auto-update)
      (add-to-list 'mu4e-view-mode-hook #'visual-line-mode)
      (setq mu4e-compose-complete-addresses nil)
      (setq bbdb-mua-pop-up t)
      (setq bbdb-mua-pop-up-window-size 5)

      (defun my:mu4e-set-account ()
        "Set the account for composing a message."
        (let* ((account
                (if mu4e-compose-parent-message
                    (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                      (string-match "/\\(.*?\\)/" maildir)
                      (match-string 1 maildir))
                  (completing-read (format "Compose with account: (%s) "
                                           (mapconcat #'(lambda (var) (car var))
                                                      mu4e-account-alist "/"))
                                   (mapcar #'(lambda (var) (car var)) mu4e-account-alist)
                                   nil t nil nil (caar mu4e-account-alist))))
               (account-vars (cdr (assoc account mu4e-account-alist))))
          (if account-vars
              (mapc #'(lambda (var)
                        (set (car var) (cadr var)))
                    account-vars)
            (error "No email account found"))))

      (add-hook 'mu4e-compose-pre-hook 'my:mu4e-set-account)

      (mu4e/mail-account-reset))))

(defun joe/init-org-drill ()
  "Init org-drill."
  (use-package org-drill
    :config
    (defun my:work-around-org-window-drill-bug ()
      "Comment out a troublesome line in `org-toggle-latex-fragment'.
See https://bitbucket.org/eeeickythump/org-drill/issues/30 for
details."
      (save-excursion
        (let ((org-library-location (concat
                                     (locate-library "org" 'nosuffix)
                                     ".el"))
              org-buffer)
          (with-current-buffer (find-file-noselect org-library-location)
            (setq org-buffer (current-buffer))
            (goto-char (point-min))
            (if (search-forward "(set-window-start nil window-start)"
                                nil 'noerror)
                ;; This version of org has the set-window-start line.
                (if (looking-at ";; ")
                    (message
                     (concat"Already modified "
                            "`org-toggle-latex-fragment' for `org-drill'"))
                  (back-to-indentation)
                  (insert ";; ")
                  (save-buffer)
                  (byte-compile-file org-library-location)
                  (elisp--eval-defun)
                  (message
                   "Modified `org-toggle-latex-fragment' for `org-drill'"))
              ;; This version of org doesn't have the set-window-start line.
              (message "Nothing to modify for org-toggle-latex-fragment.")))
          (kill-buffer org-buffer))))

    (my:work-around-org-window-drill-bug)

    (defun my:org-set-tag-as-drill ()
      "Set the current headline as a drill tag."
      (interactive)
      (org-toggle-tag "drill"))

    (defun my:org-drill-create-template ()
      "Insert a snippet for a new drill item."
      (interactive)
      (insert "*** Item                                      :drill:\n\n")
      (insert "Question\n\n")
      (insert "**** Answer\n\n")
      (insert "Answer\n")
      (search-backward "Item")
      (forward-word)
      (forward-char))

    (defun my:org-drill-create-template-cloze ()
      "Insert a template for cloze."
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
     "dc" 'my:org-drill-create-template-cloze
     "jb" 'buggy-insert-buggy
     "jn" 'buggy-insert-natty
     "js" 'buggy-insert-slice)
    ))

(defun joe/init-overseer ()
  "Init overseer."
  (use-package overseer
    :config
    (progn
      (add-to-list 'exec-path (expand-file-name "~/.cask/bin"))
      )))

(defun joe/post-init-projectile ()
  "Init projectile."
  (use-package projectile
    :config
    (progn
      (add-to-list 'projectile-globally-ignored-directories "node_modules")
      )))

(defun joe/init-ox-publish ()
  "Init ox-publish."
  (use-package ox-publish
    :config
    (progn
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
       "cs" 'swift-plaques-compile)
      )))

(defun joe/post-init-smartparens ()
  "Init s ()."
  (use-package smartparens
    :config
    (spacemacs/set-leader-keys
      "k C-h" #'sp-beginning-of-sexp
      "k C-l" #'sp-beginning-of-next-sexp)
    (sp-with-modes 'org-mode
      (sp-local-pair "~" "~"))
    ))

(defun joe/post-init-typescript ()
  "Init typescript-mode."
  (use-package typescript
    :config
    (progn
      (setq-default typescript-indent-level 2)
      ))

  )
(provide 'packages)
;;; packages.el ends here
