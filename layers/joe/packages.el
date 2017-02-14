;;; packages.el --- joe Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org


;;; Commentary:
;;

;;; Code:

(defvar joe-packages
  '(
    auto-dim-other-buffers
    auto-yasnippet
    avy
    bbdb
    company
    (conf-mode :location built-in)
    ;; (doc-popup :location local)
    evil
    evil-escape
    evil-replace-with-register
    fill-column-indicator
    flycheck
    framemove
    git-gutter
    helm
    helm-projectile
    js2-mode
    magit
    magit-filenotify
    markdown-mode
    markdown-preview-mode
    ;; mu4e
    overseer ; ERT-runner integration
    org
    org-agenda
    org-autolist
    (org-babel :location built-in)
    org-download
    (org-drill :location built-in)
    projectile
    racer
    smartparens
    string-inflection
    (tlp :location local)
    tern
    typescript
    web-mode
    zeal-at-point
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

(defun joe/post-init-avy ()
  "Init avy."
  (use-package avy
    :config
    (progn

      (defun my:avy-back-to-start ()
        "Go back to where we invoked avy.
This is par tof avy-action-copy, so that function doesn't need it."
        (let ((dat (ring-ref avy-ring 0)))
          (select-frame-set-input-focus
           (window-frame (cdr dat)))
          (select-window (cdr dat))
          (goto-char (car dat))))

      (defun my:avy-action-copy-and-yank (pt)
        "Copy and yank sexp starting on PT."
        (avy-action-copy pt)
        (yank))

      (defun my:avy-action-copy-and-yank-line (pt)
        "Copy and yank the whole line on PT."
        (kill-new (buffer-substring (line-beginning-position) (line-end-position)))
        (my:avy-back-to-start)
        (save-excursion
          (beginning-of-line)
          (insert "\n")
          (forward-char -1)
          (yank)))
      ;; Perform actions on avy targets.  Press one of the following chars
      ;; before selecting a target.  For example if you want to delete the line
      ;; with the `fg' target, `avy-goto-line x fg'.
      ;; http://emacs.stackexchange.com/questions/27979
      (setq avy-dispatch-alist
            '((?x . avy-action-kill-move)
              (?X . avy-action-kill-stay)
              (?m . avy-action-mark)
              (?i . avy-action-ispell)
              (?p . my:avy-action-copy-and-yank)
              (?P . my:avy-action-copy-and-yank-line)
              (?n . avy-action-copy)
              ))
      ))
  )

(defun joe/init-bbdb ()
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

(defun joe/post-init-company ()
  "Init company."
  (use-package company
    :config
    (progn
      (defun my:complete-with-dot-and-complete-again ()
        "Complete word at point, insert a period and complete again."
        (interactive)
        (company-complete-selection)
        (insert ".")
        (company-complete-common))

      (define-key company-active-map (kbd "C-.")
        'my:complete-with-dot-and-complete-again))))

(defun joe/post-init-conf-mode ()
  "Post init conf-mode."
  (use-package conf-mode
    :config
    (progn
      ;; Interferes with the agenda.
      (define-key conf-mode-map (kbd "C-c C-a") nil))))

(defun joe/init-doc-popup ()
  "Init doc-popup."
  (use-package doc-popup
    :config
    (progn
      (defvar evil-normal-state-map)
      (define-key evil-normal-state-map "gh" 'doc-popup-show-at-point))))

(defun joe/init-evil-replace-with-register ()
  "Init evil-replace-with-register."
  (use-package evil-replace-with-register
    :config
    (progn
      (evil-replace-with-register-install)
      )))

(defun joe/post-init-flycheck ()
  "Post init flycheck."

  (use-package flycheck
    :config
    (progn
      (flycheck-define-checker markdown-markdownlint
        "Markdown checker using mdl.

See URL `https://github.com/mivok/markdownlint'."
        :command ("markdownlint" source)
        ;; :standard-input t
        :error-patterns
        ((error line-start
                (file-name) ": " line ": " (id (one-or-more alnum)) " " (message)
                line-end))

        :modes (markdown-mode gfm-mode))

      (add-to-list 'flycheck-checkers 'markdown-markdownlint))))

(defun joe/init-framemove ()
  "Init framemove."
  (use-package framemove
    :config
    (progn
      (framemove-default-keybindings)
      (setq framemove-hook-into-windmove t))))

(defun joe/post-init-git-gutter ()
  (use-package git-gutter
    :config
    (progn
      (add-hook 'magit-post-refresh-hook #'git-gutter:update-all-windows))))

(defun joe/post-init-evil ()
  "Init evil."
  (eval-when-compile
    (require 'evil-macros))

  (define-key evil-normal-state-map (kbd "[ C-<return>")
    'my:insert-newline-above-and-follow)

  (define-key evil-normal-state-map (kbd "] C-<return>")
    'my:insert-newline-below-and-follow)

  (define-key evil-normal-state-map (kbd "[ RET")
    'my:insert-newline-above-and-stay)

  (define-key evil-normal-state-map (kbd "] RET")
    'my:insert-newline-below-and-stay)

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
  (add-to-list 'evil-insert-state-modes 'git-commit-mode))

(defun joe/post-init-evil-escape ()
  "Init evil-escape."
  (use-package evil-escape
    :config
    (progn
      (setq evil-escape-unordered-key-sequence t))))

(defun joe/post-init-fill-column-indicator ()
  "Init fill-column-indicator."
  (use-package fill-column-indicator
    :config
    (progn
      (add-hook 'prog-mode-hook 'turn-on-fci-mode)

      (require 'color)

      (defun my:color-is-closer-to-white-p (color)
        "Returns t if COLOR is closer to white than black."
        (< (color-distance color "white") (color-distance color "black")))

      (defun my:get-subtle-color-from-background (percent-difference)
        "Gets a shade PERCENT-DIFFERENCE from the current background color.
If the color is closer to white, multiply percent-difference by 2
so it's easier to see."
        (let* ((current-background-color (face-background 'default)))
          (if (my:color-is-closer-to-white-p current-background-color)
              (color-darken-name current-background-color (* 2 percent-difference))
            (color-lighten-name current-background-color percent-difference))))

      (defun my:change-fci-color (&rest args)
        "Change the fill-column-indicator based on the background.
ARGS is only used because we use this function as advice after
`load-theme'."
        (setq fci-rule-color (my:get-subtle-color-from-background 10))
        (let* ((wins (window-list (selected-frame) 'no-minibuf))
               (bufs (delete-dups (mapcar #'window-buffer wins))))
          (dolist (buf bufs)
            (with-current-buffer buf
              (when fci-mode
                ;; See http://emacs.stackexchange.com/questions/27580 for code that
                ;; should work but doesn't.
                ;; (fci-make-overlay-strings)
                ;; (fci-update-all-windows t)
                (turn-off-fci-mode)
                (turn-on-fci-mode)
                )))))

      ;; Need to run once for inital theme if different from Spacemacs default.
      (my:change-fci-color)

      (advice-add 'load-theme :after 'my:change-fci-color)

      (defun fci-mode-override-advice (&rest args))
      (advice-add 'org-html-fontify-code :around
                  (lambda (fun &rest args)
                    (advice-add 'fci-mode :override #'fci-mode-override-advice)
                    (let ((result (apply fun args)))
                      (advice-remove 'fci-mode #'fci-mode-override-advice)
                      result))))))

(defun joe/post-init-helm ()
  "Post init helm."
  (use-package helm
    :config
    (progn

      (defun my:get-zsh-history-string ()
        (split-string (shell-command-to-string "my_get_history.sh") "\n"))

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
        (require 'helm-org)
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
        "pj" #'my:helm-projectile-changed-master

        "hr" #'helm-regexp)
      ;; To re-override magit
      (with-eval-after-load 'magit
        (spacemacs/set-leader-keys
          "gd" #'helm-semantic-or-imenu))
      (setq helm-semantic-fuzzy-match t)
      (setq helm-imenu-fuzzy-match t)
      )))

(defun joe/post-init-helm-projectile ()
  "Post init helm."
  (use-package helm-projectile
    :config
    (progn
      (defun my:helm-projectile-changed-master ()
        "Finds files changed from master in the current project."
        (interactive)
        (let ((changed-files (my:project-files-changed-from-git5-sync)))
          (if changed-files
              (helm :sources (helm-build-sync-source "Changed files"
                               :candidates changed-files
                               :action (lambda (file)
                                         (find-file
                                          (concat (projectile-project-root)
                                                  file)))
                               )
                    :buffer "*helm projectile*"
                    :prompt (projectile-prepend-project-name "Find file: "))
            (helm-projectile-find-file))))

      (defun my:project-files-changed-from-master ()
        "Returns a list of files changed from master in the current project."
        (cl-mapcan
         #'my:get-files-changed-from-master-in-dir
         (projectile-get-project-directories)))

      (defun my:project-files-changed-from-git5-sync ()
        "Returns a list of files changed from master in the current project."
        (cl-mapcan
         #'my:get-files-changed-from-git5-sync-in-dir
         (projectile-get-project-directories)))


      (defun my:get-files-changed-from-git-command (directory command)
        "Returns files changed from HEAD to the commit returned by COMMAND."
        (let* ((projectile-root (projectile-project-root))
               (git-root (replace-regexp-in-string
                          "\n" ""
                          (shell-command-to-string
                           "git rev-parse --show-toplevel")))
               (default-directory directory)
               (changed-files (split-string
                               (shell-command-to-string command)
                               "\0"))
               (changed-files-no-empty (delete "" changed-files))
               (get-relative-project-file-name
                (lambda (file)
                  (file-relative-name (expand-file-name file git-root)
                                      projectile-root))))
          (projectile-adjust-files
           (mapcar get-relative-project-file-name
                   changed-files-no-empty))))

      (defun my:get-files-changed-from-master-in-dir (directory)
        "Returns list of files changed from master branch in DIRECTORY."
        (my:get-files-changed-from-git-command
         directory "git diff -z --name-only master"))

      (defun my:get-files-changed-from-git5-sync-in-dir (directory)
        "Returns list of files changed from master branch in DIRECTORY."
        (let ((git5-sync-hash
               (shell-command-to-string
                (concat "git log --max-count=1 --format='%H' "
                        "--grep 'git5:'"))))
          (my:get-files-changed-from-git-command
           directory
           (format "git diff -z --name-only %s" git5-sync-hash))))
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
        (message "eslint fixing the file %s" (buffer-file-name))
        (shell-command (concat "eslint --fix " (buffer-file-name))))

      (defun my:js-clang-format-file ()
        (interactive)
        (message "clang formatting file %s" (buffer-file-name))
        (shell-command (concat "clang-format -i -style=Google"
                               (buffer-file-name)))
        (revert-buffer t t))

      (defun eslint-fix-file-and-revert ()
        (interactive)
        (eslint-fix-file)
        (revert-buffer t t))

      (defun my:get-beginning-of-string-point ()
        "Gets the beginning of the string at point.
If point is not in a string or at the beginning of a string, return nil."
        (save-excursion
          (if (looking-at "['\"]")
              ;; Assume we're at the beginning of a string so just return.
              (point)
            ;; We're not at the beginning of a string.
            (if (not (nth 3 (syntax-ppss)))
                ;; We're not inside a string.
                nil
              ;; We're inside a string.
              (while (nth 3 (syntax-ppss))
                (forward-char -1))
              (point)))))

      (defun my:js-clang-format-string (js-code)
        "Format JS-CODE with clang-format."
        (let ((temp-file (make-temp-file "js-clang-format")))
          (write-region js-code nil temp-file)
          (shell-command (format "clang-format -i %s" temp-file))
          (prog1
              (with-temp-buffer
                (insert-file-contents temp-file)
                (buffer-string))
            (delete-file temp-file))))

      (defun my:extract-string-contents ()
        "Gets the contents of the string at point.
If point is not in a string, return nil."
        (save-excursion
          (let ((string-start (my:get-beginning-of-string-point))
                string-end)
            (when string-start
              (goto-char string-start)
              (forward-sexp)
              (setq string-end (point))
              (buffer-substring-no-properties (1+ string-start) (1- string-end))))))

      (defun my:js-format-js-code-in-string ()
        "Replaces the current string of JS code with nicely formatted code.
Uses template literals to support multiple lines of code."
        (interactive)
        (let ((string-contents (my:extract-string-contents))
              (string-start (my:get-beginning-of-string-point)))
          (when string-contents
            (goto-char string-start)
            (kill-sexp)
            (insert (format "`\n%s`" (my:js-clang-format-string string-contents)))
            )))

      (defun my:js-format-file ()
        "Runs clang format on a file."
        (interactive)
        (when (buffer-file-name)
          (shell-command (format "clang-format -i %s" (buffer-file-name)))))

      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "rf" #'eslint-fix-file-and-revert
        "fs" #'my:js-format-js-code-in-string
        "ff" #'my:js-format-file
        )

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
                           '("gtasks/ (goog.org)"
                             "~/gdrive/gorg/goog.org"
                             "^\\(\\*+\\)\\(?: +\\(CANCELLED\\|DONE\\|HOLD\\|NEXT\\|TODO\\|WAITING\\)\\)?\\(?: +\\(\\[#.\\]\\)\\)?\\(?: +\\(?:\\[[0-9%/]+\\] *\\)*\\(Work Tasks\\)\\(?: *\\[[0-9%/]+\\]\\)*\\)\\(?:[ 	]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[ 	]*$"
                             1)))

      (defun my:org-agenda-refile-to-gtd ()
        (interactive)
        (org-agenda-refile nil
                           ("htasks/ (gtd.org)"
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

(defun joe/post-init-org-download ()
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

(defun joe/post-init-markdown-mode ()
  "Post init conf-mode."
  (use-package markdown-mode
    :config
    (progn
      ;; Interferes with the agenda.
      (define-key markdown-mode-map (kbd "C-c C-a") nil))))


(defun joe/init-markdown-preview-mode ()
  "Init markdown-preview-mode."
  (use-package markdown-preview-mode
    :config
    (progn
      )))

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

(defun joe/init-org-babel ()
  (use-package org-babel
    :init
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (sh . t)
       (shell . t)
       (haskell . t)
       (js . t)
       (latex . t)
       (gnuplot . t)
       (C . t)
       (sql . t)
       (ditaa . t)
       ))
    ))

(defun joe/init-org-drill ()
  "Init org-drill."
  (use-package org-drill
    :config

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

    (spacemacs/declare-prefix "," "joe")
    (spacemacs/declare-prefix ",f" "files")
    (spacemacs/declare-prefix ",fe" "layer")
    (spacemacs/declare-prefix ",fd" "dotfiles")
    (spacemacs/declare-prefix ",fg" "gdrive")
    (spacemacs/declare-prefix ",d" "drill")
    (spacemacs/declare-prefix ",p" "paste")
    (spacemacs/declare-prefix ",y" "yank")
    (spacemacs/declare-prefix ",x" "text")
    (spacemacs/declare-prefix ",xu" "url")

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

(defun joe/post-init-racer ()
  "Init racer."
  (use-package racer
    :config
    (exec-path-from-shell-copy-env "RUST_SRC_PATH")))

(defun joe/post-init-smartparens ()
  "Init smartparens."
  (use-package smartparens
    :config
    (spacemacs/set-leader-keys
      "k C-h" #'sp-beginning-of-sexp
      "k C-l" #'sp-beginning-of-next-sexp)
    (sp-with-modes 'org-mode
      (sp-local-pair "~" "~"))
    ))

(defun joe/init-string-inflection ()
  (use-package string-inflection
    :init
    (progn
      (spacemacs/set-leader-keys
        "xii" 'string-inflection-all-cycle
        "xiu" 'string-inflection-underscore
        "xiU" 'string-inflection-upcase
        "xik" 'string-inflection-kebab-case
        "xic" 'string-inflection-lower-camelcase
        "xiC" 'string-inflection-camelcase))))

(defun joe/init-tlp ()
  "Init tlp."
  (use-package tlp
    :config
    (progn
      (spacemacs/set-leader-keys
        "h C-o" #'tlp-start-work))))

(defun joe/post-init-tern ()
  "Init tern."
  (use-package tern
    :config

    ))
(defun joe/post-init-typescript ()
  "Init typescript-mode."
  (use-package typescript
    :config
    (progn
      (setq-default typescript-indent-level 2)
      ))

  )

(defun joe/post-init-web-mode ()
  "Init web-mode."
  (use-package web-mode
    :config
    (progn
      (add-hook 'web-mode-hook 'turn-off-show-smartparens-mode))))

(defun joe/init-zeal-at-point ()
  "Init zeal-at-point."
  (use-package zeal-at-point
    :config
    (progn
      (defun my:pick-docset ()
        "Choose custom docsets based on the buffer."
        (setq zeal-at-point-docset
              (cond
               ((equal major-mode 'js2-mode) "goog,javascript,angularjs")
               (t nil))))
      (add-hook 'prog-mode-hook #'my:pick-docset))))

(provide 'packages)
;;; packages.el ends here
