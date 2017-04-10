;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun my:is-work-machine ()
  "Returns t if this is a work machine."
  (string-match ".*corp\.google\.com$" (system-name)))

(defun my:is-work-desktop ()
  "Returns t if this a work desktop."
  (string-equal "jschaf0.mtv.corp.google.com" (system-name)))

(defun my:is-work-laptop ()
  "Returns t if this a work laptop."
  (string-equal "jschaf-macbookpro.roam.corp.google.com" (system-name)))

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.dotfiles/layers/")
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     bibtex
     dash
     rust
     sql
     csv
     python
     yaml
     finance
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion
      :variables
      auto-completion-tab-key-behavior 'complete
      auto-completion-private-snippets-directory "~/.dotfiles/snippets"
      auto-completion-enable-help-tooltip t
      auto-completion-enable-sort-by-usage t
      :disabled-for
      org git)
     ;; else
     ;; better-defaults
     emacs-lisp
     git
     html
     markdown
     mu4e
     org
     javascript
     joe
     ;; TODO: disable joe-ts when typescript layer below includes tide in master
     ;; joe-ts
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     latex
     syntax-checking
     ;; TODO: enable typescript when master has updated typescript layer with
     ;; tide
     ;; typescript
     (version-control
      :variables version-control-diff-tool 'git-gutter
      :disabled-for org-mode
      )
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(noflet)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     git-gutter+
     git-gutter-fringe+
     org-pomodoro
     vi-tilde-fringe)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only)

  ;; Add google layer if on google computer.
  (when (my:is-work-desktop)
    (add-to-list 'dotspacemacs-configuration-layers 'google))
  )

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'lisp-interaction-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '(("Consolas"
                                :size 18
                                :weight normal
                                :width normal
                                :powerline-scale 1.1)
                               ("Source Code Pro"
                                :size 13
                                :weight normal
                                :width normal
                                :powerline-scale 1.1))
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'origami
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed
   )
  )

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  ;; https://github.com/syl20bnr/spacemacs/issues/7497.  Manually add path to
  ;; exec path until bug is resolved.  I think it's resolved in develop as of
  ;; October 20, 2016.
  (add-to-list 'exec-path (file-truename "~/.npm-packages/bin/")))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (defun my:is-bazel-build-file ()
    (and
     (buffer-file-name)
     (string-match "BUILD\\|WORKSPACE\\|bzl" (file-name-base (buffer-file-name)))))

  (defun my:reformat-bazel-build-file ()
    "Reformat the current BUILD file."
    (when (my:is-bazel-build-file)
      (shell-command (concat "buildifier " (buffer-file-name)))
      (revert-buffer :ignore-auto :nocofirm)))

  (defun my:add-buildifier-to-save-hook ()
    (when (my:is-bazel-build-file)
      (add-hook 'after-save-hook 'my:reformat-bazel-build-file nil 'local)))

  (unless (my:is-work-machine)
    (add-to-list 'auto-mode-alist '("BUILD\\'" . python-mode))
    (add-to-list 'auto-mode-alist '("bzl\\'" . python-mode))
    (add-to-list 'auto-mode-alist '("WORKSPACE\\'" . python-mode))
    (add-hook 'python-mode-hook 'my:add-buildifier-to-save-hook))

  ;; http://stackoverflow.com/questions/151945/
  (setq backup-directory-alist `(("." . "~/.config/.saves")))
  (setq backup-by-copying t)
  (setq delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

  (setq git-gutter-fr:side 'left-fringe)

  (setq-default git-gutter:update-interval 0)

  (when (fboundp 'fring-helper-define)
    ;; These are the defaults with git-gutter.  Spacemacs overwrites these for
    ;; some reason.
    (fringe-helper-define
      'git-gutter-fr:added nil
      "...XX..."
      "...XX..."
      "...XX..."
      "XXXXXXXX"
      "XXXXXXXX"
      "...XX..."
      "...XX..."
      "...XX...")

    (fringe-helper-define
      'git-gutter-fr:deleted nil
      "........"
      "........"
      "........"
      "XXXXXXXX"
      "XXXXXXXX"
      "........"
      "........"
      "........")

    (fringe-helper-define
      'git-gutter-fr:modified nil
      "........"
      "..XXXX.."
      "..XXXX.."
      "..XXXX.."
      "..XXXX.."
      "..XXXX.."
      "........"
      "........"))

  (add-hook 'emacs-lisp-mode-hook 'spacemacs/toggle-aggressive-indent-on)

  ;; Evilifyng org-agenda overwrites C-h trying to find a binding for
  ;; org-agenda-holiday which already has a spot with H.  Delete the C-h key
  ;; binding.  See https://github.com/syl20bnr/spacemacs/issues/3978.
  (evil-define-key 'evilified org-agenda-mode-map "\C-h" nil)

  ;; Set threshold lower after Emacs has started.  This improves responsiveness.
  (setq gc-cons-threshold (* 800 1024))

  ;; Follow symlinks to source controlled files without prompting.
  (setq vc-follow-symlinks t)

  (setq find-file-visit-truename t)

  ;; Prevent emacs from creating a symlink to indicate locking for files.
  (setq create-lockfiles nil)

  ;; Slows everything down especially for large files.
  (remove-hook 'org-mode-hook 'org-bullets-mode)

  (setq vc-handled-backends (delq 'Git vc-handled-backends))

  ;; Setup spell checking.
  (cond
   ;; Try hunspell first.
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (setq ispell-local-dictionary "en_US")
    (setq ispell-local-dictionary-alist
          ;; The list `("-d" "en_US")` contains ACTUAL parameters passed to
          ;; hunspell You could use `("-d" "en_US,en_US-med")` to check with
          ;; multiple dictionaries
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']"
             nil ("-d" "en_US") nil utf-8))))

   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

  ;; Use original file for auto-saving
  (setq auto-save-visited-file-name t)

  (cond
   ((eq system-type 'darwin)
    (setq browse-url-browser-function 'browse-url-default-macosx-browser))
   ((eq system-type 'gnu/linux)
    (setq browse-url-browser-function 'browse-url-generic)
    (setq browse-url-generic-program "google-chrome")))

  (spacemacs/toggle-mode-line-minor-modes-off)

  (defvar my:preferred-font "Consolas")
  (defvar my:preferred-font-size
    (cond ((eq system-type 'darwin) 13)
          (t 18)))
  (defvar my:preferred-power-line-scale
    (cond ((eq system-type 'darwin) 1.1)
          (t 1.1)))

  ;; Emacs 26 renamed this function without providing an
  ;; alias. https://github.com/justbur/emacs-which-key/issues/146
  (unless (fboundp 'display-buffer-in-major-side-window)
    (defalias 'display-buffer-in-major-side-window
      'window--make-major-side-window))

  (if (member my:preferred-font (font-family-list))
      (progn (message "Setting font to %s" my:preferred-font)
             (spacemacs/set-default-font `(,my:preferred-font
                                           :size ,my:preferred-font-size
                                           :weight normal
                                           :width normal
                                           :powerline-scale ,my:preferred-power-line-scale)))
    (message "Font %s not found" my:preferred-font))

  ;; Only fill comments in modes that have a comment syntax.
  (setq comment-auto-fill-only-comments t)
  (add-hook 'prog-mode-hook #'spacemacs/toggle-auto-fill-mode-on)

  (when (file-exists-p "~/.google-emacs.el")
    (load "~/.google-emacs.el"))

  (setq spacemacs-useless-buffers-regexp "ALL_BUFFERS_ARE_USEFUL")

  (spacemacs|define-custom-layout "else"
    :binding "l"
    :body
    (find-file "~/.dotfiles/layers/else/local/else-mode.el"))

  (spacemacs|define-custom-layout "config"
    :binding "c"
    :body
    (find-file "~/.dotfiles/layers/joe/config.el")
    (split-window)
    (find-file "~/.dotfiles/layers/joe/packages.el")
    )

  (defvar my:pds-dir
    "/usr/local/google/home/jschaf/depot/google3/partnerservices/pds/")

  (when (my:is-work-desktop)
    (spacemacs|define-custom-layout "prop"
      :binding "p"
      :body
      (find-file
       (concat my:pds-dir
               "sandlot/gae/app/components/survey/surveydialog/surveydialog-controller.js"))
      (split-window)
      (find-file
       (concat my:pds-dir
               "sandlot/common/types.js"))
      ))


  ;; On Mac home and end go to the document beginning or end.  Fix it to be like
  ;; PC.
  (global-set-key [home] 'move-beginning-of-line)
  (global-set-key [end] 'move-end-of-line)
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "C-M-k") #'dired-kill-subdir))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-use-notify t)
 '(bibtex-entry-format
   (quote
    (opts-or-alts required-fields numerical-fields page-dashes whitespace inherit-booktitle realign last-comma delimiters unify-case braces strings sort-fields)))
 '(css-indent-offset 2)
 '(cursor-in-non-selected-windows nil)
 '(evil-search-module (quote evil-search))
 '(evil-want-Y-yank-to-eol t)
 '(exec-path-from-shell-check-startup-files nil)
 '(git-commit-summary-max-length 65)
 '(git-gutter-fr:side (quote left-fringe) t)
 '(global-vi-tilde-fringe-mode nil)
 '(hl-todo-keyword-faces
   (quote
    (("HOLD" . "#d0bf8f")
     ("TODO" . "#cc9393")
     ("NEXT" . "#dca3a3")
     ("THEM" . "#dc8cc3")
     ("PROG" . "#7cb8bb")
     ("OKAY" . "#7cb8bb")
     ("DONT" . "#5f7f5f")
     ("FAIL" . "#8c5353")
     ("DONE" . "#afd8af")
     ("NOTE" . "#d0bf8f")
     ("KLUDGE" . "#d0bf8f")
     ("HACK" . "#d0bf8f")
     ("FIXME" . "#cc9393")
     ("XXX" . "#cc9393")
     ("XXXX" . "#cc9393")
     ("???" . "#cc9393")
     ("WAIT" . "#d0bf8f")
     ("CANX" . "#d0bf8f"))))
 '(httpd-port 31410)
 '(indicate-buffer-boundaries (quote left))
 '(js-doc-return-line " * @return {}
")
 '(js-expr-indent-offset 2)
 '(js-indent-level 2)
 '(js-paren-indent-offset 2)
 '(js-switch-indent-offset 2)
 '(js2-bounce-indent-p nil)
 '(large-file-warning-threshold 60000000)
 '(ledger-highlight-xact-under-point nil)
 '(magit-diff-refine-hunk (quote all))
 '(org-adapt-indentation nil)
 '(org-attach-auto-tag "attach")
 '(org-drill-add-random-noise-to-intervals-p t)
 '(org-drill-forgetting-index 20)
 '(org-drill-hide-item-headings-p t)
 '(org-drill-learn-fraction 0.25)
 '(org-drill-left-cloze-delimiter "[(")
 '(org-drill-right-cloze-delimiter ")]")
 '(org-drill-sm5-initial-interval 1.4)
 '(org-drill-spaced-repetition-algorithm (quote sm5))
 '(org-edit-src-content-indentation 0)
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s"))))
 '(org-habit-preceding-days 10)
 '(org-html-checkbox-type (quote html))
 '(org-lowest-priority 69)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-checklist org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(org-pomodoro-finished-sound-p t)
 '(org-pomodoro-format "P~%s")
 '(org-pomodoro-play-sounds nil)
 '(org-reverse-note-order t)
 '(package-selected-packages
   (quote
    (winum fuzzy zeal-at-point dash-at-point helm-dash noflet string-inflection org-ref key-chord helm-bibtex parsebib biblio biblio-core web-server evil-replace-with-register sql-indent csv-mode toml-mode racer flycheck-rust seq cargo rust-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic yaml-mode pcache hide-comnt ledger-mode flycheck-ledger pug-mode flyspell-correct-helm flyspell-correct auto-dictionary origami overseer org-autolist markdown-preview-mode websocket magit-filenotify framemove bbdb auto-dim-other-buffers web-mode web-beautify tagedit smeargle slim-mode scss-mode sass-mode orgit org-projectile org-present org org-download mu4e-maildirs-extension mu4e-alert ht alert log4e gntp mmm-mode markdown-toc markdown-mode magit-gitflow livid-mode skewer-mode simple-httpd less-css-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc jade-mode htmlize helm-themes helm-swoop helm-projectile helm-mode-manager helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md flycheck-pos-tip flycheck evil-magit magit magit-popup git-commit with-editor emmet-mode diff-hl company-web web-completion-data company-tern dash-functional tern company-statistics company-quickhelp pos-tip company-auctex company coffee-mode auto-yasnippet yasnippet auctex ace-jump-helm-line ac-ispell auto-complete ws-butler window-numbering which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline smex restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide ido-vertical-mode hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation help-fns+ helm-make helm helm-core google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump popup f s diminish define-word counsel-projectile projectile pkg-info epl counsel swiper ivy column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash async aggressive-indent adaptive-wrap ace-window ace-link avy quelpa package-build spacemacs-theme)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((eval progn
           (pp-buffer)
           (indent-buffer))
     (org-tufte-blog-url "http://delta46.us")
     (org-tufte-blog-directory-output "~/prog/blog/output")
     (org-tufte-blog-directory-static "~/prog/blog/static")
     (org-tufte-blog-directory "~/prog/blog")
     (org-ref-bibliography-files "~/prog/blog/references/hammelburg.bib" "~/prog/blog/references/somalia.bib")
     (auto-recompile . t))))
 '(sp-highlight-pair-overlay nil)
 '(spacemacs-theme-comment-bg nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-current-element-highlight t)
 '(web-mode-markup-indent-offset 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "#202224"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
