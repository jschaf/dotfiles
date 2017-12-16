;; abn-core-keybindings.el --- Core key bindings.


;;; Commentary:
;;

(require 'bind-map)
(require 'which-key)

;;; Code:

(defvar abn-leader-map (make-sparse-keymap)
  "Base keymap for all leader key commands.")

(defvar abn-leader-key "SPC"
  "The leader key in Evil normal, visual and motion states.")

(defvar abn-emacs-leader-key "M-m"
  "The leader key accessible in the Evil Emacs and insert states.")

(defvar abn-major-mode-leader-key ","
  "Major mode leader key is a shortcut key equivalent to <leader> m.
Set it to `nil` to disable it.")

(defvar abn-major-mode-emacs-leader-key "C-M-m"
  "Major mode leader key accessible in `emacs state' and `insert state'.")

(defvar abn-ex-command-key ":"
  "The key used for Vim Ex commands.")

(defvar abn-command-key "SPC"
  "The key used for Emacs commands (M-x) (after pressing on the leader key).")

(use-package which-key
  :diminish which-key-mode
  :demand
  :init
  (setq which-key-idle-delay 0.5)
  ;; Minibuffer feels much faster than using windows.
  (setq which-key-popup-type 'minibuffer)
  :config
  ;; Shows available keybindings after you start typing.
  (which-key-mode 1))

;; I always hit this by mistake to get to `describe-char' and I'm tired of
;; seeing the GNU license.
(global-set-key (kbd "C-h C-c") 'describe-key-briefly)

(defun abn-declare-prefix (prefix name)
  "Declare a prefix PREFIX.
PREFIX is a string describing a key sequence.  NAME is a string
used as the prefix command."
  (let* ((command name)
	 (full-prefix (concat abn-leader-key " " prefix))
	 (full-prefix-emacs (concat abn-emacs-leader-key " " prefix))
	 (full-prefix-lst (listify-key-sequence (kbd full-prefix)))
	 (full-prefix-emacs-lst (listify-key-sequence
				 (kbd full-prefix-emacs))))
    (which-key-declare-prefixes
      full-prefix-emacs name
      full-prefix name)))
(put 'abn-declare-prefix 'lisp-indent-function 'defun)

(defun abn/declare-prefix-for-mode (mode prefix name)
  "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
be added. PREFIX is a string describing a key sequence. NAME is a symbol name
used as the prefix command."
  (let  ((command (intern (concat (symbol-name mode) name)))
	 (full-prefix (concat abn-leader-key " " prefix))
	 (full-prefix-emacs (concat abn-emacs-leader-key " " prefix))
	 (is-major-mode-prefix (string-prefix-p "m" prefix))
	 (major-mode-prefix (concat abn-major-mode-leader-key
				    " " (substring prefix 1)))
	 (major-mode-prefix-emacs
	  (concat abn-major-mode-emacs-leader-key
		  " " (substring prefix 1))))
    (which-key-declare-prefixes-for-mode mode
      full-prefix-emacs name
      full-prefix name)
    (when (and is-major-mode-prefix abn-major-mode-leader-key)
      (which-key-declare-prefixes-for-mode mode major-mode-prefix name))
    (when (and is-major-mode-prefix abn-major-mode-emacs-leader-key)
      (which-key-declare-prefixes-for-mode
	mode major-mode-prefix-emacs name))))
(put 'abn/declare-prefix-for-mode 'lisp-indent-function 'defun)

(defun abn//init-leader-mode-map (mode)
  "Returns a keymap for MODE activated by the major mode bindings.
The returned keymap is active when the current `major-mode' equals
MODE.  The keys `abn-emacs-leader-key' + m and
`abn-major-mode-emacs-leader-key' activate the returned keymap."
  (let* ((mode-map-sym (intern (format "abn-%s-map" mode)))
         (root-map-active-var (intern (format "abn-%s-active-p" mode)))

         (root-map-sym (intern (format "abn-%s-root-map" mode)))
         root-map-val)

    ;; CAUTION: really tricky quoting between symbol and symbol
    ;; values.  Pay attention to set versus setq.

    ;; Use existing keymap if it exists.
    (unless (boundp mode-map-sym)
      (set mode-map-sym (make-sparse-keymap)))
    (setq mode-map-val (symbol-value mode-map-sym))

    ;; If the root map isn't set, we didn't setup the keymaps.
    (unless (boundp root-map-sym)

      ;; The variable to control whether the root map is active.
      (set root-map-active-var nil)

      ;; Bind the symbol, abn-<mode>-root-map to a keymap.
      (set  root-map-sym (make-sparse-keymap))
      (setq root-map-val (symbol-value root-map-sym))

      ;; Use minor-mode-map-alist as a hack for conditional activation
      ;; of root map.  The root-map-sym is active when
      ;; root-map-active-var is non-nil
      (add-to-list 'minor-mode-map-alist
                   (cons root-map-active-var root-map-val))

      ;; The list to monitor whenever the major-mode changes to active
      ;; the root map.
      (add-to-list 'abn--major-mode-maps-to-activate
                   (cons root-map-active-var mode))

      ;; Define keys on the root map to activate the prefix map.
      (define-key root-map-val
        (kbd (concat abn-emacs-leader-key " m")) mode-map-val)
      (define-key root-map-val
        (kbd abn-major-mode-emacs-leader-key) mode-map-val)

      ;; Define keys to activate this map in evil states.
      ;; Using `eval-after-load' to selectively unquote values.
      (eval-after-load 'evil
        `(progn
           (dolist (evil-state '(normal motion visual evilified))
             (evil-define-key '(normal motion visual evilified)
               abn-leader-map "m" ,mode-map-sym)
             ;; (define-key
             ;;   (evil-get-auxiliary-keymap ,root-map-sym evil-state t)
             ;;   (kbd (concat abn-leader-key " m"))
             ;;   ,mode-map-sym)
             )
           (evil-normalize-keymaps)
           (abn//change-major-mode-after-body-hook))))

    ;; Call in case we're already in the major mode to activate.
    (abn//change-major-mode-after-body-hook)

    mode-map-val))

(defvar abn--major-mode-maps-to-activate '())

(defun abn//change-major-mode-after-body-hook ()
  "Update the active variables for the root keymaps for major modes.
The active variables are used by `minor-mode-map-alist' to
determine which keymaps are active."
  (cl-loop for (active-var . controlled-major-mode)
           in abn--major-mode-maps-to-activate
           do
           (set active-var (eq major-mode controlled-major-mode))))

(add-hook 'change-major-mode-after-body-hook
          'abn//change-major-mode-after-body-hook)

(add-hook 'emacs-startup-hook
          'abn//change-major-mode-after-body-hook)


(defun abn//define-keys (keymap key def &rest bindings)
  "In KEYMAP define KEY to DEF and all BINDINGS.
`kbd' is applied to all KEYs.  BINDINGS is additional KEY-DEF pairs.
Always defines C-g as `keyboard-quit'."
  (declare (indent 1))
  (define-key keymap (kbd "C-g") 'keyboard-quit)
  (while key
    (define-key keymap (kbd key) def)
    (setq key (pop bindings))
    (setq def (pop bindings))))

(defun abn/define-leader-keys (key def &rest bindings)
  "Set KEY to DEF in `abn-leader-map'.
BINDINGS is additional key-definition pairs.  `kbd' is used for
every key."
  (declare (indent 0))
  (apply 'abn//define-keys abn-leader-map key def bindings))

(defun abn/define-leader-keys-for-major-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`abn-major-mode-leader-key' and
`abn-major-mode-emacs-leader-key' for the major-mode
MODE. MODE should be a quoted symbol corresponding to a valid
major mode. The rest of the arguments are treated exactly like
they are in `abn/define-leader-keys'."
  (declare (indent defun))
  (apply 'abn//define-keys (abn//init-leader-mode-map mode) key def bindings))

(abn/define-leader-keys-for-major-mode 'emacs-lisp-mode
  "z" 'previous-line)


;; Instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)

;; Auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Makes <escape> quit as much as possible.
(define-key minibuffer-local-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map
  (kbd "<escape>") 'keyboard-escape-quit)
(setq abn-key-binding-prefixes
      '((","   "leader")
	("a"   "applications")
	("ai"  "irc")
	("as"  "shells")
	("b"   "buffers")
	("c"   "compile/comments")
	("C"   "capture/colors")
	("e"   "errors")
	("f"   "files")
	("fC"  "files/convert")
	("fe"  "emacs")
	("fv"  "variables")
	("g"   "git/versions-control")
	("h"   "help")
	("hd"  "help-describe")
	("i"   "insertion")
	("j"   "jump/join/split")
	("k"   "lisp")
	("kd"  "delete")
	("kD"  "delete-backward")
	("k`"  "hybrid")
	("n"   "narrow/numbers")
	("p"   "projects")
	("p$"  "projects/shell")
	("q"   "quit")
	("r"   "registers/rings/resume")
	("Re"  "elisp")
	("Rp"  "pcre")
	("s"   "search/symbol")
	("sa"  "ag")
	("sg"  "grep")
	("sk"  "ack")
	("st"  "pt")
	("sw"  "web")
	("t"   "toggles")
	("tC"  "colors")
	("tE"  "editing-styles")
	("th"  "highlight")
	("tm"  "modeline")
	("T"   "UI toggles/themes")
	("C-t" "other toggles")
	("w"   "windows")
	("wp"  "popup")
	("x"   "text")
	("xa"  "align")
	("xd"  "delete")
	("xg"  "google-translate")
	("xl"  "lines")
	("xm"  "move")
	("xt"  "transpose")
	("xw"  "words")
	("z"   "zoom")))
(mapc (lambda (x) (apply #'abn-declare-prefix x))
      abn-key-binding-prefixes)

;; General purpose leader keys
(abn//define-keys abn-leader-map
                  "u" 'universal-argument
                  "!" 'shell-command)

;; Application leader keys
(abn/define-leader-keys
  "ac" 'calc-dispatch
  "ap" 'list-processes
  "aP" 'proced
  "au" 'undo-tree-visualize)

;; Application leader keys
(abn/define-leader-keys
  "ac" 'calc-dispatch
  "ap" 'list-processes
  "aP" 'proced
  "au" 'undo-tree-visualize)

;; Buffers
(use-package abn-funcs-buffer
  :ensure nil ; local package
  :bind
  (:map abn-leader-map
   ("TAB" . abn/alternate-buffer)
   ("bd" . abn/kill-this-buffer)
   ("be" . abn/safe-erase-buffer)
   ("bn" . next-buffer)
   ("bm" . abn/kill-other-buffers)
   ("bN" . abn/new-empty-buffer)
   ("bP" . abn/copy-clipboard-to-whole-buffer)
   ("bp" . previous-buffer)
   ("bR" . abn/safe-revert-buffer)
   ("bs" . abn/switch-to-scratch-buffer)
   ("bY" . abn/copy-whole-buffer-to-clipboard)
   ("bw" . read-only-mode)
   ("b1" . buffer-to-window-1)
   ("b2" . buffer-to-window-2)
   ("b3" . buffer-to-window-3)
   ("b4" . buffer-to-window-4)
   ("b5" . buffer-to-window-5)
   ("b6" . buffer-to-window-6)
   ("b7" . buffer-to-window-7)
   ("b8" . buffer-to-window-8)
   ("b9" . buffer-to-window-9)))

;; Errors
(use-package abn-funcs-error
  :ensure nil ; local package
  :bind
  (:map abn-leader-map
   ("en" . abn/next-error)
   ("ep" . abn/previous-error)))

;; Files
(use-package abn-funcs-file
  :ensure nil ; local package
  :bind
  (:map abn-leader-map
   ("fc" . abn/copy-file)
   ("fD" . abn/delete-current-buffer-file)
   ("fei" . abn/find-user-init-file)
   ("fed" . abn/find-user-init-file)
   ("feD" . abn/ediff-dotfile-and-template)
   ("fev" . abn/display-and-copy-emacs-version)
   ("fCd" . abn/unix2dos)
   ("fCu" . abn/dos2unix)
   ("fG" . rgrep)
   ("fl" . find-file-literally)
   ("fE" . abn/sudo-edit)
   ("fo" . abn/open-file-or-directory-in-external-app)
   ("fR" . abn/rename-current-buffer-file)
   ("fS" . evil-write-all)
   ("fs" . save-buffer)
   ("fvd" . add-dir-local-variable)
   ("fvf" . add-file-local-variable)
   ("fvp" . add-file-local-variable-prop-line)
   ("fy" . abn/show-and-copy-buffer-filename)))

;; Help
(use-package abn-funcs-help
  :ensure nil ; local package
  :bind
  (:map abn-leader-map
   ("hdb" . describe-bindings)
   ("hdc" . describe-char)
   ("hdf" . describe-function)
   ("hdk" . describe-key)
   ("hdl" . abn/describe-last-keys)
   ("hdp" . describe-package)
   ("hds" . abn/describe-system-info)
   ("hdt" . describe-theme)
   ("hdv" . describe-variable)
   ("hN"  . view-emacs-news)))

;; Navigation and Jumping
(use-package abn-funcs-navigation
  :ensure nil ; local package
  :bind
  (:map abn-leader-map
   ("j0" . abn/push-mark-and-goto-beginning-of-line)
   ("j$" . abn/push-mark-and-goto-end-of-line)
   ("jf" . find-function)
   ("jv" . find-variable)))

;; Compilation
(use-package abn-funcs-compilation
  :ensure nil ; local package
  :bind
  (:map abn-leader-map
   ("cC" . compile)
   ("ck" . kill-compilation)
   ("cr" . recompile)
   ("cd" . abn/close-compilation-window)))

;; (with-eval-after-load 'compile
;;   (evil-define-key 'motion compilation-mode-map (kbd "gf") 'find-file-at-point)
;;   (define-key compilation-mode-map "r" 'recompile)
;;   (define-key compilation-mode-map "g" nil))

;; Narrow and widen
(abn/define-leader-keys
 "nr" 'narrow-to-region
 "np" 'narrow-to-page
 "nf" 'narrow-to-defun
 "nw" 'widen)

;; Windows

(use-package abn-funcs-window
  :ensure nil ; local package
  :bind
  (:map abn-leader-map
   ("w TAB"  . abn/alternate-window)
   ("w2"  . abn/layout-double-columns)
   ("w3"  . abn/layout-triple-columns)
   ("wb"  . abn/switch-to-minibuffer-window)
   ("wd"  . abn/delete-window)
   ("wt"  . abn/toggle-current-window-dedication)
   ("wf"  . follow-mode)
   ("wF"  . make-frame)
   ("wH"  . evil-window-move-far-left)
   ("wh"  . evil-window-left)
   ("wJ"  . evil-window-move-very-bottom)
   ("wj"  . evil-window-down)
   ("wK"  . evil-window-move-very-top)
   ("wk"  . evil-window-up)
   ("wL"  . evil-window-move-far-right)
   ("wl"  . evil-window-right)
   ("wm"  . abn/toggle-maximize-buffer)
   ("wo"  . other-frame)
   ("wr"  . abn/rotate-windows-forward)
   ("wR"  . abn/rotate-windows-backward)
   ("ws"  . split-window-below)
   ("wS"  . split-window-below-and-focus)
   ("w-"  . split-window-below)
   ("wU"  . winner-redo)
   ("wu"  . winner-undo)
   ("wv"  . split-window-right)
   ("wV"  . split-window-right-and-focus)
   ("ww"  . other-window)
   ("w/"  . split-window-right)
   ("w="  . balance-windows)
   ("w+"  . abn/window-layout-toggle)
   ("w_"  . abn/maximize-horizontally)))

;; Alignment
(use-package abn-funcs-align
  :ensure nil ; local package
  :bind
  (:map abn-leader-map
   ("xa&" . abn/align-repeat-ampersand)
   ("xa(" . abn/align-repeat-left-paren)
   ("xa)" . abn/align-repeat-right-paren)
   ("xa," . abn/align-repeat-comma)
   ("xa." . abn/align-repeat-decimal)
   ("xa:" . abn/align-repeat-colon)
   ("xa;" . abn/align-repeat-semicolon)
   ("xa=" . abn/align-repeat-equal)
   ("xa\\" . abn/align-repeat-backslash)
   ("xaa" . align)
   ("xac" . align-current)
   ("xam" . abn/align-repeat-math-oper)
   ("xar" . abn/align-repeat)
   ("xa|" . abn/align-repeat-bar)
   ("xc"  . count-region)
   ("xdw" . delete-trailing-whitespace)
   ("xjc" . set-justification-center)
   ("xjf" . set-justification-full)
   ("xjl" . set-justification-left)
   ("xjn" . set-justification-none)
   ("xjr" . set-justification-right)
   ("xlc" . abn/sort-lines-by-column)
   ("xlC" . abn/sort-lines-by-column-reverse)
   ("xld" . abn/duplicate-line-or-region)
   ("xls" . abn/sort-lines)
   ("xlS" . abn/sort-lines-reverse)
   ("xlu" . abn/uniquify-lines)
   ("xtc" . transpose-chars)
   ("xtl" . transpose-lines)
   ("xtw" . transpose-words)
   ("xU"  . upcase-region)
   ("xu"  . downcase-region)
   ("xwc" . abn/count-words-analysis)
   ("x TAB" . indent-rigidly)))

(provide 'abn-core-keybindings)
;;; abn-core-keybindings.el ends here
