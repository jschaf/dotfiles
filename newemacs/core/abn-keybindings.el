;; keybindings.el --- Core key bindings.


;;; Commentary:
;;

(require 'bind-map)
(require 'general)
(require 'which-key)

;;; Code:

(defvar abn-leader-map (make-sparse-keymap)
  "Base keymap for all leader key commands.")

(defvar abn-leader-key "SPC"
  "The leader key.")

(defvar abn-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'.")

(defvar abn-major-mode-leader-key ","
  "Major mode leader key is a shortcut key equivalent to <leader> m.
Set it to `nil` to disable it.")

(defvar abn-major-mode-emacs-leader-key "C-M-m"
  "Major mode leader key accessible in `emacs state' and `insert state'.")

(defvar abn-ex-command-key ":"
  "The key used for Vim Ex commands.")

(defvar abn-command-key "SPC"
  "The key used for Emacs commands (M-x) (after pressing on the leader key).")

(bind-map abn-leader-map
  :prefix-cmd spacemacs-cmds
  :keys (abn-emacs-leader-key)
  :evil-keys (abn-leader-key)
  :override-minor-modes t
  :override-mode-name spacemacs-leader-override-mode)

(defun abn-declare-prefix (prefix name &optional long-name)
  "Declare a prefix PREFIX.
PREFIX is a string describing a key sequence.  NAME is a string
used as the prefix command.  LONG-NAME if given is stored in
`spacemacs/prefix-titles'."
  (let* ((command name)
         (full-prefix (concat abn-leader-key " " prefix))
         (full-prefix-emacs (concat abn-emacs-leader-key " " prefix))
         (full-prefix-lst (listify-key-sequence (kbd full-prefix)))
         (full-prefix-emacs-lst (listify-key-sequence
                                 (kbd full-prefix-emacs))))
    ;; define the prefix command only if it does not already exist
    (unless long-name (setq long-name name))
    (which-key-declare-prefixes
      full-prefix-emacs (cons name long-name)
      full-prefix (cons name long-name))))
(put 'abn-declare-prefix 'lisp-indent-function 'defun)

(defun abn-define-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key BINDINGS under leader keys.
Adds to both `abn-leader-key' and `abn-emacs-leader-key'.  KEY
should be a string suitable for passing to `kbd', and it should
not include the leaders.  DEF is most likely a quoted command.
See `define-key' for more information about the possible choices
for DEF.  This function simply uses `define-key' to add the
bindings.

For convenience, this function will accept additional KEY DEF
pairs.  For example,

\(abn-define-leader-keys
   \"a\" 'command1
   \"jk\" 'command2
   \"bb\" 'command3\)"
  (while key
    (define-key abn-leader-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))
(put 'abn-define-leader-keys 'lisp-indent-function 'defun)

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
      '(("a"   "applications")
        ("ai"  "irc")
        ("as"  "shells")
        ("b"   "buffers")
        ("c"   "compile/comments")
        ("C"   "capture/colors")
        ("e"   "errors")
        ("f"   "files")
        ("fC"  "files/convert")
        ("fe"  "emacs(spacemacs)")
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

(abn-define-leader-keys "u" 'universal-argument)
(abn-define-leader-keys "!" 'shell-command)

;; Application leader keys
(abn-define-leader-keys
 "ac"  'calc-dispatch
 "ap"  'list-processes
 "aP"  'proced
 "au"  'undo-tree-visualize)

;; Buffers
(abn-define-leader-keys
 "TAB"   'spacemacs/alternate-buffer
 "bd"    'spacemacs/kill-this-buffer
 "be"    'spacemacs/safe-erase-buffer
 "bh"    'spacemacs/home
 "b C-d" 'spacemacs/kill-matching-buffers-rudely
 "bn"    'next-buffer
 "bm"    'spacemacs/kill-other-buffers
 "bN"    'spacemacs/new-empty-buffer
 "bP"    'spacemacs/copy-clipboard-to-whole-buffer
 "bp"    'previous-buffer
 "bR"    'spacemacs/safe-revert-buffer
 "bs"    'spacemacs/switch-to-scratch-buffer
 "bY"    'spacemacs/copy-whole-buffer-to-clipboard
 "bw"    'read-only-mode
 "b1"    'buffer-to-window-1
 "b2"    'buffer-to-window-2
 "b3"    'buffer-to-window-3
 "b4"    'buffer-to-window-4
 "b5"    'buffer-to-window-5
 "b6"    'buffer-to-window-6
 "b7"    'buffer-to-window-7
 "b8"    'buffer-to-window-8
 "b9"    'buffer-to-window-9
 )

;; Errors
(abn-define-leader-keys
 "en" 'spacemacs/next-error
 "eN" 'spacemacs/previous-error
 "ep" 'spacemacs/previous-error)

;; Files
(abn-define-leader-keys
 "fc" 'spacemacs/copy-file
 "fD" 'spacemacs/delete-current-buffer-file
 "fei" 'spacemacs/find-user-init-file
 "fed" 'spacemacs/find-dotfile
 "feD" 'spacemacs/ediff-dotfile-and-template
 "feR" 'dotspacemacs/sync-configuration-layers
 "fev" 'spacemacs/display-and-copy-version
 "fCd" 'spacemacs/unix2dos
 "fCu" 'spacemacs/dos2unix
 "fg" 'rgrep
 "fl" 'find-file-literally
 "fE" 'spacemacs/sudo-edit
 "fo" 'spacemacs/open-file-or-directory-in-external-app
 "fR" 'spacemacs/rename-current-buffer-file
 "fS" 'evil-write-all
 "fs" 'save-buffer
 "fvd" 'add-dir-local-variable
 "fvf" 'add-file-local-variable
 "fvp" 'add-file-local-variable-prop-line
 "fy" 'spacemacs/show-and-copy-buffer-filename)

;; Help
(abn-define-leader-keys
 "hdb" 'describe-bindings
 "hdc" 'describe-char
 "hdf" 'describe-function
 "hdk" 'describe-key
 "hdl" 'spacemacs/describe-last-keys
 "hdp" 'describe-package
 "hdP" 'configuration-layer/describe-package
 "hds" 'spacemacs/describe-system-info
 "hdt" 'describe-theme
 "hdv" 'describe-variable
 "hI"  'spacemacs/report-issue
 "hn"  'view-emacs-news)

;; Insertions
(abn-define-leader-keys
 "iJ" 'spacemacs/insert-line-below-no-indent
 "iK" 'spacemacs/insert-line-above-no-indent
 "ik" 'spacemacs/evil-insert-line-above
 "ij" 'spacemacs/evil-insert-line-below)

;; Formatting
(abn-define-leader-keys
 "jo" 'open-line
 "j=" 'spacemacs/indent-region-or-buffer
 "jS" 'spacemacs/split-and-new-line
 "jk" 'spacemacs/evil-goto-next-line-and-indent)

;; Navigation and Jumping
(abn-define-leader-keys
 "j0" 'spacemacs/push-mark-and-goto-beginning-of-line
 "j$" 'spacemacs/push-mark-and-goto-end-of-line
 "jf" 'find-function
 "jv" 'find-variable)

;; Compilation
(abn-define-leader-keys
 "cC" 'compile
 "ck" 'kill-compilation
 "cr" 'recompile
 "cd" 'spacemacs/close-compilation-window)
(with-eval-after-load 'compile
  (evil-define-key 'motion compilation-mode-map (kbd "gf") 'find-file-at-point)
  (define-key compilation-mode-map "r" 'recompile)
  (define-key compilation-mode-map "g" nil))

;; Narrow and widen
(abn-define-leader-keys
 "nr" 'narrow-to-region
 "np" 'narrow-to-page
 "nf" 'narrow-to-defun
 "nw" 'widen)

;; Windows
(abn-define-leader-keys
 "w TAB"  'spacemacs/alternate-window
 "w2"  'spacemacs/layout-double-columns
 "w3"  'spacemacs/layout-triple-columns
 "wb"  'spacemacs/switch-to-minibuffer-window
 "wd"  'spacemacs/delete-window
 "wt"  'spacemacs/toggle-current-window-dedication
 "wf"  'follow-mode
 "wF"  'make-frame
 "wH"  'evil-window-move-far-left
 "wh"  'evil-window-left
 "wJ"  'evil-window-move-very-bottom
 "wj"  'evil-window-down
 "wK"  'evil-window-move-very-top
 "wk"  'evil-window-up
 "wL"  'evil-window-move-far-right
 "wl"  'evil-window-right
 "wm"  'spacemacs/toggle-maximize-buffer
 "wc"  'spacemacs/toggle-centered-buffer-mode
 "wC"  'spacemacs/centered-buffer-mode-full-width
 "wo"  'other-frame
 "wr"  'spacemacs/rotate-windows-forward
 "wR"  'spacemacs/rotate-windows-backward
 "ws"  'split-window-below
 "wS"  'split-window-below-and-focus
 "w-"  'split-window-below
 "wU"  'winner-redo
 "wu"  'winner-undo
 "wv"  'split-window-right
 "wV"  'split-window-right-and-focus
 "ww"  'other-window
 "w/"  'split-window-right
 "w="  'balance-windows
 "w+"  'spacemacs/window-layout-toggle
 "w_"  'spacemacs/maximize-horizontally)

;; Alignment
(abn-define-leader-keys
 "xa&" 'spacemacs/align-repeat-ampersand
 "xa(" 'spacemacs/align-repeat-left-paren
 "xa)" 'spacemacs/align-repeat-right-paren
 "xa," 'spacemacs/align-repeat-comma
 "xa." 'spacemacs/align-repeat-decimal
 "xa:" 'spacemacs/align-repeat-colon
 "xa;" 'spacemacs/align-repeat-semicolon
 "xa=" 'spacemacs/align-repeat-equal
 "xa\\" 'spacemacs/align-repeat-backslash
 "xaa" 'align
 "xac" 'align-current
 "xam" 'spacemacs/align-repeat-math-oper
 "xar" 'spacemacs/align-repeat
 "xa|" 'spacemacs/align-repeat-bar
 "xc"  'count-region
 "xdw" 'delete-trailing-whitespace
 "xjc" 'set-justification-center
 "xjf" 'set-justification-full
 "xjl" 'set-justification-left
 "xjn" 'set-justification-none
 "xjr" 'set-justification-right
 "xlc" 'spacemacs/sort-lines-by-column
 "xlC" 'spacemacs/sort-lines-by-column-reverse
 "xld" 'spacemacs/duplicate-line-or-region
 "xls" 'spacemacs/sort-lines
 "xlS" 'spacemacs/sort-lines-reverse
 "xlu" 'spacemacs/uniquify-lines
 "xtc" 'transpose-chars
 "xtl" 'transpose-lines
 "xtw" 'transpose-words
 "xU"  'upcase-region
 "xu"  'downcase-region
 "xwc" 'spacemacs/count-words-analysis
 "x TAB" 'indent-rigidly)

(provide 'abn-keybindings)
;;; abn-keybindings.el ends here
