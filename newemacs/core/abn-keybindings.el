;; keybindings.el --- Core key bindings.

(require 'bind-map)
(require 'general)

(defvar abn-leader-map (make-sparse-keymap)
  "Base keymap for all leader key commands.")

(defvar abn-leader-key "SPC"
  "The leader key.")

(defvar abn-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'")

(defvar abn-major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it.")

(defvar abn-major-mode-emacs-leader-key "C-M-m"
  "Major mode leader key accessible in `emacs state' and `insert state'")

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

(defun abn/define-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under
`abn-leader-key' and `abn-emacs-leader-key'.
KEY should be a string suitable for passing to `kbd', and it
should not include the leaders. DEF is most likely a quoted
command. See `define-key' for more information about the possible
choices for DEF. This function simply uses `define-key' to add
the bindings.

For convenience, this function will accept additional KEY DEF
pairs. For example,

\(abn/define-leader-keys
   \"a\" 'command1
   \"C-c\" 'command2
   \"bb\" 'command3\)"
  (while key
    (define-key abn-leader-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))
(put 'abn/define-leader-keys 'lisp-indent-function 'defun)

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

(abn/define-leader-keys "u" 'universal-argument)
(abn/define-leader-keys "!" 'shell-command)

(abn/define-leader-keys
 "ac"  'calc-dispatch
 "ap"  'list-processes
 "aP"  'proced
 "au"  'undo-tree-visualize)

;; File bindings
(abn/define-leader-keys
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

(provide 'abn-keybindings)
