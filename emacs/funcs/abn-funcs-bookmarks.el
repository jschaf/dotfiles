;;; abn-funcs-bookmarks.el --- Functions for bookmarks

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(defun abn//find-file-builder (name path)
  "Open the file at PATH."
  `(defun ,name()
     ,(format "Open the file at %s." path)
     (interactive)
     (find-file ,path)))

(defun abn/make-file-shortcuts (bindings)
  "Create shortcuts for the (function-name path) tuple in BINDINGS.
BINDINGS is a list of 2-element lists, a symbol for the function
name and a file path."
  (declare (indent 1))
  (cl-loop for (function-name path) in bindings
	   do
	   (let ((file-finder-defun (abn//find-file-builder
				     function-name path)))
	     (eval file-finder-defun))))

(abn/make-file-shortcuts
    '(;; Dotfiles
      (abn/bookmark-joe-config "~/.dotfiles/layers/joe/config.el")
      (abn/bookmark-i3-conf "~/.dotfiles/tag-linux/config/i3/config")
      (abn/bookmark-my-org "~/.dotfiles/layers/joe/local/my-org.el")
      (abn/bookmark-joe-packages "~/.dotfiles/layers/joe/packages.el")
      (abn/bookmark-tmux-conf "~/.dotfiles/tmux.conf")
      (abn/bookmark-joe-xinitrc "~/.dotfiles/tag-linux/xsession")

      ;; Else
      (abn/bookmark-else-elisp "~/.dotfiles/emacs/else-templates/Emacs-Lisp.lse")
      (abn/bookmark-else-shell "~/.dotfiles/emacs/else-templates/Shell.lse")
      (abn/bookmark-else-template "~/.dotfiles/emacs/else-templates/Template-cust.lse")

      ;; Emacs
      (abn/bookmark-emacs-autocomplete-funcs "~/.dotfiles/emacs/funcs/abn-funcs-autocomplete.el")
      (abn/bookmark-emacs-autocomplete-module "~/.dotfiles/emacs/modules/abn-module-autocomplete.el")

      (abn/bookmark-emacs-bookmarks-funcs "~/.dotfiles/emacs/funcs/abn-funcs-bookmarks.el")
      (abn/bookmark-emacs-bookmarks-module "~/.dotfiles/emacs/modules/abn-module-bookmarks.el")

      ;; ZSH
      (abn/bookmark-zsh-aliases "~/.dotfiles/zsh/aliases.zsh")
      (abn/bookmark-zshrc "~/.dotfiles/zsh/.zshrc")
      (abn/bookmark-zshrc-keys "~/.dotfiles/zsh/keys.zsh")
      (abn/bookmark-zsh-plugins "~/.dotfiles/zsh/plugins.zsh")
      (abn/bookmark-zshenv "~/.dotfiles/zsh/.zshenv")
      (abn/bookmark-zsh-profile "~/.dotfiles/zsh/.zprofile")
      (abn/bookmark-zsh-host "~/.zsh/host.zsh")

      ;; Other repos
      (abn/bookmark-spacemacs "~/prog/spacemacs/init.el")

      ;; Org
      (abn/bookmark-gtd "~/gdrive/org/gtd.org")
      (abn/bookmark-crypto "~/prog/crypto/docs/todo.org")
      (abn/bookmark-goog "~/gdrive/gorg/goog.org")
      (abn/bookmark-journal "~/gdrive/org/journal.org")
      (abn/bookmark-sandlot "~/gdrive/gorg/sandlot.org")
      (abn/bookmark-refile "~/gdrive/org/refile.org")
      (abn/bookmark-refile "~/gdrive/gorg/today.org")
      (abn/bookmark-people "~/gdrive/org/people.org")
      (abn/bookmark-ledger "~/gdrive/financials/personal.ledger")
      (abn/bookmark-joe-checklist "~/gdrive/org/checklists/checklist.org")
      (abn/bookmark-refile-work "~/gdrive/org/work-refile.org")
      (abn/bookmark-org-drill "~/gdrive/drill/programming.org")))

(defun abn/bookmark-switch-between-func-and-module ()
  "Switch between the abn-func-* and abn-module-* of current buffer."
  (interactive)
  (if (not buffer-file-name)
      (error "Buffer has no file name."))
  (if (not (string-match-p "\\(funcs\\|module\\)"
                           (file-name-nondirectory (buffer-file-name))))
      (error "File doesn't have funcs or module in name."))
  (let ((new-file (abn//bookmark-module-funcs-other-file-name)))
    (find-file new-file)))

(defun abn//bookmark-module-funcs-other-file-name ()
  "Get the corresponding abn-funcs or abn-module name from current buffer."
  (let* ((current-file (buffer-file-name))
         (parent-dir (file-name-directory current-file))
         (grandparent-dir (file-name-directory (directory-file-name parent-dir)))
         (base-name (file-name-nondirectory current-file)))
    (string-match "\\(abn-\\(work-\\)?\\)\\(funcs\\|module\\)\\(.*\\)"
                  base-name)
    (let* ((name-prefix (match-string 1 base-name))
           (current-type (match-string 3 base-name))
           (new-type (if (string-equal current-type "funcs")
                         "module"
                       "funcs"))
           (new-parent-dir (if (string-equal new-type "funcs") "funcs" "modules"))
           (name-suffix (match-string 4 base-name))
           (new-name (concat name-prefix new-type name-suffix)))
      (concat grandparent-dir new-parent-dir
              "/" name-prefix new-type name-suffix))))


(provide 'abn-funcs-bookmarks)
;;; abn-funcs-bookmarks.el ends here
