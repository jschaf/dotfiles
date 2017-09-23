;;; abn-funcs-bookmarks.el --- Functions for bookmarks

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(defun abn//find-file-builder (name path)
  "Create function to open PATH."
  `(defun ,name()
     ,(format "Open the file at %s." path)
     (interactive)
     (find-file ,path)))

(defun abn/make-file-shortcuts-in-dir (bindings)
  "Create shortcuts for the (function-name path) tuple in BINDINGS.
BINDINGS is a list of 2-element lists, a symbol for the function
name and a file path."
  (declare (indent 1))
  (cl-loop for (function-name path) in bindings
	   do
	   (let ((file-finder-defun (abn//find-file-builder
				     function-name path)))
	     (eval file-finder-defun))))

(abn/make-file-shortcuts-in-dir
    '(;; Dotfiles
      (abn/bookmark-joe-config "~/.dotfiles/layers/joe/config.el")
      (abn/bookmark-i3-conf "~/.dotfiles/tag-linux/config/i3/config")
      (abn/bookmark-my-org "~/.dotfiles/layers/joe/local/my-org.el")
      (abn/bookmark-joe-packages "~/.dotfiles/layers/joe/packages.el")
      (abn/bookmark-tmux-conf "~/.dotfiles/tmux.conf")
      (abn/bookmark-joe-xinitrc "~/.dotfiles/tag-linux/xsession")

      ;; ZSH
      (abn/bookmark-zshrc "~/.dotfiles/zsh/.zshrc")
      (abn/bookmark-zshrc-keys "~/.dotfiles/zsh/keys.zsh")
      (abn/bookmark-zsh-plugins "~/.dotfiles/zsh/plugins.zsh")
      (abn/bookmark-zshenv "~/.dotfiles/zsh/.zshenv")
      (abn/bookmark-zsh-prompts "~/.dotfiles/zsh/prompts/prompt_pure_setup")
      (abn/bookmark-zsh-host "~/.zsh/host.zsh")
      (abn/bookmark-zsh-work "~/.dotfiles-work/zsh/work.zsh")
      (abn/bookmark-hgrc "~/.dotfiles-work/hgrc")

      ;; Work
      (abn/bookmark-work-emacs "~/.dotfiles-work/emacs/abn-module-work.el")

      ;; Org
      (abn/bookmark-gtd "~/gdrive/org/gtd.org")
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


(provide 'abn-funcs-bookmarks)
;;; abn-funcs-bookmarks.el ends here
