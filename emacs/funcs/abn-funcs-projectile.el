;;; abn-funcs-projectile.el --- Functions for projectile.

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(defun abn/make-projectile-shortcuts (bindings)
  "Create shortcuts for the (function-name path) tuple in BINDINGS.
BINDINGS is a list of 2-element lists, a symbol for the function
name and a file path."
  (declare (indent 1))
  (cl-loop for (function-name path) in bindings
	   do
	   (let ((shortcut-defun (abn//projectile-bookmark-builder
                                  function-name path)))
	     (eval shortcut-defun))))

(defun abn//projectile-bookmark-builder (name path)
  `(defun ,name (&optional arg)
     ,(format"Open the project at %s.
Invokes the command referenced by
`projectile-switch-project-action' on switch.  With a prefix ARG
invokes `projectile-commander' instead of
`projectile-switch-project-action.'" path)
     (interactive "P")
     (if (file-exists-p ,path)
         (projectile-switch-project-by-name ,path arg)
       (error "Can't switch to non-existent project: %s" ,path))))


(abn/make-projectile-shortcuts
    '((abn/projectile-dotfiles abn-dotfiles-dir)
      (abn/projectile-dotfiles-emacs (expand-file-name abn-dotfiles-dir "emacs.d/"))
      (abn/projectile-esup "~/prog/esup")))

(defun abn/project-files-changed-from-master ()
  "Returns a list of files changed from master in the current project."
  (cl-mapcan
   #'abn/get-files-changed-from-master-in-dir
   (projectile-get-project-directories)))

(defun abn/get-files-changed-from-git-command (directory command)
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

(defun abn/get-files-changed-from-master-in-dir (directory)
  "Returns list of files changed from master branch in DIRECTORY."
  (abn/get-files-changed-from-git-command
   directory "git diff -z --name-only master"))

(defun abn/get-files-changed-from-git5-sync-in-dir (directory)
  "Returns list of files changed from master branch in DIRECTORY."
  (let ((git5-sync-hash
	 (shell-command-to-string
	  (concat "git log --max-count=1 --format='%H' "
		  "--grep 'git5:'"))))
    (abn/get-files-changed-from-git-command
     directory
     (format "git diff -z --name-only %s" git5-sync-hash))))




(provide 'abn-funcs-projectile)
;;; abn-funcs-projectile.el ends here
