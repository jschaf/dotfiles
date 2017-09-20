;;; abn-funcs-projectile.el --- Functions for projectile.

;;; Commentary:
;;

;;; Code:

(defun my:project-files-changed-from-master ()
  "Returns a list of files changed from master in the current project."
  (cl-mapcan
   #'my:get-files-changed-from-master-in-dir
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

(provide 'abn-funcs-projectile)
;;; abn-funcs-projectile.el ends here
