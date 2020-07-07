;;; abn-core-constants.el --- Locations of everything

;;; Commentary:
;; 

;;; Code:

(defconst abn-init-time (current-time)
  "When Emacs starting evaling our code.
Similar to `before-init-time'")

(defvar abn-dotfiles-dir "/p/dotfiles"
  "The root dir of all dotfiles.")

(defvar abn-dir "/p/dotfiles/emacs"
  "The root dir of the emacs config.")
(setq user-emacs-directory abn-dir)

(defvar abn-core-dir (expand-file-name "core" abn-dir)
  "The home of core functionality.")
(add-to-list 'load-path abn-core-dir)

(defvar abn-local-dir (expand-file-name "local" abn-dir)
  "The home of my local functionality.")
(add-to-list 'load-path abn-local-dir)

(defvar abn-funcs-dir (expand-file-name "funcs" abn-dir)
  "The home of functions that support modules and core.")
(add-to-list 'load-path abn-funcs-dir)

(defvar abn-modules-dir (expand-file-name "modules" abn-dir)
  "This directory houses all of the modules.")
(add-to-list 'load-path abn-modules-dir)

(defvar abn-work-dir "/p/dotfiles-work")
(add-to-list 'load-path abn-work-dir)

(defvar abn-cache-dir (expand-file-name "~/.emacs.d/.cache")
  "This directory for cache files.")

(provide 'abn-core-constants)
;;; abn-core-constants.el ends here
