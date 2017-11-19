;;; start.el --- Emacs Initialization File
;;
;; Copyright (c) 2017 Joe Schafer
;;
;; Author: Joe Schafer <joe@jschaf.com>
;; URL: https://github.com/jschaf/dotfiles
;;
;; This file is not part of GNU Emacs.

;; Prevent Emacs from calling package-initialize.  We'll do it ourselves.
;; (package-initialize)

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 0.76MB.
(setq gc-cons-threshold (* 50 1000 1000))

(defvar abn-dir (expand-file-name "~/.dotfiles/emacs")
  "The root dir of the config file.")

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

(defvar abn-work-dir (expand-file-name "~/.dotfiles-work/emacs"))
(add-to-list 'load-path abn-work-dir)

(defvar abn-cache-dir (expand-file-name "~/.emacs.d/.cache")
  "This directory for cache files.")

;; Core
(require 'abn-core-packages)
(require 'abn-core-emacs-settings)
(require 'abn-core-keybindings)
(require 'abn-core-ui)
(when (eq system-type 'darwin)
  (require 'abn-core-mac-os))

;; Modules
(require 'abn-module-autocomplete)
(require 'abn-module-avy)
(require 'abn-module-base)
(require 'abn-module-benchmark)
(require 'abn-module-bookmarks)
(require 'abn-module-coding)
(require 'abn-module-dired)
(require 'abn-module-editing)
(require 'abn-module-emacs-config)
(require 'abn-module-emacs-lisp)
(require 'abn-module-evil)
(require 'abn-module-git)
(require 'abn-module-highlight)
(require 'abn-module-ivy)
(require 'abn-module-java)
(require 'abn-module-javascript)
(require 'abn-module-magit)
(require 'abn-module-markdown)
(require 'abn-module-mode-line)
(require 'abn-module-org)
(require 'abn-module-projectile)
(require 'abn-module-python)
(require 'abn-module-shell-script)
(require 'abn-module-smartparens)

;; Ignore errors if work file isn't found.
(require 'work-init nil 'noerror)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; Suppress message "For information about GNU Emacs..."
(setq inhibit-startup-echo-area-message "jschaf")

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
