;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2017 Joe Schafer
;;
;; Author: Joe Schafer <joe@jschaf.com>
;; URL: https://github.com/jschaf/dotfiles
;;
;; This file is not part of GNU Emacs.

;; (package-initialize)

;; Reduces the frequency of garbage collection by making it happen on each 50MB
;; of allocated data.  The default is on every 0.76MB.
(setq gc-cons-threshold (* 50 1000 1000))

(defvar abn-dir (file-name-directory load-file-name)
  "The root dir of the config file.")
(defvar abn-core-dir (expand-file-name "core" abn-dir)
  "The home of core functionality.")
(defvar abn-local-dir (expand-file-name "local" abn-dir)
  "The home of my local functionality.")
(defvar abn-funcs-dir (expand-file-name "funcs" abn-dir)
  "The home of functions that support modules and core.")
(defvar abn-modules-dir (expand-file-name  "modules" abn-dir)
  "This directory houses all of the modules.")
(defvar abn-cache-dir (expand-file-name "~/.config/emacs")
  "This directory houses all of the modules.")

(add-to-list 'load-path abn-core-dir)
(add-to-list 'load-path abn-local-dir)
(add-to-list 'load-path abn-funcs-dir)
(add-to-list 'load-path abn-modules-dir)

;; Core
(require 'abn-core-packages)
(require 'abn-core-keybindings)
(require 'abn-core-ui)
(when (eq system-type 'darwin)
  (require 'abn-core-mac-os))

;; Modules
(require 'abn-module-avy)
(require 'abn-module-base)
(require 'abn-module-crux)
(require 'abn-module-dired)
(require 'abn-module-editing)
(require 'abn-module-emacs-config)
(require 'abn-module-emacs-lisp)
(require 'abn-module-evil)
(require 'abn-module-git)
(require 'abn-module-ivy)
(require 'abn-module-projectile)

(setq custom-file (expand-file-name "custom.el" abn-dir))

;; TODO: make the server periodically check if it's running.
(require 'server)
(unless (server-running-p) (server-start))

(message "Emacs ready.")
