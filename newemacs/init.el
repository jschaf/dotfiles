;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2017 Joe Schafer
;;
;; Author: Joe Schafer <joe@jschaf.com>
;; URL: https://github.com/jschaf/dotfiles
;;
;; This file is not part of GNU Emacs.

;; Reduces the frequency of garbage collection by making it happen on each 50MB
;; of allocated data.  The default is on every 0.76MB.
(setq gc-cons-threshold (* 50 1000 1000))

(defvar abn-dir (file-name-directory load-file-name)
  "The root dir of the config file.")
(defvar abn-core-dir (expand-file-name "core" abn-dir)
  "The home of core functionality.")
(defvar abn-local-dir (expand-file-name "local" abn-dir)
  "The home of my local functionality.")
(defvar abn-modules-dir (expand-file-name  "modules" abn-dir)
  "This directory houses all of the modules.")

(add-to-list 'load-path abn-core-dir)
(add-to-list 'load-path abn-local-dir)
(add-to-list 'load-path abn-modules-dir)

;; Core
(require 'abn-packages)
(require 'abn-keybindings)
(require 'abn-ui)
(when (eq system-type 'darwin)
  (require 'abn-mac-os))

;; Modules
(require 'abn-evil)
(require 'abn-editing)
(require 'abn-ivy)
(require 'abn-crux)

(setq custom-file (expand-file-name "custom.el" abn-dir))

;; TODO: make the server periodically check if it's running.
(require 'server)
(unless (server-running-p) (server-start))

(message "Emacs ready.")
