;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2017 Joe Schafer
;;
;; Author: Joe Schafer <joe@jschaf.com>
;; URL: https://github.com/jschaf/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Reduces the frequency of garbage collection by making it happen on each 50MB
;; of allocated data (the default is on every 0.76MB).
(setq gc-cons-threshold 50000000)

(defvar abn-dir (file-name-directory load-file-name)
  "The root dir of the config file.")
(defvar abn-core-dir (expand-file-name "core" abn-dir)
  "The home of core functionality.")
(defvar abn-modules-dir (expand-file-name  "modules" abn-dir)
  "This directory houses all of the modules.")

(add-to-list 'load-path abn-core-dir)
(add-to-list 'load-path abn-modules-dir)

;; Core
(require 'abn-packages)
(require 'abn-keybindings)
(require 'abn-ui)
(when (eq system-type 'darwin) (require 'abn-mac-os))

;; Modules
(require 'abn-evil)
(require 'abn-crux)

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from Emacs (especially on Microsoft Windows).
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file

;; Don't ask for confirmation when opening symlinked file.
(setq vc-follow-symlinks t )

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name

(setq inhibit-startup-screen t )
(setq ring-bell-function 'ignore)
(setq initial-scratch-message "")

;; Warns when opening files bigger than 100MB.
(setq large-file-warning-threshold 100000000)

;; Double space for sentences.
(setq-default sentence-end-double-space t)

;; Newline at end of file
(setq require-final-newline t)

;; Revert buffers automatically when underlying files are changed externally.
(global-auto-revert-mode t)

(add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially)

(require 'uniquify)
;; When having windows with repeated filenames, uniquify them
;; by the folder they are in rather those annoying <2>,<3>,.. etc
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      ;; don't screw special buffers
      uniquify-ignore-buffers-re "^\\*")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
