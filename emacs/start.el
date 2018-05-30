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

;;; Code:

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 0.8MB.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
;; Portion of heap used for allocation.  Defaults to 0.1.
(setq gc-cons-percentage 0.6)

;; Core

(add-to-list 'load-path "~/.dotfiles/emacs/core")
;; Must come first
(require 'abn-core-constants)
(require 'abn-core-packages)
(require 'abn-core-emacs-settings)
(require 'abn-core-keybindings)
(require 'abn-core-lib)
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
(require 'abn-module-elisp-testing)
(require 'abn-module-emacs-config)
(require 'abn-module-emacs-lisp)
(require 'abn-module-email)
(require 'abn-module-evil)
(require 'abn-module-git)
(require 'abn-module-help)
(require 'abn-module-highlight)
(require 'abn-module-ivy)
(require 'abn-module-java)
(require 'abn-module-javascript)
(require 'abn-module-langs)
(require 'abn-module-magit)
(require 'abn-module-markup)
(require 'abn-module-mode-line)
(require 'abn-module-org)
(require 'abn-module-projectile)
(require 'abn-module-python)
(require 'abn-module-shell-script)
(require 'abn-module-smartparens)
(unless (display-graphic-p) (require 'abn-module-tty))

;; Ignore errors if work file isn't found.
(require 'work-init nil 'noerror)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; Suppress message "For information about GNU Emacs..."
(setq inhibit-startup-echo-area-message "jschaf")

;; Use a hook so the message doesn't get clobbered by other messages.
;; (add-hook 'emacs-startup-hook)
(add-hook! 'emacs-startup-hook
           (message "Emacs ready in %s"
                    (format "%.2f seconds"
                            (float-time
                             (time-subtract after-init-time
                                            before-init-time)))))
