;;; abn-core-packages.el --- Default package selection

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'cl))
(require 'package)

;; Always load the newer .el or .elc file.
(setq load-prefer-newer t)

(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(defvar abn-essential-packages
  '(bind-map
     dash
     evil
     f
     general
     spacemacs-theme
     use-package
     which-key)
  "A list of packages to ensure are installed at launch.")

(defun abn-all-packages-installed-p ()
  "Check if all packages in `abn-packages' are installed."
  (cl-every #'package-installed-p abn-essential-packages))

(defun abn-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (package-installed-p package)
    (package-install package)))

(defun abn-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'abn-require-package packages))

(defun abn-install-packages ()
  "Install all packages listed in `abn-essential-packages'."
  (unless (abn-all-packages-installed-p)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "done.")
    (abn-require-packages abn-essential-packages)))

(abn-install-packages)

(setq use-package-always-ensure t)

(provide 'abn-core-packages)
;;; abn-core-packages.el ends here
