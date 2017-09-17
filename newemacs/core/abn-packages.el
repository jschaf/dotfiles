;;; abn-packages.el --- Default package selection

;;; Commentary:
;;

;;; Code:
(require 'cl)
(require 'package)

(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Set package-user-dir to be relative to abn install path.
(setq package-user-dir (expand-file-name "elpa" abn-dir))
(package-initialize)

(defvar abn-essential-packages
  '(dash
    f
    spacemacs-theme
    use-package)
  "A list of packages to ensure are installed at launch.")

(defun abn-all-packages-installed-p ()
  "Check if all packages in `abn-packages' are installed."
  (every #'package-installed-p abn-essential-packages))

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

(load-theme 'spacemacs-dark 'no-confirm)

(provide 'abn-packages)
;;; abn-packages.el ends here
