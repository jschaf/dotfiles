;;; abn-module-emacs-lisp.el --- Elisp setup

;;; Commentary:
;;

;; Packages

(require 'general)

(use-package abn-funcs-emacs-lisp
  :ensure nil ; built-in
  :commands
  (abn/nav-find-elisp-thing-at-point-other-window
   abn/ert-run-tests-buffer
   abn/overwrite-lisp-indent-func)
  :init
  (add-hook 'emacs-lisp-mode-hook #'abn/overwrite-lisp-indent-func)
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (abn/define-leader-keys-for-major-mode mode
      "gG" 'abn/nav-find-elisp-thing-at-point-other-window
      "tb" 'abn/ert-run-tests-buffer
      "tq" 'ert)))

;; Minor mode that keeps emacs lisp code always indented.
(use-package aggressive-indent
  :defer t
  :diminish aggressive-indent-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'lisp-interaction-mode-hook 'aggressive-indent-mode))

(use-package eldoc
  :diminish 'eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

;; Goto definition for emacs lisp.
(use-package elisp-slime-nav
  :defer t
  :diminish elisp-slime-nav-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
    (add-hook 'lisp-interaction-mode-hook 'elisp-slime-nav-mode)

    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (abn/declare-prefix-for-mode mode "mg" "find-symbol")
      (abn/declare-prefix-for-mode mode "mh" "help")
      (abn/define-leader-keys-for-major-mode mode
	"hh" 'elisp-slime-nav-describe-elisp-thing-at-point
	"gg" 'elisp-slime-nav-find-elisp-thing-at-point))))

(use-package elisp-mode
  :ensure nil ; built-in
  :demand
  :init
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (abn/declare-prefix-for-mode mode "mc" "compile")
    (abn/declare-prefix-for-mode mode "me" "eval")
    (abn/declare-prefix-for-mode mode "mt" "tests")
    (abn/define-leader-keys-for-major-mode mode
      ","  'lisp-state-toggle-lisp-state
      "cc" 'emacs-lisp-byte-compile
      "e$" 'lisp-state-eval-sexp-end-of-line
      "eC" 'abn/eval-current-form
      "ec" 'abn/eval-current-form-sp
      "eb" 'eval-buffer
      "ee" 'eval-last-sexp
      "ef" 'eval-defun
      "el" 'lisp-state-eval-sexp-end-of-line
      "er" 'eval-region
      "es" 'abn/eval-current-symbol-sp
      "tq" 'ert))
  :config
  (defun abn/shorten-emacs-lisp-mode-name ()
    (setq mode-name "Elisp"))
  (add-hook 'emacs-lisp-mode-hook #'abn/shorten-emacs-lisp-mode-name))

(use-package evil-lisp-state
  :defer t
  :init
  (setq evil-lisp-state-global nil)
  :config
  (abn/define-leader-keys "k" evil-lisp-state-map))

(provide 'abn-module-emacs-lisp)
;;; abn-module-emacs-lisp.el ends here
