;;; packages.el --- joe Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org


;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(defvar joe-packages
  '(
    auto-yasnippet
    (doc-popup
     :location local)
    ebib
    emacs-lisp
    evil
    evil-escape
    helm-bibtex
    help-fns+
    hydra
    jinja2-mode
    key-chord
    magit
    org
    (org-ref
     :location local)
    (otb
     :location local)
    persistent-scratch
    pos-tip
    request
    s
    typescript
    )
  "List of all packages to install and/or initialize.
Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar joe-excluded-packages '()
  "List of packages to exclude.")


(defun joe/post-init-auto-yasnippet ()
  "Init auto-yasnippet."
  (use-package auto-yasnippet
    :config
    (progn
      (setq aya-persist-snippets-dir "~/.dotfiles/snippets")
      (add-to-list 'yas-snippet-dirs "~/.dotfiles/snippets")
      (setq yas-snippet-dirs (delete "~/.emacs.d/snippets" yas-snippet-dirs))
      (setq yas-snippet-dirs (delete
                              (expand-file-name "~/.emacs.d/private/snippets/")
                              yas-snippet-dirs)))))

(defun joe/init-doc-popup ()
  "Init doc-popup."
  (use-package doc-popup
    :config
    (progn
      (defvar evil-normal-state-map)
      (define-key evil-normal-state-map "gh" 'doc-popup-show-at-point))))

(defun joe/init-ebib ()
  "Init ebib."
  (use-package ebib))

(defun joe/init-helm-bibtex ()
  "Init helm-bibtex."
  (use-package helm-bibtex))

(defun joe/init-key-chord ()
  "Init key-chord."
  (use-package key-chord))

(defun joe/init-jinja2-mode ()
  "Init jinja2-mode."
  (use-package jinja2
    :defer t
    :init
    (progn
      (defun my-jinja2-block (id action context)
        (insert " ")
        (save-excursion
          (insert " ")))

      (require 'smartparens)
      (defvar sp-navigate-consider-stringlike-sexp)
      (add-to-list 'sp-navigate-consider-stringlike-sexp
                   'jinja2-mode)

      ;; Remove curly brace binding because it prevents
      ;; a binding for Jinja constructs.
      (sp-local-pair 'jinja2-mode "{" "}" :actions nil)
      (sp-local-pair 'jinja2-mode "{%" "%}"
                     :post-handlers '(:add my-jinja2-block)
                     :trigger "jjb")
      (sp-local-pair 'jinja2-mode "{{" "}}"
                     :post-handlers '(:add my-jinja2-block)
                     :trigger "jji")))
  :config
  (progn
    (add-hook 'jinja2-mode-hook 'smartparens-mode)))

(defun joe/post-init-evil ()
  "Init evil."
  (use-package evil
    :config
    (progn
      (eval-when-compile
        (require 'evil-macros))

      (evil-define-motion my:evil-next-visual-line-5 (count)
        "Move the cursor 5 lines up."
        :type line
        (let (line-move-visual)
          (evil-next-visual-line (* 5 (or count 1)))))

      (evil-define-motion my:evil-previous-visual-line-5 (count)
        "Move the cursor 5 lines up."
        :type line
        (let (line-move-visual)
          (evil-previous-visual-line (* 5 (or count 1)))))

      (define-key evil-normal-state-map "\M-k" 'spacemacs/evil-smart-doc-lookup)
      (define-key evil-normal-state-map "K" 'my:evil-previous-visual-line-5)
      (cl-loop for (key . func) in
               `(("J" . my:evil-next-visual-line-5)
                 ("K" . my:evil-previous-visual-line-5)
                 ("gj" . evil-join)
                 ("H" . my:back-to-indentation-or-beginning)
                 ("L" . evil-end-of-line)
                 ("\C-j" . scroll-up-command)
                 ("\C-k" . scroll-down-command))
               do
               (define-key evil-normal-state-map key func)
               (define-key evil-visual-state-map key func)
               (define-key evil-motion-state-map key func))
      ;; Make movement keys work on visual lines instead of acutal lines.
      ;; This imitates Emacs behavior rather than Vim behavior.
      (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>")
        'evil-next-visual-line)
      (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>")
        'evil-previous-visual-line)
      (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>")
        'evil-next-visual-line)
      (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>")
        'evil-previous-visual-line)

      ;; We need to add text before we can edit it.
      (add-to-list 'evil-insert-state-modes 'git-commit-mode)

      (unless window-system
        ;; C-i is the same as tab in the terminal
        (setq evil-want-C-i-jump nil)
        ;; I'm not sure why the above variable isn't respected. I think it's evil's
        ;; fault. I didn't see any key rebinding in spacemacs.
        (define-key evil-motion-state-map "\C-i" nil)))))

(defun joe/post-init-evil-escape ()
  "Init evil-escape."
  (use-package evil-escape
    :config
    (progn
      (setq evil-escape-unordered-key-sequence t))))

(defun joe/init-hydra ()
  (use-package hydra
    :config
    (progn
      )))

(defun joe/post-init-org ()
  "Init org."
  (use-package org
    :config
    (progn
      (setq org-src-fontify-natively t)

      (with-eval-after-load 'ox-publish
        (dolist (project
                 `(("sp-biz-plan"
                    :author "Joe Schafer"
                    :base-directory "~/prog/swift-plaques-business-plan"
                    :publishing-directory "~/prog/swift-plaques-business-plan/output"
                    :base-extension "org"
                    )))
          (my:replace-or-add-to-alist 'org-publish-project-alist project)))

      (defun my:make-org-link-cite-key-visible (&rest _)
        "Make the org-ref cite link visible in descriptive links."
        (when (string-prefix-p "cite:" (match-string 1))
          (remove-text-properties (+ (length "cite:") (match-beginning 1))
                                  (match-end 1)
                                  '(invisible))))

      (defun my:org-set-tag-as-drill ()
        (interactive)
        (org-toggle-tag "drill"))
      (joe/set-leader-keys
       "dd" 'my:org-set-tag-as-drill)

      (with-eval-after-load 'ox-latex
        (let* ((text-spacing
                (s-join
                 "\n"
                 '("\\ifxetex"
                   "  \\newcommand{\\textls}[2][5]{%"
                   "  \\begingroup\\addfontfeatures{LetterSpace=#1}#2\\endgroup"
                   "}"
                   "\\renewcommand{\\allcapsspacing}[1]{\\textls[15]{#1}}"
                   "\\renewcommand{\\smallcapsspacing}[1]{\\textls[10]{#1}}"
                   "\\renewcommand{\\allcaps}[1]{\\textls[15]{\\MakeTextUppercase{#1}}}"
                   "\\renewcommand{\\smallcaps}[1]{\\smallcapsspacing{\\scshape\\MakeTextLowercase{#1}}}"
                   "\\renewcommand{\\textsc}[1]{\\smallcapsspacing{\\textsmallcaps{#1}}}"
                   "\\fi")))
               (tufte-handout-class
                `("tufte-handout"
                  ,(s-join "\n"
                           `("\\documentclass{tufte-handout}"
                             "[DEFAULT-PACKAGES]"
                             "[EXTRA]"
                             ,text-spacing
                             "% http://tex.stackexchange.com/questions/200722/"
                             "[PACKAGES]"))
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")))
               (tufte-book-class
                `("tufte-book"
                  ,(s-join "\n"
                           `("\\documentclass{tufte-handout}"
                             "[DEFAULT-PACKAGES]"
                             "[EXTRA]"
                             ,text-spacing
                             "% http://tex.stackexchange.com/questions/200722/"
                             "[PACKAGES]"))
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}"))))
          (my:replace-or-add-to-alist 'org-latex-classes tufte-book-class)
          (my:replace-or-add-to-alist 'org-latex-classes tufte-handout-class))))))

(defun joe/init-persistent-scratch ()
  "Init persistent-scratch."
  (use-package persistent-scratch
    :config
    (progn
      (persistent-scratch-autosave-mode 1)
      ;; Don't clog up .emacs.d
      (setq persistent-scratch-save-file "~/.emacs-persistent-scratch")

      ;; Ensure file exists
      (unless (file-exists-p persistent-scratch-save-file)
        (write-region "" nil persistent-scratch-save-file))

      (with-current-buffer "*scratch*"
        (emacs-lisp-mode)
        (lisp-interaction-mode)
        (if (= (buffer-size) 0)
            (persistent-scratch-restore)

          (save-excursion
            (goto-char (point-max))
            (insert "\n\n;; Old Scratch\n\n"))
          (with-temp-buffer
            (insert-file-contents persistent-scratch-save-file)
            (append-to-buffer "*scratch*" (point-min) (point-max)))))

      (defun joe--advise-write-file-for-scratch (orig-fun &rest args)
        (if (eq (current-buffer) (get-buffer "*scratch*"))
            (progn (persistent-scratch-save)
                   (message "Wrote *scratch* to %s." persistent-scratch-save-file))
          (apply orig-fun args)))

      (advice-add 'spacemacs/write-file :around
                  #'joe--advise-write-file-for-scratch))))

(defun joe/init-typescript-mode ()
  "Init typescript-mode."
  (use-package typescript
    :init
    (progn
      (with-eval-after-load 'compile
        (add-to-list 'compilation-error-regexp-alist 'typescript)
        (add-to-list 'compilation-error-regexp-alist-alist
                     '(typescript "^\\(.+?\\)(\\([[:digit:]]+\\),\\([[:digit:]]+\\)): \\(.*\\)$"
                                  1 2 3 nil 1))


        (add-to-list 'compilation-error-regexp-alist 'typescript-lint)
        ;; ornament/static/js/main.ts[176, 34]: expected parameter: 'error' to have a typedef
        (add-to-list 'compilation-error-regexp-alist-alist
                     '(typescript-lint "^\\(.+?\\)\\[\\([[:digit:]]+\\), \\([[:digit:]]+\\)\\]: \\(.*\\)$"
                                       1 2 3 nil 1))))))

(defun joe/post-init-magit ()
  "Init magit."
  (use-package magit
    :config
    (progn
      (require 'smerge-mode)
      (setq smerge-refine-ignore-whitespace nil))))

(defun joe/init-otb ()
  "Init otb."
  (use-package otb
    :config
    (progn
      (joe/set-leader-keys
       "tm" 'my:toggle-mac-modifiers
       "bb" 'my:switch-to-blah-buffer
       "bB" 'my:new-blah-buffer
       "cb" 'joe-blog-compile
       "cB" '(lambda () (interactive) (joe-blog-compile 'force))
       "cp" 'joe-blog-publish
       "cP" 'joe-blog-purge-everything))
    ))



(defun joe/init-org-ref ()
  "Init org-ref."
  (use-package org-ref
    :config
    (progn
      ;; optional but very useful libraries in org-ref
      (require 'doi-utils)
      (require 'jmax-bibtex)
      (require 'pubmed)
      (require 'arxiv)
      (require 'sci-id)
      (require 'bibtex)
      (require 'reftex-cite)

      ;; (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib")

      ;;       org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
      ;;       org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
      ;;       org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/"

      ;;       helm-bibtex-bibliography "~/Dropbox/bibliography/references.bib"
      ;;       helm-bibtex-library-path "~/Dropbox/bibliography/bibtex-pdfs"
      ;;       helm-bibtex-notes-path "~/Dropbox/bibliography/helm-bibtex-notes"

      ;;       bibtex-file-path ".:~/Dropbox/bibliography/"
      ;;       )
      )))

(defun joe/post-init-s ()
  "Init s ()."
  (use-package s
    :config
    (progn
      (defun my:snake-case-at-point-or-region ()
        "Snake_case the current word or text selection."
        (interactive)
        (operate-on-point-or-region 's-snake-case))

      (defun my:dasherise-at-point-or-region ()
        "Dasherise-the-current CamelCase or snake_case word or text selection."
        (interactive)
        (operate-on-point-or-region 's-dashed-words))

      (defun my:upper-camelcase-at-point-or-region ()
        "UpperCamelCaseTheCurrent dashed-or-snake_case_words or any words in text selection."
        (interactive)
        (operate-on-point-or-region 's-upper-camel-case))

      (defun my:lower-camelcase-at-point-or-region ()
        "LowerCamelCaseTheCurrent dashed or snake_case word or any words in text selection."
        (interactive)
        (operate-on-point-or-region 's-lower-camel-case))

      (defun my:humanize-at-point-or-region ()
        "Humanize variable names, insert spaces instead of - or _ or un-CamelCase humps to spaced words."
        (interactive)
        (operate-on-point-or-region 's-capitalized-words))

      (defun my:titleized-at-point-or-region ()
        "Convert snaked, dashed, underscored, camelcase, or spaced words in region to Title Case."
        (interactive)
        (operate-on-point-or-region 's-titleized-words))

      (joe/set-leader-keys
       "ss" 'my:snake-case-at-point-or-region
       "sd" 'my:dasherise-at-point-or-region
       "scu" 'my:upper-camelcase-at-point-or-region
       "scl" 'my:lower-camelcase-at-point-or-region
       "sh" 'my:humanize-at-point-or-region
       "st" 'my:titleized-at-point-or-region))))

(provide 'packages)

;;; packages.el ends here
