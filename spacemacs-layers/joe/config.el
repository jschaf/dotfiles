;;; config.el --- Configuration for Joe's private layer.


;;; Commentary:
;;

;;; Code:

(when (eq system-type 'darwin)
  (defvar mac-option-modifier)
  (defvar mac-command-modifier)

  (setq mac-option-modifier 'meta
        mac-command-modifier 'super)

  (defvar my:mac-modifier-state 'mac
    "Toggle between BUILT-IN and USB.")

  (defun my:toggle-mac-modifiers ()
    (interactive)
    (if (eq my:mac-modifier-state 'usb)
        (progn
          (setq my:mac-modifier-state 'built-in
                mac-option-modifier 'control
                mac-command-modifier 'meta)
          (message "Mac modifier keys set for Mac keyboard."))

      (setq my:mac-modifier-state 'usb
            mac-option-modifier 'meta
            mac-command-modifier 'super)
      (message "Mac modifier keys set for USB keyboard."))))

(defun my:back-to-indentation-or-beginning ()
  "Move point to first non-whitespace char or `beginning-of-line'."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun unfill-paragraph ()
  "Replace newline characters with a single space."
  (interactive)
  (let ((fill-column 90002000))
    (if mark-active
        (fill-region (region-beginning) (region-end))
      (fill-paragraph))))

(defun my:new-blah-buffer ()
  "Open up a guaranteed new blah (scratch) buffer."
  (interactive)
  (switch-to-buffer (cl-loop for num from 0
                          for name = (format "blah-%03i" num)
                          while (get-buffer name)
                          finally return name)))

(defun my:switch-to-blah-buffer ()
  "Switch to a blah buffer, or create a new one."
  (interactive)
  (cl-loop for buffer in (buffer-list)
      if (string-match "blah-.+" (buffer-name buffer))
         return (switch-to-buffer buffer)
      finally do (my:new-blah-buffer)))

(setq-default evil-escape-key-sequence "jk")

(setq-default sentence-end-double-space t)

(defun my:nuke-all-buffers ()
  "Kill all buffers, leaving only *scratch* only."
  (interactive)
  (mapc
   (lambda (buffer)
     (kill-buffer buffer))
   (buffer-list))
  (delete-other-windows))

;; Use a decent font.
(defun fontify-frame (frame)
  "Use appropriate font and size on FRAME."
  (interactive)
  (let ((font-family (if (member "Consolas for Powerline" (font-family-list))
                         "Consolas for Powerline"
                       (face-attribute 'default :family))))
    (when window-system
      (if (> (display-pixel-width) 2000)
          (set-frame-parameter frame 'font (format "%s 13" font-family))
        (set-frame-parameter frame 'font (format "%s 13" font-family))))))

;; Fontify current frame
(fontify-frame nil)

(require 'core-keybindings)

(with-eval-after-load 'lisp-mode
  (require 'pp)
  (defun my-pp-eval-last-sexp-in-current-buffer ()
    (interactive)
    (pp-eval-last-sexp t))
  (spacemacs/set-leader-keys-for-major-mode 'lisp-interaction-mode
    "ep" 'my-pp-eval-last-sexp-in-current-buffer
    "eP" 'pp-eval-last-sexp))

;; I always hit this by mistake to get to `describe-char' and I'm tired of
;; seeing the GNU license
(global-set-key (kbd "C-h C-c") 'describe-key-briefly)

;; Follow symlinks to source controlled files without prompting.
(setq vc-follow-symlinks t)

;; UTF-8 everywhere
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; .dir-local.el tweaks
;;
;; See http://stackoverflow.com/questions/5147060/ for explanation of
;; why we're creating a new hook.  tl;dr: wierd interaction between
;; `set-auto-mode' and `hack-local-variables'
(add-hook 'hack-local-variables-hook 'my:run-local-vars-mode-hook)
(defun my:run-local-vars-mode-hook ()
  "Hook for all major-modes after processing local variables.
Creates a hook for all major modes.
e.g. `python-mode-local-vars-hook',
`emacs-lisp-mode-local-vars-hook'"
  (run-hooks (intern (format "%s-local-vars-hook" (symbol-name major-mode)))))

(defvar my:use-jinja-for-html-p nil
  "Use `jinja2-mode' if non-nil, otherwise `html-mode'.
Primarily for use in .dir-locals.el")

(defun my:maybe-choose-jinja2-mode ()
  (when my:use-jinja-for-html-p
    (jinja2-mode)))

(add-hook 'html-mode-local-vars-hook 'my:maybe-choose-jinja2-mode)

;; Custom keymaps
(defvar joe-map (make-keymap))

(defun joe/set-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under `joe-map'.
The key for `joe-map' is `joe-leader-key'.  KEY should be a
string suitable for passing to `kbd', and it should not include
the leaders.  DEF is most likely a quoted command.  See
`define-key' for more information about the possible choices for
DEF.  This function simply uses `define-key' to add the bindings.

For convenience, BINDINGS is additional KEY DEF pairs.  For
example,

\(joe/set-leader-keys
   \"a\" 'command1
   \"c\" 'command2
   \"bb\" 'command3\)"
  (while key
    (define-key joe-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))

(spacemacs/set-leader-keys
  "," joe-map)

(joe/set-leader-keys
 "nb" 'my:nuke-all-buffers)


;; ;; optional but very useful libraries in org-ref
;; (add-to-list 'load-path "~/prog/org-ref")
;; (require 'org-ref)
;; (require 'doi-utils)
;; (require 'jmax-bibtex)
;; (require 'pubmed)
;; (require 'arxiv)
;; (require 'sci-id)
;; (require 'bibtex)
;; (require 'reftex-cite)

;; (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib")

;;       org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
;;       org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
;;       org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/"

;;       helm-bibtex-bibliography "~/Dropbox/bibliography/references.bib"
;;       helm-bibtex-library-path "~/Dropbox/bibliography/bibtex-pdfs"
;;       helm-bibtex-notes-path "~/Dropbox/bibliography/helm-bibtex-notes"

;;       bibtex-file-path ".:~/Dropbox/bibliography/"
;;       )


;; (add-to-list 'load-path "~/.dotfiles/spacemacs-layers/joe/")
;; (require 'joe-blog)

;; (joe/set-leader-keys
;;  "tm" 'my:toggle-mac-modifiers
;;  "bb" 'my:switch-to-blah-buffer
;;  "bB" 'my:new-blah-buffer
;;  "cb" 'joe-blog-compile
;;  "cB" '(lambda () (interactive) (joe-blog-compile 'force))
;;  "cp" 'joe-blog-publish
;;  "cP" 'joe-blog-purge-everything)

(provide 'config)

;;; config.el ends here
