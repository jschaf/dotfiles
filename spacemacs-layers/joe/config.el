(setq mac-option-modifier 'meta
      mac-command-modifier 'super)


(defvar my:mac-modifier-state 'mac
  "Toggle between BUILT-IN and USB")

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
    (message "Mac modifier keys set for USB keyboard.")))

(my:toggle-mac-modifiers)

(defun my:back-to-indentation-or-beginning ()
  "Go back to indentation, or beginning of line on second press."
  (interactive)
  (if (eq last-command 'my:back-to-indentation-or-beginning)
      (evil-beginning-of-line)
    (evil-first-non-blank)))

(defmacro my:make-evil-line-move-motion (name multiplier)
  `(evil-define-motion ,name (count)
     ,(format "Move the cursor (COUNT * %s) lines down." multiplier)
     :type line
     (let (line-move-visual)
       (evil-next-visual-line (* ,multiplier (or count 1))))))

(my:make-evil-line-move-motion my:evil-next-visual-line-5 5)
(my:make-evil-line-move-motion my:evil-previous-visual-line-5 -5)

(defun unfill-paragraph ()
  "Replace newline characters with a single space."
  (interactive)
  (let ((fill-column 90002000))
    (if mark-active
        (fill-region (region-beginning) (region-end))
      (fill-paragraph))))

(defun my:back-to-indentation-or-beginning ()
  "Go back to indentation, or beginning of line on second press."
  (interactive)
  (if (eq last-command 'my:back-to-indentation-or-beginning)
      (evil-beginning-of-line)
    (evil-first-non-blank)))

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



(with-eval-after-load 'lisp-mode
  (require 'pp)
  (defun my-pp-eval-last-sexp-in-current-buffer ()
    (interactive)
    (pp-eval-last-sexp t))
  (spacemacs/set-leader-keys-for-major-mode 'lisp-interaction-mode
    "ep" 'my-pp-eval-last-sexp-in-current-buffer
    "eP" 'pp-eval-last-sexp))

(defun my:evil-keybindings ()
  (interactive)
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
           (define-key evil-motion-state-map key func)))

;; I always hit this by mistake to get to `describe-char' and I'm tired of
;; seeing the GNU license
(global-set-key (kbd "C-h C-c") 'describe-key-briefly)

(use-package evil
  :init
  (progn
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

  ;; (define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
  ;; We need to add text before we can edit it.
  (add-to-list 'evil-insert-state-modes 'git-commit-mode)

  (unless window-system
    ;; C-i is the same as tab in the terminal
    (setq evil-want-C-i-jump nil)
    ;; I'm not sure why the above variable isn't respected. I think it's evil's
    ;; fault. I didn't see any key rebinding in spacemacs.
    (define-key evil-motion-state-map "\C-i" nil))))

;; Follow symlinks to source controlled files without prompting.
(setq vc-follow-symlinks t)

(setq tramp-default-method "ssh")

(with-eval-after-load 'evil-escape
  (setq evil-escape-unordered-key-sequence t))

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

(use-package org
  :config
  (progn
    (setq org-src-fontify-natively t)

    (defun my:make-org-link-cite-key-visible (&rest _)
      "Make the org-ref cite link visible in descriptive links."
      (when (string-prefix-p "cite:" (match-string 1))
        (remove-text-properties (+ (length "cite:") (match-beginning 1))
                                (match-end 1)
                                '(invisible))))

    (advice-add 'org-activate-bracket-links :after #'my:make-org-link-cite-key-visible)
    ;; (advice-remove 'org-activate-bracket-links #'my:make-org-link-cite-key-visible)
    ))


(defun my:nuke-all-buffers ()
  "Kill all buffers, leaving only *scratch* only."
  (interactive)
  (mapc
   (lambda (buffer)
     (kill-buffer buffer))
   (buffer-list))
  (delete-other-windows))

;; Custom keymaps
(defvar joe-map (make-keymap))

(defun joe/set-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under `joe-map' and
`joe-leader-key'.  KEY should be a string suitable for passing to
`kbd', and it should not include the leaders. DEF is most likely
a quoted command. See `define-key' for more information about the
possible choices for DEF. This function simply uses `define-key'
to add the bindings.

For convenience, this function will accept additional KEY DEF
pairs. For example,

\(joe/set-leader-keys
   \"a\" 'command1
   \"C-c\" 'command2
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
