;;; abn-funcs-emacs-lisp.el --- Functions for Emacs lisp

;; Functions

;;; Commentary:
;;

;;; Code:

(defun abn//find-ert-test-buffer (ert-test)
  "Return the buffer where ERT-TEST is defined."
  (car (find-definition-noselect (ert-test-name ert-test) 'ert-deftest)))

(defun abn/ert-run-tests-buffer ()
  "Run all the tests in the current buffer."
  (interactive)
  (save-buffer)
  (load-file (buffer-file-name))
  (let ((cbuf (current-buffer)))
    (ert '(satisfies (lambda (test)
                       (eq cbuf (spacemacs//find-ert-test-buffer test)))))))


(defun abn/nav-find-elisp-thing-at-point-other-window ()
  "Find thing under point and go to it another window."
  (interactive)
  (let ((symb (variable-at-point)))
    (if (and symb
             (not (equal symb 0))
             (not (fboundp symb)))
        (find-variable-other-window symb)
      (find-function-at-point))))

;; Fix keyword alignment to avoid:
;;
;; (:keymaps 'ivy-minibuffer-map
;;            "C-j" 'ivy-next-line)
;;
;; https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
(defun abn/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(defun abn/overwrite-lisp-indent-func ()
  (setq-local lisp-indent-function #'abn/lisp-indent-function))

(provide 'abn-funcs-emacs-lisp)

;;; abn-funcs-emacs-lisp.el ends here
