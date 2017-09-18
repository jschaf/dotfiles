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

(provide 'abn-funcs-emacs-lisp)

;;; abn-funcs-emacs-lisp.el ends here
