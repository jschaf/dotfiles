;;; abn-funcs-elisp-testing.el --- Functions for elisp-test

;;; Commentary:
;;

;;; Code:

(require 'abn-core-constants)
(require 'abn-core-lib)

(defun abn/run-all-tests ()
  "Runs all files ending in '-test.el'."
  (interactive) 
  (cl-loop for test-file in (directory-files-recursively abn-dir ".*-test\.el$")
           do (load-file test-file))
  (if noninteractive
      (ert-run-tests-batch-and-exit)
    (ert-run-tests-interactively t)))

(defmacro def-test! (name &rest body)
  "Define a namespaced ERT test with NAME using BODY."
  (declare (indent defun) (doc-string 2))
  (let (plist)
    (while (keywordp (car body))
      (push (pop body) plist))
    (setq plist (reverse plist))
    (when (plist-get plist :skip)
      (setq body `((ert-skip nil) ,@body)))
    (when-let* ((modes (abn/listify (plist-get plist :minor-mode))))
      (dolist (mode modes)
        (setq body `((with-minor-mode!! ,mode ,@body)))))
    (when-let* ((before (plist-get plist :before)))
      (setq body `(,@before ,@body)))
    (when-let* ((after (plist-get plist :after)))
      (setq body `(,@body @after)))
    `(ert-deftest
         ,(cl-loop with path = (file-relative-name
                                (file-name-sans-extension load-file-name)
                                abn-dir)
                   for (rep . with) in '(("/test/" . "/") ("/" . ":"))
                   do (setq path (replace-regexp-in-string rep with path t t))
                   finally return (intern (format "%s::%s" path name)))
         ()
       (with-temp-buffer
         (save-mark-and-excursion
          (save-window-excursion
            ,@body))))))

(defmacro with-minor-mode!! (mode &rest body)
  "Enable MODE and eval BODY."
  (declare (indent defun))
  `(progn (,mode +1)
          ,@body
          (,mode -1)))

(provide 'abn-funcs-elisp-testing)
;;; abn-funcs-elisp-testing.el ends here
