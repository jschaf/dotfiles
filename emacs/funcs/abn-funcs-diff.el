;;; abn-funcs-diff.el --- Functions for diff

;;; Commentary:
;;

;;; Code:
(defun abn/ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff
   ediff-current-difference nil 'C nil
   (concat
    (ediff-get-region-contents
     ediff-current-difference 'A ediff-control-buffer)
    (ediff-get-region-contents
     ediff-current-difference 'B ediff-control-buffer))))

(defun abn/add-copy-both-to-ediff-mode-map ()
  (define-key ediff-mode-map "B" 'ediff-copy-both-to-C))

(provide 'abn-funcs-diff)
;;; abn-funcs-diff.el ends here
