;;; abn-module-elisp-testing.el --- Config for elisp-test

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-elisp-testing
  :ensure nil ; local package
  :commands (def-test! abn/run-all-tests)
  :bind
  (:map abn-leader-map
   ("fet" . abn/run-all-tests))
  )



(provide 'abn-module-elisp-testing)
;;; abn-module-elisp-testing.el ends here
