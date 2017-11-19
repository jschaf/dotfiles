;;; abn-module-benchmark.el --- Config for benchmark

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-benchmark
  :ensure nil ; local package
  :commands (abn/list-loaded-packages abn/list-loaded-features))

(provide 'abn-module-benchmark)
;;; abn-module-benchmark.el ends here
