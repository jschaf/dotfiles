;;; packages.el --- ELSE Layer packages File for Spacemacs
;;
;; Copyright (c) 2016 Joe Schafer
;;
;; Author: Joe Schafer <joe@jschaf.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq else-packages
      '(
        (else-mode :location local)
        ))

(defun else/init-else ()
  (use-package else-mode
    :defer t))

