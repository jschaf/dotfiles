
(require 'org)
(require 'ox)
(require 'ox-html-clean)


(defvar joe-blog-directory "~/prog/blog-redux")
(defvar joe-blog-html-postamble
  "<div class='footer'>
Copyright 2015 %a (%v HTML).<br>
Last updated %C. <br>
Built with %c.
</div>")

(setf org-html-mathjax-options
      '((path "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML")
        (scale "100") (align "left") (indent "2em") (mathml nil)))

(setq org-publish-project-alist
      `(("blog-redux-content"
         :author "Joe Schafer"
         :email "Joe.Schafer@delta46.us"
         :base-directory ,joe-blog-directory
         :base-extension "org"
         :publishing-directory "~/prog/blog-redux/output"
         :publishing-function org-html-clean-publish-to-html

         ;; HTML options
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-html5-fancy t
         :html-doctype "html5"
         :html-postamble ,joe-blog-html-postamble

         ;; General Options
         :with-toc nil
         :headline-levels 3
         :table-of-contents nil
         :section-numbers nil
         :auto-sitemap t
         )
        ("blog-redux-static"
         :base-directory "~/prog/blog-redux/static"
         :base-extension "css"
         :publishing-directory "~/prog/blog-redux/output"
         :publishing-function org-publish-attachment
         )
        ("blog-redux"
         :components ("blog-redux-content" "blog-redux-static"))))

(defun joe-blog-compile ()
  (interactive)
  (org-publish "blog-redux" 'force))

(defun joe-blog-publish-to-server ()
  (interactive)
  (compile (format "make -C %s publish" joe-blog-directory)))

(defun joe-blog-compile-and-publish ()
  (interactive)
  (joe-blog-compile)
  (joe-blog-publish-to-server))

(evil-leader/set-key
  "cb" 'joe-blog-compile-and-publish)

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "warning" nil t))))
      (run-with-timer 0.5 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                      buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(provide 'joe-blog)
