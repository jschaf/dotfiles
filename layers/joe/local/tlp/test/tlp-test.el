;;; tlp-test.el --- tests for the tlp project

(require 'ert)
(require 'tlp)
(require 'org)

(defmacro tlp/with-org-buffer (str &rest body)
  "Create an `org-mode' buffer with STR and run body."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (let ((buffer-file-name "*tlp-test*"))
       (insert ,str)
       (goto-char (point-min))
       ,@body)))

(ert-deftest tlp--extract-org-json-src-block ()
  (tlp/with-org-buffer "*** config
#+BEGIN_SRC json
{\"shortName\": \"tlp\"}
#+END_SRC"
    (should (equal (tlp--extract-org-json-src-block)
                   "{\"shortName\": \"tlp\"}\n"))))

(ert-deftest tlp--extract-org-json-src-block_missing-begin-src ()
  (tlp/with-org-buffer "*** config
{\"shortName\": \"tlp\"}
#+END_SRC"
    (should-error (tlp--extract-org-json-src-block))))

(ert-deftest tlp--load-config-json ()
  (should (equal (tlp--load-config-json "{\"a\": 2}")
                 '((a . 2)))))

(ert-deftest tlp--load-config-json_error ()
  (should-error (tlp--load-config-json "{\"a\"}")
                :type 'tlp-error))
