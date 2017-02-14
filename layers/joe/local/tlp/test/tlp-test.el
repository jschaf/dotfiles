;;; tlp-test.el --- tests for the tlp project

(require 'ert)
(require 'noflet)
(require 'org)
(require 'tlp)

(defmacro tlp/with-org-buffer (str &rest body)
  "Create an `org-mode' buffer with STR and run body."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (noflet ((file-exists-p (filename) t)
              (buffer-file-name (&optional buffer) "*tlp-test*"))
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

(ert-deftest tlp--extract-org-json-src-block_missing_begin_src ()
  (tlp/with-org-buffer "*** config
{\"shortName\": \"tlp\"}
#+END_SRC"
    (should-error (tlp--extract-org-json-src-block))))

(ert-deftest tlp--parse-json ()
  (should (equal (tlp--parse-json "{\"a\": 2}")
                 '((a . 2)))))

(ert-deftest tlp--parse-json-config-json_error ()
  (should-error (tlp--parse-json "{\"a\"}")
                :type 'tlp-config-format))

(ert-deftest tlp--parse-json-config ()
  (tlp/with-org-buffer "* heading :tlp:
** config :tlpConfig:
#+BEGIN_SRC json
{\"shortName\": \"tlp\"}
#+END_SRC"
    (should (equal (tlp--parse-json-config)
                   '((shortName . "tlp"))))))

(ert-deftest tlp--parse-json-config_missing_config ()
  (tlp/with-org-buffer "* heading :tlp:"
    (should-error (tlp--parse-json-config)
                  :type 'tlp-missing-config)))

(ert-deftest tlp--parse-json-config_load_first ()
  (tlp/with-org-buffer "* heading :tlp:
** config :tlpConfig:
#+BEGIN_SRC json
{\"first\": \"first\"}
#+END_SRC

#+BEGIN_SRC json
{\"second\": \"second\"}
#+END_SRC"
    (should (equal (tlp--parse-json-config)
                   '((first . "first"))))))

(ert-deftest tlp-make-config_name ()
  (should (equal (tlp-make-config '((name . "projectName")))
                 (make-instance 'tlp-config-class :name "projectName"))))

(ert-deftest tlp-make-config_all ()
  (should (equal (tlp-make-config '((name . "projectName")
                                    (project-root . "/root/../path")
                                    (repo-branch . "branch")
                                    (layouts . (1))
                                    (global-marks . (2))
                                    (commands . ("cmd"))
                                    (tmux-session . "tmux")))
                 (make-instance 'tlp-config-class
                                :name "projectName"
                                :project-root "/path"
                                :repo-branch "branch"
                                :layouts '(1)
                                :global-marks '(2)
                                :commands '("cmd")
                                :tmux-session "tmux"))))
