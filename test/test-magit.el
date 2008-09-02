;;; test-magit.el --- Tests for magit.

;; Licensed under the same terms as magit.

(add-to-list 'load-path (file-name-directory (or (buffer-file-name)
                                                 load-file-name)))
(require 'ert)

(defvar magit-test-root
  (concat (file-name-directory (or (buffer-file-name) load-file-name))
          "test-repo/")
  "Location of repository used for magit tests.")

(defun magit-initialize-repo ()
  "Create the repository and populate it with a few files."
  (shell-command (format "mkdir -p %s && cd %s && git init"
                         magit-test-root magit-test-root))
  (cd magit-test-root)
  (save-excursion
    (dolist (f '("alpha" "beta" "gamma" "delta"))
      (find-file (concat magit-test-root f))
      (insert f)
      (save-buffer)
      (kill-buffer nil)
      (shell-command (concat "git add " f)))
    (shell-command "git commit -m \"Initialized repository.\"")))

(defmacro magit-deftest (name &rest body)
  `(ert-deftest ,name ()
                (magit-initialize-repo)
                (magit-status magit-test-root)
                (unwind-protect
                    (progn ,@body)
                  (kill-buffer "*magit: test-repo*")
                  (shell-command (concat "rm -rf " magit-test-root)))))

(magit-deftest magit-status
               (should (equal (buffer-name) "*magit: test-repo*")))

(provide 'test-magit)