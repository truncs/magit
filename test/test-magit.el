;;; test-magit.el --- Tests for magit.

;; Licensed under the same terms as magit.

(local-set-key (kbd "C-c e") (lambda () (interactive)
                               (save-buffer)
                               (eval-buffer)
                               (ert-run-tests-interactively "magit")))

(add-to-list 'load-path (file-name-directory (or (buffer-file-name)
                                                 load-file-name)))
(require 'ert)
(load "../magit")

(defvar magit-test-root
  (concat (file-name-directory (or (buffer-file-name) load-file-name))
          "test-repo/")
  "Location of repository used for magit tests.")

(defun magit-initialize-repo ()
  "Create the repository and populate it with a few files."
  (shell-command (concat "rm -rf " magit-test-root))
  (shell-command (format "mkdir -p %s && cd %s && git init"
                         magit-test-root magit-test-root))
  (cd magit-test-root)
  (save-excursion
    (dolist (f '("alpha" "beta" "gamma" "delta"))
      (find-file (concat magit-test-root f))
      (insert f)
      (save-buffer)
      (kill-buffer nil))
    (shell-command (format "cd %s && git add . && git commit -m \"Initialized repository.\""
                           magit-test-root))))

(defmacro magit-deftest (name &rest body)
  `(ert-deftest ,name ()
                (magit-initialize-repo)
                (magit-status magit-test-root)
                ,@body
                (kill-buffer "*magit: test-repo*")))

(magit-deftest magit-status
               (should (equal (buffer-name) "*magit: test-repo*"))
               (should (string-match "^Local: +master.*/magit/test/test-repo/$"
                                     (buffer-substring (point-min) (point-max))))
               (should (string-match "^Head: +[a-z0-9]+\.\.\. Initialized repository\.$"
                                     (buffer-substring (point-min) (point-max)))))

(magit-deftest magit-auto-update
               (save-excursion
                 (let ((magit-auto-update t))
                   (find-file (concat magit-test-root "alpha"))
                   (insert "ALPHA")
                   (save-buffer)
                   ;; TODO: why isn't this happening already?
                   (magit-maybe-update-status)
                   (kill-buffer nil))
                 (switch-to-buffer "*magit: test-repo*")
                 (goto-char (point-min))
                 (should (search-forward "Changes"))))

(provide 'test-magit)