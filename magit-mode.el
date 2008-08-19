;;; magit -- control Git from Emacs.

;; Copyright (C) 2008  Marius Vollmer
;;
;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; Minor mode for magit
;; See magit.el for more information
(require 'magit)

(defcustom magit-mode-key-prefix "C-c C-v"
  "Prefix keystrokes for magit minor-mode commands."
  :group 'magit
  :type 'string
  :set 'magit-mode-key-prefix-set)

(defun magit-is-in-git-working-dir (&optional dir)
  (let ((default-directory (or dir default-directory)))
    (= (call-process "git" nil nil nil "rev-parse" "--git-dir")
       0)))

(defun magit-stage-file (file)
  "Stage FILE."
  (magit-run "git" "add" file))

(defun magit-unstage-file (file)
  "UnStage FILE."
  (magit-run "git" "reset" "-q" "HEAD" "--" file))

(defun magit-stage-current-file (&optional unstage)
  "Stage the current file. if UNSTAGE is not nil UnStage intead."
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
   (if unstage
       (magit-run "git" "reset" "-q" "HEAD" "--"
		  (file-name-nondirectory (buffer-file-name)))
     (magit-run "git" "add" (file-name-nondirectory (buffer-file-name)))))

(defun magit-diff-current-file (&optional ask-p)
  "Diff the current file."
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (let* ((rev (if ask-p (magit-read-rev "Diff against: " "HEAD") "Staging"))
	 (file (file-name-nondirectory (buffer-file-name)))
	 (desc (format "%s vs %s" file rev)))
    (if ask-p
	(magit-do-diff desc rev "--" file)
      (magit-do-diff desc "--" file))))

(defun magit-cancel-modifications (&optional from-head)
  "Cancel modifications made to the current file."
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (if from-head 
      (magit-run "git" "checkout" "HEAD" (file-name-nondirectory (buffer-file-name)))
    (magit-run "git" "checkout" (file-name-nondirectory (buffer-file-name)))))

(defun magit-status-other-window (dir)
  (interactive (list (magit-read-top-dir current-prefix-arg)))
  (pop-to-buffer nil)
  (magit-status dir))

(defun magit-current-file-other-version-other-window (&optional ask-p)
  "Show other version of the current file in another window."
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (let* ((mode major-mode)
	 (rev (magit-read-rev "show version: " "master"))
	 (canon-name (car (split-string
			   (shell-command-to-string
			    (concat "git ls-files --full-name -- "
				    (buffer-file-name))))))
	 (git-name (concat rev ":" canon-name))
	 (buf (get-buffer-create (concat "*" git-name "*"))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(erase-buffer)
	(call-process "git" nil buf nil "show" git-name)
	(funcall mode)
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)))
    (pop-to-buffer buf)))

(defvar magit-minor-mode nil)
(defvar magit-minor-mode-map (make-sparse-keymap "MaGit"))
(defvar magit-file-cmd-map (make-sparse-keymap))

(defun magit-mode-key-prefix-set (var val)
  (define-key magit-minor-mode-map (read-kbd-macro val) magit-file-cmd-map)
  (custom-set-default var val))

(let ((map magit-file-cmd-map))
  (define-key map (kbd "v") 'magit-status-other-window)
  (define-key map (kbd "b") 'magit-create-branch)
  (define-key map (kbd "s") 'magit-stage-current-file)
  (define-key map (kbd "=") 'magit-diff-current-file)
  (define-key map (kbd "u") 'magit-cancel-modifications)
  (define-key map (kbd "~") 'magit-current-file-other-version-other-window)
  (define-key map (kbd "c") 'magit-log-edit)
  (define-key map (kbd "l") 'magit-log-head))

;;;###autoload
(defun magit-minor-mode (&optional arg)
  "Turn-on magit-minor-mode which enable key bindings for magit
in current buffer."
  (interactive "p")
  (setq magit-minor-mode (if (null arg)
			     (not magit-minor-mode)
			   (> arg 0)))
  (when magit-minor-mode
    (if (boundp 'vc-mode)
	(set 'vc-mode nil))))

;;;###autoload
(defun magit-minor-mode-find-file-hook ()
  (if (magit-is-in-git-working-dir)
      (magit-minor-mode 1)
    (magit-minor-mode -1)))

(or (assq 'magit-minor-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(magit-minor-mode " MaGit") minor-mode-alist)))

(setcdr (or (assq 'magit-minor-mode minor-mode-map-alist)
	    (car (setq minor-mode-map-alist
		       (cons (list 'magit-minor-mode)
			     minor-mode-map-alist))))
	magit-minor-mode-map)

(if (and (boundp 'vc-handled-backends)
	 (listp (symbol-value 'vc-handled-backends)))
    (set 'vc-handled-backends
	 (delq 'Git (symbol-value 'vc-handled-backends))))




(provide 'magit-mode)
