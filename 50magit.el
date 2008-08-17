;;; Autoloads for magit

(autoload 'magit-status "magit" nil t)
(autoload 'magit-minor-mode-find-file-hook "magit-mode" nil t)
(add-hook 'find-file-hook 'magit-minor-mode-find-file-hook)

