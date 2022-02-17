;;; power-filesystem.el -*- lexical-binding: t; -*-

;; Desired packages
(straight-use-package 'all-the-icons-dired)
(if IS-LINUX
    (straight-use-package 'disk-usage))

;; Configuration
(add-hook 'dired-mode #'all-the-icons-dired-mode)

(provide 'power-filesystem)
