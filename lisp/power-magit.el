;;; power-magit.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(provide 'power-magit)
;;; power-magit.el ends here
