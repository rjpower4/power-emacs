;;; power-crux.el --- -*- lexical-binding: t -*-

;;; Commentary

;;; Code:

;; Collection of Ridiculously Useful eXtensions
(use-package crux
  :after general
  :general
  ("C-a" 'crux-move-beginning-of-line)
  ("<C-M-return>" 'crux-smart-open-line-above)
  ("<M-return>" 'crux-smart-open-line)
  ("C-x K" 'crux-kill-other-buffers)
  ("C-k" 'crux-smart-kill-line)
  :config
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(provide 'power-crux)
;;; power-crux.el ends here
