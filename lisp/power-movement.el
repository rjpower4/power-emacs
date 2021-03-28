;;; power-movement.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Ace Window
(use-package ace-window
  :general
  ("C-x C-o" 'ace-window))

;; Avy
(use-package avy
  :general
  ("C-; c" 'avy-goto-char-2)
  ("C-; l" 'avy-goto-line)
  ("C-; w" 'avy-goto-word-1)
  :custom
  (avy-timeout-seconds 0.3))

(provide 'power-movement)
;;; power-movement.el ends here
