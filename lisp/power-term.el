;;; power-term.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package vterm
  :defer t)

(use-package eterm-256color
  :defer t
  :ensure t
  :config
  (add-hook 'term-mode-hook #'eterm-256color-mode)
  (add-hook 'term-mode-hook (lambda ()
                                    (setq-local global-hl-line-mode
                                                nil))))

(provide 'power-term)
;;; power-term.el ends here
