;;; power-scripting.el -*- lexical-binding: t; -*-

(use-package python
  :ensure nil
  :config
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3")))

(use-package lsp-pyright
  :after lsp
  :defer t
  :hook (python-mode . (lambda () (require 'lsp-pyright))))

(use-package pyvenv)


(provide 'power-scripting)
