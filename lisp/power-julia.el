;;; power-julia.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package julia-mode
  :defer t)

(use-package julia-repl
  :defer t
  :config
  (julia-repl-set-terminal-backend 'vterm)
  (set-language-environment "UTF-8")
  (julia-repl-set-terminal-backend 'vterm)
  :hook
  (julia-mode . julia-repl-mode))

(provide 'power-julia)
;;; power-julia.el ends here
