;;; power-julia.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package julia-mode)

(use-package julia-repl
  :config
  (set-language-environment "UTF-8")
  :hook
  (julia-mode . julia-repl-mode))

(provide 'power-julia)
;;; power-julia.el ends here
