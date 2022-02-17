;;; power-base-devel.el -*- lexical-binding: t; -*-

;; Desired packages
(straight-use-package 'modern-cpp-font-lock)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-treemacs)
(straight-use-package 'rustic)
(straight-use-package 'lsp-pyright)

;; Configuration
(add-hook #'c-mode-common
          (lambda () (c-set-style "stroustrup")))
(add-hook #'python-mode
          (lambda () (require 'lsp-pyright)))
(setq lsp-session-file (concat power-cache-dir "lsp-session-v1")
      lsp-server-install-dir (concat power-cache-dir "lsp-server-install/"))
(add-hook #'after-init-hook
          (lambda ()
            (modern-c++-font-lock-global-mode t)))
(require 'python)
(when (and (executable-find "python3")
           (string= python-shell-interpreter "python"))
  (setq python-shell-interpreter "python3"))

(provide 'power-base-devel)
