;;; power-clojure.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package clojure-mode
  :defer t
  :config
  (add-hook 'clojure-mode #'smartparens-strict-mode)
  (use-package aggressive-indent
    :config
    (add-hook 'clojure-mode #'aggressive-indent-mode))
  (use-package clojure-mode-extra-font-locking))

(use-package cider
  :defer t
  :diminish subword-mode
  :config
  (add-hook 'cider-repl-mode-hook #'subword-mode)
  (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-file (no-littering-expand-var-file-name "cider-history"))
  (setq cider-save-file-on-load t)
  (setq cider-font-lock-dynamically '(macro core function var)))

(use-package clj-refactor
  :defer t
  :config
  (add-hook 'clojure-mode-hook #'clj-refactor-mode))

(provide 'power-clojure)
;;; power-clojure.el ends here
