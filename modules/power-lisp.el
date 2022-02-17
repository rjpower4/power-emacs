;;; power-lisp.el -*- lexical-binding: t; -*-

;; Desired packages
(straight-use-package 'clojure-mode)
(straight-use-package 'cider)
(straight-use-package 'clojure-mode-extra-font-locking)
(straight-use-package 'slime)

;; Configuration
(add-hook 'cider-repl-mode-hook #'subword-mode)
;;(add-hook 'cider-repl-mode-hook #'company-mode)
(setq nrepl-hide-special-buffers t
      nrepl-log-messages nil
      cider-font-lock-dynamically '(macro core function var deprecated)
      cider-overlays-use-font-lock t
      cider-prompt-for-symbol nil
      cider-repl-history-display-duplicates nil
      cider-repl-history-display-stile 'one-line
      cider-repl-history-file (concat power-cache-dir "cider-repl-history")
      cider-repl-history-highlight-current-entry t
      cider-repl-history-quit-action 'delete-and-restore
      cider-repl-history-highlight-inserted-item t
      cider-repl-history-size 1000
      cider-repl-result-prefix ";; => "
      cider-repl-print-length 100
      cider-repl-user-clojure-font-lock t
      cider-repl-use-pretty-printing t
      cider-repl-wrap-history nil
      cider-save-file-on-load t
      cider-stacktrace-default-filters '(tooling dup)
      cider-repl-pop-to-buffer-on-connect 'display-only)
(setq inferior-lisp-program "sbcl")

(provide 'power-lisp)
