;;; power-lsp.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package lsp-mode
  :defer t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil)              ; use flycheck
  (lsp-file-watch-threshold 1000)
  (read-process-output-max (* 1024 1024))
  (lsp-eldoc-hook nil)
  :general
  (:keymaps 'lsp-mode-map
            "C-c C-f" 'lsp-format-buffer)
  :hook
  ((java-mode python-mode c-mode c++-mode latex-mode) . lsp))


(provide 'power-lsp)
;;; power-lsp.el ends here
