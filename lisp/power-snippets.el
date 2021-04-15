;;; power-snippets.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package yasnippet
  :defer 3
  :diminish yas-minor-mode
  :init
  (use-package yasnippet-snippets :after yasnippet)
  :hook ((prog-mode LaTeX-mode) . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key)))


(provide 'power-snippets)
;;; power-snippets.el ends here
