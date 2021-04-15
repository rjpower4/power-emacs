;;; power-company.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package company
  :diminish company-mode
  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
  :custom
  ;; Keep it to current mode
  (company-dabbrev-other-buffers t)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode eaf-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0.1)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  :config
  (global-company-mode 1))

(use-package company-tabnine
  :defer t
  :config
  (setq company-idle-delay 0))

(provide 'power-company)
;;; power-company.el ends here
