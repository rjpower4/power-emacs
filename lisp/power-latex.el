;;; power-latex.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code

(use-package pdf-tools-install
  :defer t
  :ensure pdf-tools
  :if (not IS-WINDOWS)
  :mode "\\.pdf\\'"
  :commands (pdf-loader-install)
  :custom
  (TeX-view-program-selection '((output-pdf "pdf-tools")))
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (pdf-loader-install))
  

(use-package tex
  :defer t
  :ensure auctex
  :bind (:map TeX-mode-map
              ("C-c C-o" . TeX-recenter-output-buffer)
              ("C-c C-l" . TeX-next-error)
              ("M-[" . outline-previous-heading)
              ("M-]" . outline-next-heading))
  :preface
  (defun power-switch-to-help-window (&optional ARG REPARSE)
    "Switches to the *TeX HelP* buffer after compxilation."
    (other-window 1))
  :custom
  (TeX-auto-save t)
  (TeX-byte-compile t)
  (TeX-clean-confirm nil)
  (TeX-fontify-script nil)
  (font-latex-fontify-script nil)
  (TeX-master 'dwim)
  (TeX-parse-self t)
  (TeX-PDF-mode t)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  :config
  (advice-add 'TeX-next-error :after #'power-switch-to-help-window)
  (advice-add 'TeX-recenter-output-buffer :after #'power-switch-to-help-window)
  ;; the ":hook" doesn't work for this one... don't ask me why.
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package reftex
  :defer t
  :ensure nil
  :hook (LaTeX-mode . reftex-mode)
  :custom
  (reftex-save-parse-info t)
  (reftex-use-multiple-selection-buffers t)
  :config
  (setq reftex-cite-format
        '((?a . "\\autocite[]{%l}")
          (?b . "\\blockcquote[]{%l}{}")
          (?c . "\\cite[]{%l}")
          (?f . "\\footcite[]{%l}")
          (?n . "\\nocite{%l}")
          (?p . "\\parencite[]{%l}")
          (?s . "\\smartcite[]{%l}")
          (?t . "\\textcite[]{%l}"))
        reftex-plug-into-AUCTeX t
        reftex-toc-split-windows-fraction 0.3)
  (add-hook 'reftex-toc-mode-hook
            (lambda () (reftex-toc-rescan))))

(use-package bibtex
  :defer t
  :after auctex
  :hook (bibtex-mode . power-bibtex-fill-column)
  :preface
  (defun power-bibtex-fill-column ()
    "Ensures that each entry does not exceed 120 characters."
    (setq fill-column 120)))

(use-package company-auctex
  :defer t
  :after (auctex company)
  :config (company-auctex-init))

(use-package company-math :after (auctex company))
(use-package company-reftex
  :defer t)

(use-package latex-preview-pane
  :defer t
  :config
  (latex-preview-pane-enable))

(use-package preview
  :defer t
  :ensure nil
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale))))


(provide 'power-latex)
;;; power-latex.el ends here
