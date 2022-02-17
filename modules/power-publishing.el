;;; power-publishing.el -*- lexical-binding: t -*-

;; Desired packages
(straight-use-package 'auctex)
(straight-use-package 'bibtex)
(straight-use-package 'pdf-tools)
(straight-use-package 'graphviz-dot-mode)

;; Configuration
(setq reftex-plug-intoAUCTex t
      reftex-save-parse-info t
      reftex-use-multiple-selection-buffers t
      reftex-cite-format '((?a . "\\autocite[]{%l}")
                           (?b . "\\blockcquote[]{%l}{}")
                           (?c . "\\cite[]{%l}")
                           (?f . "\\footcite[]{%l}")
                           (?n . "\\nocite{%l}")
                           (?p . "\\parencite[]{%l}")
                           (?s . "\\smartcite[]{%l}")
                           (?t . "\\textcite[]{%l}"))
      reftex-toc-split-windows-fraction 0.3)
(add-hook #'LaTeX-mode #'reftex-mode)
(add-hook #'reftex-toc-mode-hook (lambda () (reftex-toc-rescan)))
(setq TeX-auto-save t
      TeX-byte-compile t
      TeX-clean-confirm nil
      TeX-fontify-script nil
      font-latex-fontify-script nil
      font-latex-fontify-sectioning 'color
      font-latex-fontify-script nil
      TeX-master 'dwim
      TeX-parse-self t
      TeX-PDF-mode t
      TeX-source-correlate-mode t
      TeX-source-correlate-start-server t)
(advice-add 'TeX-next-error :after #'power-switch-to-help-window)
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(add-hook #'LaTeX-mode #'LaTeX-preview-setup)
(setq-default preview-scale 1.4
              preview-scale-function
              (lambda () (* (/ 10.0 (preview-document-pt))  preview-scale)))
(setq pdf-view-use-scaling t
      pdf-view-use-imagemagick nil
      pdf-annot-activate-created-annotations t)
 ;;    :diminish (pdf-view-themed-minor-mode
 ;;               pdf-view-midnight-minor-mode
 ;;               pdf-view-printer-minor-mode)
 ;;    :defines pdf-annot-activate-created-annotations
 ;;    :functions (my-pdf-view-set-midnight-colors my-pdf-view-set-dark-theme)
 ;;    :hook ((pdf-tools-enabled . pdf-view-themed-minor-mode)
 ;;           (pdf-tools-enabled . pdf-view-auto-slice-minor-mode)
 ;;           (pdf-tools-enabled . pdf-isearch-minor-mode))
 ;;    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
 ;;    :magic ("%PDF" . pdf-view-mode)
 ;;    :bind (:map pdf-view-mode-map
 ;;           ("C-s" . isearch-forward))
 ;;    :init
 ;;    (setq pdf-view-use-scaling t
 ;;          pdf-view-use-imagemagick nil
 ;;          pdf-annot-activate-created-annotations t)
 ;;    :config
 ;;    ;; Activate the package
;;    (pdf-tools-install t nil t nil))
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
(add-hook #'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)
(setq graphviz-dot-indent-width 4)

;; Keybinding
(general-define-key
 :keymaps 'TeX-mode-map
 "C-c c-o" #'TeX-recenter-output-buffer
 "C-c c-l" #'TeX-next-error
 "M-[" #'outline-previous-heading
 "M-]" #'outline-next-heading)


;; (use-package company-auctex
;;   :defer t
;;   :after (auctex company)
;;   :config (company-auctex-init))

;; (use-package company-math
;;   :after (auctex company))

;; (use-package company-reftex
;;   :defer t
;;   :after (auctex company))

;; (use-package latex-preview-pane
;;   :defer t
;;   :config
;;   (latex-preview-pane-enable))

(provide 'power-publishing)
;;; power-publishing.el ends here
