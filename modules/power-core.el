;;; power-core.el -*- lexical-binding:  t; -*-

;; Desired packages
(straight-use-package 'diminish)
(straight-use-package 'general)

;; Diminished
(diminish 'eldoc-mode)

;; ========================================================================================
;;; Mode Hiding Utility
;; ========================================================================================
;;(use-package diminish)

;; ========================================================================================
;;; Treemacs
;; ========================================================================================
;; (use-package treemacs
;;   :defer t
;;   :general
;;   ("M-[" 'treemacs-select-window)
;;   :init
;;   (setq treemacs-follow-after-init t
;;         treemacs-is-never-other-window t
;;         treemacs-sorting 'alphabetic-case-insensitive-asc
;;         treemacs-persist-file (concat power-cache-dir "treemacs-last-error-persist")
;;         treemacs-git-mode 'simple)
;;   :config
;;   (treemacs-follow-mode -1))

;; ========================================================================================
;;; Crux
;; ========================================================================================
;; (use-package crux
;;   :config
  ;; (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

;; ========================================================================================
;;; Company
;; ========================================================================================
;; (use-package company
;;   :diminish company-mode
;;   :hook ((LaTeX-mode prog-mode) . company-mode)
;;   :custom
;;   (company-tooltip-limit 20)
;;   (company-show-numbers t)
;;   (company-idle-delay 0)
;;   (company-echo-delay 0)
;;   (company-tooltip-align-annotations t)
;;   (company-require-match 'never)
;;   (company-global-modes '(not shell-mode eaf-mode)))

;; (use-package company-tabnine
;;   :defer t)

;; ========================================================================================
;;; Flycheck
;; ========================================================================================
;; (use-package flycheck
;;   :defer t
;;   :diminish
;;   :hook
;;   (after-init . global-flycheck-mode)
;;   :commands (flycheck-add-mode)
;;   :custom
;;   (flycheck-global-modes '(python-mode latex-mode))
;;   (flycheck-emacs-lisp-load-path 'inherit)
;;   (flycheck-indication-mode (if (display-graphic-p) 'right-fringe
;;                               'right-margin)))

;; (use-package flycheck-color-mode-line
;;   :defer t
;;   :config 
;;   (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))



;; ========================================================================================
;;; PDF
;; ========================================================================================
 ;; (use-package pdf-view
 ;;    :ensure pdf-tools
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

(provide 'power-core)
;;; power-core.el ends here
