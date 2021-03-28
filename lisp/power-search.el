;;; power-search.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package selectrum
  :config
  (add-hook 'after-init-hook 'selectrum-mode)
  ;; Always show full height so that words don't move
  (setq selectrum-fix-vertical-window-height t))

(use-package selectrum-prescient
  :config
  (prescient-persist-mode +1)
  (selectrum-prescient-mode +1))

(use-package consult
  :config
  (setq-default consult-project-root-function 'projectile-project-root)
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame))

(use-package marginalia
  :init
  (marginalia-mode)

  ;; Refresh Selectrum on marginalia cycle
  (advice-add #'marginalia-cycle :after
              (lambda ()
                (when (bound-and-true-p selectrum-mode)
                  (selectrum-exhibit 'keep-selected))))

  ;; Show Docs
  (setq marginalia-annotators
        '(marginalia-annotators-heavy
          marginalia-annotators-light
          nil)))

;; (use-package ivy
;;   :diminish
;;   :init
;;   (use-package amx :defer t)
;;   (use-package counsel
;;     :diminish
;;     :config
;;     (counsel-mode 1))
;;   (use-package swiper :defer t)
;;   :custom
;;   (ivy-use-virtual-buffers t)
;;   (ivy-height 10)
;;   (ivy-on-del-err-function nil)
;;   (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
;;   (ivy-wrap t))

(provide 'power-search)
;;; power-search.el ends here
