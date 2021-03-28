;;; power-themes.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Icons
(use-package all-the-icons)

;; External theme packages
(use-package color-theme-sanityinc-solarized)
(use-package color-theme-sanityinc-tomorrow)
(use-package modus-themes
  :config
  (setq modus-themes-no-mixed-fonts t
        modus-themes-org-blocks 'grayscale
        modus-themes-slanted-constructs t))

(defvar power-light-theme 'modus-operandi)
(defvar power-dark-theme  'modus-vivendi)
(load-theme power-dark-theme t)

(defvar power-quick-switch-themes
  (let ((themes-list
         (list power-dark-theme
               power-light-theme)))
    (nconc themes-list themes-list)))

(defun power/switch-theme ()
  "Switch between themes."
  (interactive)
  (if-let* ((next-theme (cadr power-quick-switch-themes)))
      (progn
        (consult-theme next-theme)))
  (setq power-quick-switch-themes (cdr power-quick-switch-themes)))

(provide 'power-themes)
;;; power-themes.el ends here
