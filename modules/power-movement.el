;;; power-movement.el -*- lexical-binding: t -*-

(use-package ace-window
  :general
;;  ("C-x C-o" 'ace-window)
  :custom
  (aw-scope 'frame))

;; ========================================================================================
;;; Jumping Around
;; ========================================================================================
(use-package avy
  :general
  ("C-; C-;" 'avy-goto-char-timer)
  ("C-; w"   'avy-goto-word-1)
  ("C-; l"   'avy-goto-line)
  :custom
  (aw-keys '(?a ?s ?f ?g ?h ?j ?k ?l))
  (aw-background nil)
  (avy-timeout-seconds 0.3))

(provide 'power-movement)
;;; power-movement.el ends here
