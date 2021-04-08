;;; power-movement.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Ace Window
(use-package ace-window
  :general
  ("C-x C-o" 'ace-window)
  :custom
  (aw-scope 'frame))

;; Avy
(use-package avy
  :general
  ("C-; c" 'avy-goto-char-2)
  ("C-; l" 'avy-goto-line)
  ("C-; w" 'avy-goto-word-1)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-background nil)
  (avy-timeout-seconds 0.3))

(defun power/switch-to-buffer (buffer)
  "Display BUFFER in the selected window or, 
if BUFFER is displayed in an existing window, select that window instead."
  (interactive
   (list (get-buffer (read-buffer
                      "Switch to buffer: "
                      (other-buffer (current-buffer))))))
  (if-let ((win (get-buffer-window buffer t)))
      (let ((nframe (window-frame win)))
        (select-frame-set-input-focus nframe)
        (make-frame-visible nframe)
        (select-window win))
    (switch-to-buffer buffer)))

(provide 'power-movement)
;;; power-movement.el ends here
