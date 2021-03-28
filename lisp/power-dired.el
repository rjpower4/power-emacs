;;; power-dired.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package dired
  :ensure nil
  :config
  (use-package diredfl
    :config
    (diredfl-global-mode)
    (require 'dired-x))
  (use-package diff-hl
    :config
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

  ;; Enable auto-reverting
  (global-auto-revert-mode t)

  ;; Reuse the same dired buffer, don't create a bajillion of them
  (put 'dired-find-alternate-file 'disabled nil)

  :custom
  ;; Make it quick
  (dired-dwim-target t)
  
  ;; Ask about recursive deletes/copies only for top level directory
  (dired-recursive-deletes 'top)
  (dired-recursive-copies 'top)

  ;; Auto refresh dired but don't brag about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)

  ;; Separation Anxiety
  (delete-by-moving-to-trash t)

  ;; Get the newest file
  (load-prefer-newer t)

  ;; Again, don't talk to me about it
  (auto-revert-use-notify nil)
  (auto-revert-interval 3)              ; Every 3 seconds

  :hook
  ;; Prefer to use '-' to go up a directory
  (dired-mode . (lambda ()
                  (local-set-key (kbd "-")
                                 (lambda () (interactive)
                                   (find-alternate-file "..")))))

  :general
  (:keymaps 'dired-mode-map
            [mouse-2] 'dired-find-file
            "C-c C-q" 'wdired-change-to-wdired-mode))

(use-package disk-usage
  :commands (disk-usage))

(provide 'power-dired)
;;; power-dired.el ends here
