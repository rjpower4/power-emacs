;;; power-python.el --- -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

;; For editing pip requirements
(use-package pip-requirements)

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook
            (lambda () (unless (file-remote-p default-directory)
                         (anaconda-mode 1))))
  (add-hook 'anaconda-mode-hook
            (lambda ()
              (anaconda-eldoc-mode (if anaconda-mode 1 0))))
  (use-package company-anaconda
    :config
    (add-to-list 'company-backends 'company-anaconda)))




(provide 'power-python)
;;; power-python.el ends here
