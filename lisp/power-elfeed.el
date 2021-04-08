;;; power-elfeed.el --- -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

(use-package elfeed)

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files
        `(,(expand-file-name "elfeed.org" org-directory))))

(provide 'power-elfeed)
;;; power-elfeed.el ends here
