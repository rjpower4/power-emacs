;;; power-irc.el --- -*- lexical binding: t -*-

;;; Commentary:

;;; Code:


(use-package circe
  :defer t
  :config
  (require 'auth-source)
  (auth-source-pass-enable)
  (defun power/pass-get-password (entry)
    "Get the password for `entry'"
    (let ((d (auth-source-pass-parse-entry entry)))
      (alist-get 'secret d)))

  (defun power/pass-get-user (entry)
    "Get the value associated with 'username' in `entry'"
    (let ((d (auth-source-pass-parse-entry entry)))
      (cdr (assoc "username" d))))

  (setq circe-network-options
        `(("Freenode"
           :tls t
           :port 6697
           :nick "rjpower4"
           :host "chat.freenode.net"
           :sasl-external t
           :nickserv-password (lambda (&rest _)
                                (power/pass-get-password "irc/freenode.net"))
           :channels (:after-auth "#emacs" "#fedora"))
          ("Geekshed"
           :tls t
           :port 6697
           :nickserv-password (lambda (&rest _)
                            (power/pass-get-password "irc/freenode.net"))))))

(provide 'power-irc)
