;; -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2012-2021 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   26-Dec-2012

;; Revision:  29-Feb-2016 Changed from `usr-' to `u-'
;;                        Removed deprecated key bindings in u-eshell-setup
;;
(message "Loading u-eshell...")
(require 'eshell)
;;
(defun u-eshell-kill-line ()
  "Kills the entire contents of the current command line."
  (interactive "*")
  (eshell-bol)
  (let ((pm (point-max))
        (nl (1+ (line-end-position))))
    (kill-region (point) (if (< nl pm) nl pm))))

(defun u-eshell-kill-bol ()
  "Kills to the beginning of the current command line."
  (interactive "*")
  (let ((point-sav (point)))
    (eshell-bol)
    (kill-region (point) point-sav)))

(defun u-eshell-setup ()
  "`eval-after-load' initialization function for eshell-mode."
  (define-key eshell-mode-map [M-mouse-4]    'eshell-previous-prompt)
  (define-key eshell-mode-map [M-wheel-up]   'eshell-previous-prompt)
  (define-key eshell-mode-map [M-mouse-5]    'eshell-next-prompt)
  (define-key eshell-mode-map [M-wheel-down] 'eshell-next-prompt))
;;
(message "Loading u-eshell...done")
(provide 'u-eshell)
