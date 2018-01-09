;;; u-ssh.el --- SSH support -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2006-2018 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   10-May-2006

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software",
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; Except as contained in this notice, the name(s of the above copyright
;; holders shall not be used in advertising or otherwise to promote the sale,
;; use or other dealings in this Software without prior written authorization.

;; The software is provided "As Is", without warranty of any kind, express or
;; implied, including but not limited to the warranties of merchantability,
;; fitness for a particular purpose and noninfringement. In no event shall
;; the authors or copyright holders be liable for any claim, damages or other
;; liability, whether in an action of contract, tort or otherwise, arising
;; from, out of or in connection with the software or the use or other
;; dealings in the software.

;;; Commentary:

;; /method:user@remotehost:filename

;; Revision: 29-Feb-2016 Changed from `usr-' to `u-'

;;; Code:

(message "Configuring from u-ssh...")
;;
(require 'ssh)

(defvar u-ssh-alist (list (cons 'pi '((host . "172.16.3.195") (user . "pi")))
                            ))

(defun u-ssh (hostname username)
  "SSH to remote host."
  (let ((ssh-args (concat hostname " -l " username)))
    (ssh ssh-args)))

(defun u-ssh-menu (menu-selection)
  "SSH to menu-slected remote host."
  (interactive)
  (let* ((ssh-list (assq menu-selection u-ssh-alist))
         (hostname (cdr (assq 'host ssh-list)))
         (username (cdr (assq 'user ssh-list))))
    (u-ssh hostname username)))
;;
(message "Loading u-ssh...done")
(provide 'u-ssh)

;;; u-ssh.el ends here
