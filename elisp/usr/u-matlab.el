;;; u-matlab.el --- MATLAB major mode support -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;              Copyright © 2016-2017 Tom Fontaine

;;
;; Author:      Tom Fontaine
;; Date:        18-Apr-2016
;; Time-stamp: <18-Jan-2017 12:34:41 EST, modified by Tom Fontaine>
;;

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

;; Revision: 23-Jun-2016 Removed globally set `semantic-mode'
;;           17-Jan-2017 Added call to `matlab-cedet-setup'
;;

;;; Code:

(message "Loading u-matlab...")
;;
(defun matlab-setup ()
  "MATLAB-mode setup function."
  (matlab-cedet-setup)
  (imenu-add-to-menubar "Navigate")
  (setq comment-start "% " comment-end "")
  (define-key matlab-mode-map [menu-bar] nil))

(setq matlab-imenu-generic-expression '((nil "^function\\(\\s-+[^=\n]+\\s-*=\\s-*\\|\\s-+\\)\\(\\sw+\\)\\s-*(" 2)))

(add-hook 'matlab-mode-hook 'matlab-setup)

;;
(message "Loading u-matlab...done")
(provide 'u-matlab)

;;; u-matlab.el ends here