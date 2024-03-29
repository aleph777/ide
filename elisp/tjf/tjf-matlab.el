;;; tjf-matlab.el --- MATLAB major mode support -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2024 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   18-Apr-2016

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
;;           03-Feb-2021 ‘tjf’ overhaul
;;

;;; Code:

(message "Loading tjf-matlab...")
;; (require 'matlab)

;;
(defun tjf:matlab/setup ()
  "MATLAB-mode setup function."
  (matlab-cedet-setup)
  (imenu-add-to-menubar "Navigate")
  (setq comment-start "% " comment-end "")
  (define-key matlab-mode-map [menu-bar] nil))

;;
(setq matlab-imenu-generic-expression '((nil "^function\\(\\s-+[^=\n]+\\s-*=\\s-*\\|\\s-+\\)\\(\\sw+\\)\\s-*(" 2)))

;;
(message "Loading tjf-matlab...done")
(provide 'tjf-matlab)

;;; tjf-matlab.el ends here
