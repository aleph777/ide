;; tjf-query-replace.el --- Perl query-replace -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2001-2021 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   06-Dec-2001

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

;; Revision: 03-Feb-2021 ‘tjf’ overhaul
;;

;;; Code:

(message "Loading tjf-query-replace...")
(eval-when-compile
  (require 'tjf-macro))

;;
(defcustom tjf:query-replace/opt-history-variable 'query-replace-history
  "Perl replace option variable."
  :group   'matching
  :type    'symbol
  :version "20.3")

(defun tjf:query-replace/read-args (string)
  "Prompt user for `query-replace' arguments from STRING."
  (let (from to opt)
    (setq from (read-from-minibuffer
                (format "%s: " string)
                nil nil nil
                query-replace-from-history-variable
                nil t))
    (setq to (read-from-minibuffer
              (format "%s %s with: " string from)
              nil nil nil
              query-replace-to-history-variable from t))
    (setq opt (read-from-minibuffer
               (format "%s %s with: %s using: " string from to)
               nil nil nil
               tjf:query-replace/opt-history-variable from t))
    (list from to opt)))

(defun tjf:query-replace/do (from-string to-string opt-string)
  "Replace FROM-STRING with TO-STRING using OPT-STRING."
  (interactive (tjf:query-replace/read-args "Perl query replace"))
  (with-buffer-or-region (beg end)
                         (call-process-region beg end "perl" t t t "-pe" (concat "s/" from-string "/" to-string "/" opt-string))))

;;
(message "Loading tjf-query-replace...done")
(provide 'tjf-query-replace)

;;; tjf-query-replace.el ends here
