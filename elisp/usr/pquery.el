;;; pquery.el --- Perl query-replace -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2001-2020 Tom Fontaine

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

;; Revision:
;;

;;; Code:

(message "Loading pquery...")
;;
(defcustom query-replace-opt-history-variable 'query-replace-history
  "Perl replace option variable."
  :group 'matching
  :type 'symbol
  :version "20.3")

(defun pquery-replace-read-args (string)
  "Prompts user for query-replace arguments."
  (let (from to opt)
    (if query-replace-interactive
        (setq from (car search-ring))
      (setq from (read-from-minibuffer (format "%s: " string)
                                       nil nil nil
                                       query-replace-from-history-variable
                                       nil t)))
    (setq to (read-from-minibuffer (format "%s %s with: " string from)
                                   nil nil nil
                                   query-replace-to-history-variable from t))
    (setq opt (read-from-minibuffer (format "%s %s with: %s using: " string from to)
                                    nil nil nil
                                    query-replace-opt-history-variable from t))
    (if (and transient-mark-mode mark-active)
        (list from to opt (region-beginning) (region-end))
      (list from to opt nil nil))))

(defun pquery-replace (from-string to-string opt-string &optional start end)
  "Perl `query-replace' function replace FROM-STRING with TO-STRING using
OPT-STRING, optionally using START and END as region bounds."
  (interactive (pquery-replace-read-args "Perl query replace"))
  (setq start (or start (point-min))
        end   (or end   (point-max)))
    (call-process-region start end "perl" t t t "-pe" (concat "s/" from-string "/" to-string "/" opt-string)))
;;
(message "Loading pquery...done")
(provide 'pquery)

;;; pquery.el ends here
