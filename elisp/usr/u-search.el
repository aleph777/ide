;;; u-search.el --- Search menu and associated functions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2021 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   25-Feb-2016

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

;; Revision:    16-Nov-2016 Fixed bug with `buffer-list'
;;

;;; Code:

(message "Loading u-search...")
(require 'u-flags)
;;
(defvar u-search-menu
  '("Search"
    ["Find..."                     u-occur       :active t]
    ["Find (All Files)..."         u-multi-occur :active t]
    "---"
    ["Search Forward..."         search-forward         :active t]
    ["Search Backward..."        search-backward        :active t]
    ["Search Regexp Forward..."  search-forward-regexp  :active t]
    ["Search Regexp Backward..." search-backward-regexp :active t]
    "---"
    ["Repeat Search Forward"         nonincremental-repeat-search-forward     :active t]
    ["Repeat Search Backward"        nonincremental-repeat-search-backward    :active t]
    ["Repeat Search Regexp Forward"  nonincremental-repeat-re-search-forward  :active t]
    ["Repeat Search Regexp Backward" nonincremental-repeat-re-search-backward :active t]
    "---"
    ["Replace..."        query-replace        :enable (is-rw?)]
    ["Replace Regexp..." query-replace-regexp :enable (is-rw?)]
    ["Perl Replace..."   pquery-replace       :enable (is-rw?)]
    "---"
    ["Map Replace"        query-mapreplace        :enable (is-rw?)]
    ["Map Replace Regexp" query-mapreplace-regexp :enable (is-rw?)]
    ))

(defun xah-search-current-word ()
  "Perform an `isearch' on the current word or text selection.
URL `http://ergoemacs.org/emacs/modernization_isearch.html' Version 2015-04-09"
  (interactive)
  (let ( ξp1 ξp2 )
    (if (use-region-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp2 (region-end)))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq ξp1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq ξp2 (point))))
    (setq mark-active nil)
    (when (< ξp1 (point))
      (goto-char ξp1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties ξp1 ξp2))))

(defun u-search-thing ()
  "Return the region as a string or the `thing-at-point'."
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (or (thing-at-point 'symbol)
        (car regexp-history))))

(defun u-search-prompt ()
  "Prompt for REGEXP to match liines for `u-occur' or `u-multi-occur'."
  (read-regexp "Show lines matching regular expression" (u-search-thing) nil))

(defun u-occur ()
  "Call `occur', but use `u-search-thing' for regexp default."
  (interactive)
  (occur (u-search-prompt)))

(defun u-multi-occur ()
  "Call `multi-occur', but use `u-search-thing' for regexp default and `buffer-list' for buffers."
  (interactive)
  (multi-occur (buffer-list) (u-search-prompt)))
  
(defun u-search-buffer ()
  "Find matching text (i.e. `occur') for text at point or region."
  (interactive)
  (occur (u-search-thing)))

(defun u-search-all-files ()
  "Find matching text in all open files (i.e. `multi-occur') for text at point or region."
  (interactive)
  (multi-occur (buffer-list) (u-search-thing)))
;;
(message "Loading u-search...done")
(provide 'u-search)

;;; u-search.el ends here
