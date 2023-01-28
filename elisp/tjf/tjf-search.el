;;; tjf-search.el --- Search menu and associated functions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2023 Tom Fontaine

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

;; Revision: 16-Nov-2016 Fixed bug with `buffer-list'
;;           03-Feb-2021 ‘tjf’ overhaul
;;           02-Jul-2021 Added ‘tjf:search/add-to-isearch-search-ring’
;;

;;; Code:

(message "Loading tjf-search...")
(require 'mapreplace)
(require 'tjf-flags)
(require 'tjf-query-replace)

(eval-when-compile
  (require 'tjf-macro))

;;
(defalias 'search-word-forward  'tjf:search/forward-word-at-point)
(defalias 'search-word-backward 'tjf:search/backward-word-at-point)

(defun tjf:search/all-files ()
  "Find matching text in all open files (i.e. `multi-occur') for text at point or region."
  (interactive)
  (multi-occur (buffer-list) (tjf:search/thing)))

(defun tjf:search/buffer ()
  "Find matching text (i.e. `occur') for text at point or region."
  (interactive)
  (occur (tjf:search/thing)))

(defun tjf:search/current-word-or-region ()
  "Perform an `isearch' on the current word or text selection."
  (interactive)
  (with-word-or-region (beg end)
                       (setq mark-active nil)
                       (when (< beg (point))
                         (goto-char beg))
                       (isearch-mode t)
                       (isearch-yank-string (buffer-substring-no-properties beg end))))

(defun tjf:search/multi-occur ()
  "Call `multi-occur', but use `tjf:search/thing' for regexp default and `buffer-list' for buffers."
  (interactive)
  (multi-occur (buffer-list) (tjf:search/prompt)))

(defun tjf:search/occur ()
  "Call `occur', but use `tjf:search/thing' for regexp default."
  (interactive)
  (occur (tjf:search/prompt)))

(defun tjf:search/prompt ()
  "Prompt for REGEXP to match lines for `u-occur' or `u-multi-occur'."
  (read-regexp "Show lines matching regular expression" (tjf:search/thing) nil))

(defun tjf:search/thing ()
  "Return the region as a string or the `thing-at-point'."
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (or (thing-at-point 'symbol)
        (car regexp-history))))

(defun tjf:search/word-at-point ()
  "Return word (no properties) at point, otherwise return nil."
  (when (looking-at-word-or-symbol)
      (buffer-substring-no-properties (word-beginning-position) (word-end-position))))

(defun tjf:search/match-word (word direction)
  "Find the next match for WORD in DIRECTION."
  (let ((regexp (concat "[^[:word:]]" word "[^[:word:]]")))
    (if (eq direction 'forward)
        (save-excursion
          (search-forward-regexp regexp (point-max) t))
      (save-excursion
          (search-backward-regexp regexp (point-min) t)))))

(defun tjf:search/goto-word-at-point (direction)
  "Search in DIRECTION for the word at point."
  (if (looking-at-word-or-symbol)
      (let ((word (tjf:search/word-at-point)))
        (if (tjf:search/match-word word direction)
            (goto-char (+ 1 (match-beginning 0)))
          (error (concat " ‘" word "’ not found")))
        (tjf:search/add-to-isearch-search-ring word))
    (error "Word not grabbed")))

(defun tjf:search/forward-word-at-point ()
  "Search for the word at point."
  (interactive)
  (tjf:search/goto-word-at-point 'forward))

(defun tjf:search/backward-word-at-point ()
  "Search for the word at point."
  (interactive)
  (tjf:search/goto-word-at-point 'backward))

(defun tjf:search/add-to-isearch-search-ring (isearch-string)
  "Add ISEARCH-STRING to isearch ‘search-ring’."
  (if (> (length isearch-string) 0)
      ;; Update the ring data.
      (if isearch-regexp
          (if (or (null regexp-search-ring)
                  (not (string= isearch-string (car regexp-search-ring))))
              (progn
                (setq regexp-search-ring
                      (cons isearch-string regexp-search-ring))
                (if (> (length regexp-search-ring) regexp-search-ring-max)
                    (setcdr (nthcdr (1- search-ring-max) regexp-search-ring)
                            nil))))
        (if (or (null search-ring)
                (not (string= isearch-string (car search-ring))))
            (progn
              (setq search-ring (cons isearch-string search-ring))
              (if (> (length search-ring) search-ring-max)
                  (setcdr (nthcdr (1- search-ring-max) search-ring) nil)))))))

(defvar tjf:search/menu
  '("Search"
    ["Find..."                     tjf:search/occur       :active t]
    ["Find (All Files)..."         tjf:search/multi-occur :active t]
    "---"
    ["Map Replace"        query-mapreplace        :enable (tjf:flags/enable-write?)]
    ["Map Replace Regexp" query-mapreplace-regexp :enable (tjf:flags/enable-write?)]
    "---"
    ["Repeat Search Backward"        nonincremental-repeat-search-backward    :active t]
    ["Repeat Search Forward"         nonincremental-repeat-search-forward     :active t]
    ["Repeat Search Regexp Backward" nonincremental-repeat-re-search-backward :active t]
    ["Repeat Search Regexp Forward"  nonincremental-repeat-re-search-forward  :active t]
    "---"
    ["Replace..."        query-replace        :enable (tjf:flags/enable-write?)]
    ["Replace Regexp..." query-replace-regexp :enable (tjf:flags/enable-write?)]
    ["Perl Replace..."   tjf:query-replace/do :enable (tjf:flags/enable-write?)]
    "---"
    ["Search Backward..."        search-backward        :active t]
    ["Search Forward..."         search-forward         :active t]
    ["Search Regexp Backward..." search-backward-regexp :active t]
    ["Search Regexp Forward..."  search-forward-regexp  :active t]
    ))

;;
(message "Loading tjf-search...done")
(provide 'tjf-search)

;;; tjf-search.el ends here
