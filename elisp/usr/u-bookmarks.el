;;; u-bookmarks.el --- Bookmark menu and associated functions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2017-2018 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   14-Jan-2017

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

;; Revision: 09-Nov-2017 added ‘bm-save’ to ‘u-bookmarks-menu’
;;

;;; Code:

(message "Loading u-bookmarks...")
;;
(require 'bm)
(require 'u-flags)

(defvar u-bookmarks-menu
  '("Bookmarks"
    ["Save Bookmarks"    bm-save t]
    ["Toggle Bookmark"   bm-toggle            :enable (is-not-shell?)]
    ["Bookmark Function" bm-bookmark-defun    :enable (is-not-shell?)]
    ["Annotate Bookmark" bm-bookmark-annotate :enable (is-bookmark?)]
    "---"
    ["Show All Bookmarks"       bm-show-all t]
    ["Show Local Bookmarks"     bm-show     t]
    ["Show Bookmark Annotation" bm-bookmark-show-annotation :enable (is-bookmark?)]
    ))

(defun bm-bookmark-defun ()
  "Drops a temporary breadcrumb/bookmark at the beginning of the current defun."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (bm-toggle)))

(defun bm-show-goto-bookmark-1 nil
  "Goto the bookmark on current line in the `bm-show-buffer-name' buffer."
  (interactive)
  (let ((buffer-name (get-text-property (point) 'bm-buffer))
        (bookmark (get-text-property (point) 'bm-bookmark)))
    (if (null buffer-name)
        (message "No bookmark at this line.")
      (pop-to-buffer (get-buffer buffer-name) t)
      (bm-goto bookmark))))
;;
(message "Loading u-bookmarks...done")
(provide 'u-bookmarks)

;;; u-bookmarks.el ends here
