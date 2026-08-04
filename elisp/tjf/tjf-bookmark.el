;;; tjf-bookmark.el --- Bookmark menu and associated functions -*-lexical-binding: t-*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2017-2026 Tom Fontaine

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

;;; Code:

(message "Loading tjf-bookmark...")
(require 'bm)
(require 'tjf-flags)

;; 
(defvar tjf:bookmark/menu)
(setq tjf:bookmark/menu
  '("Bookmarks"
    ["Save Bookmarks"    bm-buffer-save t]
    ["Toggle Bookmark"   bm-toggle            :enable (tjf:flags/is-not-shell-mode?)]
    ["Bookmark Function" tjf:bookmark/defun   :enable (tjf:flags/is-not-shell-mode?)]
    ["Annotate Bookmark" bm-bookmark-annotate :enable (tjf:flags/is-bookmark?)]
    ["---" nil :visible t :enable nil]
    ["Show All Bookmarks"       bm-show-all t]
    ["Show Local Bookmarks"     bm-show     t]
    ["Show Bookmark Annotation" bm-bookmark-show-annotation :enable (tjf:flags/is-bookmark?)]
    ))

(defun tjf:bookmark/defun ()
  "Drops a temporary breadcrumb/bookmark at the beginning of the current defun."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (bm-toggle)))

(defun tjf:bookmark/show-goto-bookmark nil
  "Goto the bookmark on current line in the `bm-show-buffer-name' buffer."
  (interactive)
  (let ((buffer-name (get-text-property (point) 'bm-buffer))
        (bookmark
         (get-text-property (point) 'bm-bookmark)))
    (if (null buffer-name)
        (message "No bookmark at this line.")
      (pop-to-buffer (get-buffer buffer-name) t)
      (bm-goto bookmark))))

;;
(message "Loading tjf-bookmark...done")
(provide 'tjf-bookmark)

;;; tjf-bookmark.el ends here
