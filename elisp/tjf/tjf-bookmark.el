;;; tjf-bookmark.el --- Bookmark menu and associated functions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2017-2021 Tom Fontaine

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
;;           06-May-2019 replaced ‘bm-save’ with ‘bm-buffer-save’
;;           03-Feb-2021 ‘tjf’ overhaul
;;

;;; Code:

(message "Loading tjf-bookmark...")
(require 'bm)
(require 'tjf-flags)

;;; overload
(with-eval-after-load 'bm
  (defun bm-show-display-lines (header lines)
    "Show bookmarked LINES to the `bm-show-buffer-name' buffer."
    (if (= (length lines) 0)
        (message "No bookmarks defined.")
      (with-output-to-temp-buffer bm-show-buffer-name
        (set-buffer standard-output)
        (insert lines)
        (bm-show-mode)
        ;; Can only set header-line-format after bm-show-mode has called
        ;; kill-all-local-variables.  This use of propertize allows the
        ;; header line to line up with the left fringe, thanks
        ;; http://www.emacswiki.org/emacs/HeaderLine!
        (setq header-line-format
              (concat (propertize " " 'display '((space :align-to 0))) header))
        (setq buffer-read-only t)
        (when bm-electric-show
          (pop-to-buffer (current-buffer))
          (fit-window-to-buffer) ;; tjf
          ))))

  (defun bm-show-extract-bookmarks (&optional lifo-order all)
    "Return (HEADER BOOKMARK-LIST) for displaying a list of bookmarks.
Both are strings to be used in the bookmark lists provided to
users by the likes of `bm-show' and `bm-show-all'."
    ;; format-non-nil is just like format except it ignores any nil
    ;; arguments.  For example, (format-non-nil "%s %s" "foo" nil "bar")
    ;; yields "foo bar".  This is useful below where we conditionally
    ;; omit annotations.
    ;;
    ;; lstrip strips trailing white space from STR.  lstrip was stolen
    ;; from s.el and
    ;; http://ergoemacs.org/emacs/modernization_elisp_lib_problem.html.
    (cl-flet ((format-non-nil (format-string &rest args)
                              (apply #'format format-string (delete nil args)))
              (lstrip (str)
                      (if (string-match "\\`[ \t\n\r]+" str)
                          (replace-match "" t t str)
                        str)))
      (let* ((bookmarks (if lifo-order
                            (bm-overlays-lifo-order all)
                          (if all (bm-overlay-all)
                            (bm-overlay-in-buffer))))
             (file-line-width (bm-find-file-line-max-width bookmarks all))
             (format-string (concat (format "%%-%ds" file-line-width)
                                    (when bm-show-annotations
                                      (format " %%-%ds" bm-annotation-width))
                                    " %s")))
        (list
         ;; The header
         (format-non-nil format-string
                         (if all
                             (format "%s:%s" bm-header-buffer-name
                                     bm-header-line)
                           bm-header-line)
                         (when bm-show-annotations
                           bm-header-annotation)
                         bm-header-contents)
         ;; The bookmark list
         (mapconcat
          #'(lambda (bm)
              (with-current-buffer (overlay-buffer bm)
                (let* ((line (lstrip (buffer-substring (overlay-start bm)
                                                       (overlay-end bm))))
                       ;; line numbers start on 1
                       (line-num (+ 1 (count-lines (point-min) (overlay-start bm))))
                       (string
                        (format-non-nil format-string
                                        (if all
                                            (format "%s:%d" (buffer-name)
                                                    line-num)
                                          line-num)
                                        (when bm-show-annotations
                                          (or (overlay-get bm 'annotation) ""))
                                        (if (string-match "\n$" line)
                                            line
                                          (concat line "\n")))))
                  (put-text-property 0 (length string) 'bm-buffer (buffer-name) string)
                  (put-text-property 0 (length string) 'bm-bookmark bm string)
                  (add-text-properties 0 (length (format "%s:%d" (buffer-name) line-num))
                                       '(font-lock-face bookmark-menu-bookmark
                                                        mouse-face highlight
                                                        follow-link t
                                                        help-echo "mouse-1: go to this bookmark in other window") string) ;; tjf
                  string)))
          bookmarks
          "")))))
  )

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
        (bookmark    (get-text-property (point) 'bm-bookmark)))
    (if (null buffer-name)
        (message "No bookmark at this line.")
      (pop-to-buffer (get-buffer buffer-name) t)
      (bm-goto bookmark))))

(defvar tjf:bookmark/menu
  '("Bookmarks"
    ["Save Bookmarks"    bm-buffer-save t]
    ["Toggle Bookmark"   bm-toggle            :enable (tjf:flags/is-not-shell-mode?)]
    ["Bookmark Function" tjf:bookmark/defun   :enable (tjf:flags/is-not-shell-mode?)]
    ["Annotate Bookmark" bm-bookmark-annotate :enable (tjf:flags/is-bookmark?)]
    "---"
    ["Show All Bookmarks"       bm-show-all t]
    ["Show Local Bookmarks"     bm-show     t]
    ["Show Bookmark Annotation" bm-bookmark-show-annotation :enable (tjf:flags/is-bookmark?)]
    ))

;;
(message "Loading tjf-bookmark...done")
(provide 'tjf-bookmark)

;;; tjf-bookmark.el ends here
