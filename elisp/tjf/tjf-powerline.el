;;; tjf-powerline.el --- Powerline setup -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2024 by Tom Fontaine

;; Author: Tom Fontaine
;; Date:   30-Mar-2016

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

;; Revision: 01-Apr-2016 cleaned up do-update-modeline-vars
;;           09-May-2016 added `anzu'
;;           14-Sep-2016 fixed `mode-line-inactive'
;;           18-Sep-2016 moved `alias-face' to `u-macro'
;;           23-May-2017 changed ‘powerline-red-face’ to ‘error’
;;           13-Jun-2018 added ‘require’ for ‘u-flags’
;;           03-Jul-2018 moved ‘powerline-vc’ to rhs face2
;;           10-Jul-2019 added ‘require’ for ‘anzu’
;;           28-Oct-2020 reworked  ‘powerline' changes
;;           03-Feb-2021 ‘tjf’ overhaul
;;           29-Aug-2022 using ‘%C’ for column format
;;           02-Oct-2022 removed ‘tjf:powerline/column’
;;           08-May-2023 added separate region info
;;

;;; Code:

(message "Loading tjf-powerline...")
(require 'anzu)
(require 'powerline)
(require 'tjf-flags)
(require 'tjf-macro)
(require 'tjf-view)

;;
(defvar tjf:powerline/buffer-byte-count nil
  "String value of Buffer byte count.")
(make-variable-buffer-local 'tjf:powerline/bufreg-byte-count)

(defvar tjf:powerline/buffer-line-count nil
  "String value of Buffer line count.")
(make-variable-buffer-local 'tjf:powerline/buffer-line-count)

(defvar tjf:powerline/buffer-word-count nil
  "String value of Buffer word count.")
(make-variable-buffer-local 'tjf:powerline/buffer-word-count)

(defvar tjf:powerline/region-byte-count nil
  "String value of Region byte count.")
(make-variable-buffer-local 'tjf:powerline/region-byte-count)

(defvar tjf:powerline/region-line-count nil
  "String value of Region line count.")
(make-variable-buffer-local 'tjf:powerline/region-line-count)

(defvar tjf:powerline/region-word-count nil
  "String value of Region word count.")
(make-variable-buffer-local 'tjf:powerline/region-word-count)

(defvar tjf:powerline/encoding-alist (list (cons 'prefer-utf-8-unix            "Unix")
                                           (cons 'prefer-utf-8-dos             "DOS")
                                           (cons 'prefer-utf-8-mac             "Mac")
                                           (cons 'undecided-unix               "Unix")
                                           (cons 'undecided-dos                "DOS")
                                           (cons 'undecided-mac                "Mac")
                                           (cons 'utf-8-unix                   "UTF-8 Unix")
                                           (cons 'utf-8-emacs-unix             "UTF-8 Unix")
                                           (cons 'utf-8-dos                    "UTF-8 DOS")
                                           (cons 'utf-8-mac                    "UTF-8 Mac")
                                           (cons 'utf-8-with-signature-unix    "UTF-8 BOM Unix")
                                           (cons 'utf-8-with-signature-dos     "UTF-8 BOM DOS")
                                           (cons 'utf-8-with-signature-mac     "UTF-8 BOM Mac")
                                           (cons 'utf-16be-with-signature-unix "UCS-2 BE BOM Unix")
                                           (cons 'utf-16be-with-signature-dos  "UCS-2 BE BOM DOS")
                                           (cons 'utf-16be-with-signature-mac  "UCS-2 BE BOM Mac")
                                           (cons 'utf-16le-with-signature-unix "UCS-2 LE BOM Unix")
                                           (cons 'utf-16le-with-signature-dos  "UCS-2 LE BOM DOS")
                                           (cons 'utf-16le-with-signature-mac  "UCS-2 LE BOM Mac")))

(defvar tjf:powerline/encoding-format '("" tjf:powerline/encoding "  "))

(defvar tjf:powerline/row-column-format '(" Ln: %l Col: %C ")
  "Format for displaying the column in the mode line.")

(defvar tjf:powerline/word-count-format
    (if (use-region-p)
        '("" tjf:powerline/buffer-line-count "," tjf:powerline/buffer-word-count "," tjf:powerline/buffer-byte-count " ["
             tjf:powerline/region-line-count "," tjf:powerline/region-word-count "," tjf:powerline/region-byte-count "] ")
      '("" tjf:powerline/buffer-line-count "," tjf:powerline/buffer-word-count "," tjf:powerline/buffer-byte-count " "))
  "Format for displaying the word count info in the mode line.")

(defun tjf:powerline/buffer-status ()
  "Return the read/write/modified status of the current buffer."
  (if (and buffer-read-only (buffer-modified-p))
      (propertize "RO/MOD " 'face 'powerline-red-face 'help-echo "Buffer is read-only and has been modified." 'mouse-face 'mode-line-highlight)
    (if (buffer-modified-p)
        (propertize " MOD   "  'face 'powerline-red-face 'help-echo "Buffer has been modified." 'mouse-face 'mode-line-highlight)
      (if buffer-read-only
          (propertize "  RO   " 'face 'mode-line 'help-echo "Buffer is read-only." 'mouse-face 'mode-line-highlight)
        (propertize "  ――   " 'help-echo "Buffer is unmodified." 'mouse-face 'mode-line-highlight)))))

(defun tjf:powerline/encoding ()
  "Translate `buffer-file-coding-system' into a modeline string."
  (if buffer-file-name
      (or (cdr (assq buffer-file-coding-system tjf:powerline/encoding-alist))
          (capitalize (symbol-name buffer-file-coding-system)))
    ""))

(defvar tjf:powerline/encoding (tjf:powerline/encoding))

(defun tjf:powerline/fill (face reserve)
    "Return empty space using FACE and leaving RESERVE space on the right."
    (setq reserve (* (- reserve 3) 0.825))
    (propertize " "
                'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
                'face face))

(defun tjf:powerline/update-modeline-vars ()
  "Update the strings containing the modeline variables."
  (if (use-region-p)
      (progn
        (setq tjf:powerline/word-count-format '("" tjf:powerline/buffer-line-count "," tjf:powerline/buffer-word-count "," tjf:powerline/buffer-byte-count " ["
                                                   tjf:powerline/region-line-count "," tjf:powerline/region-word-count "," tjf:powerline/region-byte-count "] "))
        (setq tjf:powerline/region-line-count (int-to-string (abs (- (line-number-at-pos (point)) (line-number-at-pos (mark))))))
        (setq tjf:powerline/region-word-count (int-to-string (count-words-region (point) (mark))))
        (setq tjf:powerline/region-byte-count (int-to-string (abs (- (point) (mark))))))
    (setq tjf:powerline/word-count-format '("" tjf:powerline/buffer-line-count "," tjf:powerline/buffer-word-count "," tjf:powerline/buffer-byte-count " ")))
  (setq tjf:powerline/buffer-line-count (int-to-string (line-number-at-pos (point-max))))
  (setq tjf:powerline/buffer-word-count (int-to-string (count-words-region (point-min) (point-max))))
  (setq tjf:powerline/buffer-byte-count (int-to-string (point-max)))
  (setq tjf:powerline/encoding (tjf:powerline/encoding))
  (set-buffer-modified-p (buffer-modified-p)))

(defun tjf:powerline/theme-lhs (mode-line face0 face1 face2)
  "Define the left-hand side of the modeline using MODE-LINE, FACE0, FACE1, and FACE2."
  (let ((separator-left  (intern (format "powerline-%s-%s" (powerline-current-separator) (car powerline-default-separator-dir)))))
    (list
     (when (and (boundp 'global-anzu-mode) global-anzu-mode)
       (powerline-raw '(:eval (anzu--update-mode-line) anzu-mode-line 'l)))
     (unless (and (boundp 'global-anzu-mode) global-anzu-mode anzu--state)
       (powerline-raw '(:eval (tjf:powerline/buffer-status))))
     (funcall separator-left mode-line face0)
     (powerline-buffer-id face0 'l)
     (when (and (boundp 'which-function-mode) (eq tjf:view/which-function-mode 'on))
       (powerline-raw which-func-format face0 'l))
     (powerline-vc face0) ;; !!!!
     (powerline-raw " " face0)
     (funcall separator-left face0 face1)
     (powerline-major-mode face1 'l)
     (powerline-process face1)
     (powerline-narrow face1 'l)
     (powerline-raw " " face1)
     (funcall separator-left face1 face2))))


(defun tjf:powerline/theme-rhs (mode-line face0 face1 face2)
  "Define the right-hand side of the modeline using MODE-LINE, FACE0, FACE1, and FACE2."
  (let ((separator-right (intern (format "powerline-%s-%s" (powerline-current-separator) (cdr powerline-default-separator-dir)))))
    (list (powerline-raw global-mode-string face2 'r)
          (funcall separator-right face2 face1)
          (powerline-raw tjf:powerline/row-column-format face1 'r)
          (funcall separator-right face1 face0)
          (powerline-raw tjf:powerline/word-count-format face0 'r)
          (funcall separator-right face0 mode-line)
          (powerline-raw tjf:powerline/encoding-format nil 'l))))

(defun tjf:powerline/theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("  "
                  (:eval
                   (let* ((active (powerline-selected-window-active))

                          ;; mode-line > face0 > face1 > face2 > face1 > face0 > mode-line
                          (mode-line (if active 'mode-line         'mode-line-inactive))
                          (face0     (if active 'powerline-active0 'powerline-inactive0))
                          (face1     (if active 'powerline-active1 'powerline-inactive1))
                          (face2     (if active 'powerline-active2 'powerline-inactive2))

                          ;; (separator-left  (intern (format "powerline-%s-%s" (powerline-current-separator) (car powerline-default-separator-dir))))
                          ;; (separator-right (intern (format "powerline-%s-%s" (powerline-current-separator) (cdr powerline-default-separator-dir))))

                          (lhs    (tjf:powerline/theme-lhs mode-line face0 face1 face2))
                          (center nil)
                          (rhs    (tjf:powerline/theme-rhs mode-line face0 face1 face2)))
		             (concat (powerline-render lhs)
                             (powerline-fill-center face2 (/ (powerline-width center) 2.0))
                             (powerline-render center)
			                 (tjf:powerline/fill face2 (powerline-width rhs))
			                 (powerline-render rhs)))))))

;;
(message "Loading tjf-powerline...done")
(provide 'tjf-powerline)

;;; tjf-powerline.el ends here
