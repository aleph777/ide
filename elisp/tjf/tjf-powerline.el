;;; tjf-powerline.el --- Powerline setup -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2021 by Tom Fontaine

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

;; Revision: 01-Apr-2016 Cleaned up do-update-modeline-vars
;;           09-May-2016 Added `anzu'
;;           14-Sep-2016 Fixed `mode-line-inactive'
;;           18-Sep-2016 Moved `alias-face' to `u-macro'
;;           23-May-2017 Changed ‘powerline-red-face’ to ‘error’
;;           13-Jun-2018 Added ‘require’ for ‘u-flags’
;;           03-Jul-2018 Moved ‘powerline-vc’ to rhs face2
;;           10-Jul-2019 Added ‘require’ for ‘anzu’
;;           28-Oct-2020 Reworked because of ‘powerline' changes
;;           03-Feb-2021 ‘tjf’ overhaul
;;

;;; Code:

(message "Loading tjf-powerline...")
(require 'anzu)
(require 'powerline)
(require 'tjf-flags)
(require 'tjf-macro)
(require 'tjf-view)

;;
(defvar tjf:powerline/bufreg-byte-count nil
  "String value of Buffer or Region byte count.")
(make-variable-buffer-local 'tjf:powerline/bufreg-byte-count)

(defvar tjf:powerline/bufreg-line-count nil
  "String value of Buffer or Region line count.")
(make-variable-buffer-local 'tjf:powerline/bufreg-line-count)

(defvar tjf:powerline/bufreg-word-count nil
  "String value of Buffer or Region word count.")
(make-variable-buffer-local 'tjf:powerline/bufreg-word-count)

(defvar tjf:powerline/column nil
  "String value of current (1+) column.")
(make-variable-buffer-local 'tjf:powerline/column)

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

(defvar tjf:powerline/encoding-format '("" tjf:powerline/encoding ""))

(defvar tjf:powerline/row-column-format '(" Ln: %l Col: " tjf:powerline/column " ")
  "Format for displaying the column in the mode line.")

(defvar tjf:powerline/word-count-format '("" tjf:powerline/bufreg-line-count "," tjf:powerline/bufreg-word-count "," tjf:powerline/bufreg-byte-count " ")
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
        (setq tjf:powerline/bufreg-line-count (int-to-string (abs (- (line-number-at-pos (point)) (line-number-at-pos (mark))))))
        (setq tjf:powerline/bufreg-word-count (int-to-string (count-words-region (point) (mark))))
        (setq tjf:powerline/bufreg-byte-count (int-to-string (abs (- (point) (mark))))))
    (setq tjf:powerline/bufreg-line-count (int-to-string (line-number-at-pos (point-max))))
    (setq tjf:powerline/bufreg-word-count (int-to-string (count-words-region (point-min) (point-max))))
    (setq tjf:powerline/bufreg-byte-count (int-to-string (point-max))))
  (setq tjf:powerline/column    (int-to-string (1+ (current-column))))
  (setq tjf:powerline/encoding (tjf:powerline/encoding))
  (set-buffer-modified-p (buffer-modified-p)))


(defun tjf:powerline/alt-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("  "
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))  ;; mode-line > face1 > face2 > face3 > face2 > face1 > mode-line

                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))

                          (separator-left  (intern (format "powerline-%s-%s" (powerline-current-separator) (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s" (powerline-current-separator) (cdr powerline-default-separator-dir))))

                          (lhs (list
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
                                (funcall separator-left face1 face2)

                                ))

                          (center (list (powerline-raw (if (tjf:flags/is-caps-lock-on? (tjf:flags/x-led-mask)) " CAPS-LOCK" "") face2 'l)))

                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
				     (powerline-raw tjf:powerline/row-column-format face1 'r)
				     (funcall separator-right face1 face0)
				     (powerline-raw tjf:powerline/word-count-format face0 'r)
				     (funcall separator-right face0 mode-line)
                                     (powerline-raw tjf:powerline/encoding-format nil 'l)
                                     )))
		     (concat (powerline-render lhs)
                             (powerline-fill-center face2 (/ (powerline-width center) 2.0))
                             (powerline-render center)
			     (tjf:powerline/fill face2 (powerline-width rhs))
			     (powerline-render rhs))))
                  )))

(defun tjf:powerline/theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("  "
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))  ;; mode-line > face1 > face2 > face3 > face2 > face1 > mode-line

                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))

                          (separator-left  (intern (format "powerline-%s-%s" (powerline-current-separator) (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s" (powerline-current-separator) (cdr powerline-default-separator-dir))))
                          (lhs (list
                                (powerline-raw '(:eval
                                                 (if (and buffer-read-only (buffer-modified-p))
                                                     (propertize "RO/MOD " 'face 'powerline-red-face
                                                                 'help-echo "Buffer is read-only and has been modified."
                                                                 'mouse-face 'mode-line-highlight)
                                                   (if (buffer-modified-p)
                                                       (propertize "MOD " 'face 'powerline-red-face
                                                                   'help-echo "Buffer has been modified."
                                                                   'mouse-face 'mode-line-highlight)
                                                     (if buffer-read-only
                                                         (propertize "RO " 'face 'mode-line
                                                                     'help-echo "Buffer is read-only."
                                                                     'mouse-face 'mode-line-highlight)
                                                       (propertize "―― " 'help-echo "Buffer is unmodified."
                                                                   'mouse-face 'mode-line-highlight))))))
                                (when (and (boundp 'global-anzu-mode) global-anzu-mode)
                                  (powerline-raw '(:eval (anzu--update-mode-line) anzu-mode-line 'l)))
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
                                (funcall separator-left face1 face2)
                                (powerline-raw (if (tjf:flags/is-caps-lock-on? (tjf:flags/x-led-mask)) " CAPS-LOCK" "") face2 'l)
                                ))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
				     (powerline-raw tjf:powerline/row-column-format face1 'r)
				     (funcall separator-right face1 face0)
				     (powerline-raw tjf:powerline/word-count-format face0 'r)
				     (funcall separator-right face0 mode-line)
                                     (powerline-raw tjf:powerline/encoding-format nil 'l)
                                     )))
		     (concat (powerline-render lhs)
			     (tjf:powerline/fill face2 (powerline-width rhs))
			     (powerline-render rhs))))
                  )))

;; (add-hook 'post-command-hook 'tjf:powerline/update-modeline-vars)

;; (alias-face powerline-red-face fontaine/powerline-red)

;; (setq powerline-default-separator 'wave)
;; (tjf:powerline/theme)
;
;;
(message "Loading tjf-powerline...done")
(provide 'tjf-powerline)

;;; tjf-powerline.el ends here
