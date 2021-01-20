;;; u-powerline.el --- Powerline setup -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

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
;;

;;; Code:

(message "Loading u-powerline...")
;;
(require 'anzu)
(require 'powerline)
(require 'u-flags)
(require 'u-macro)
(require 'u-view)

(defvar current-column nil
  "String value of current (1+) column.")
(defvar bufreg-line-count nil
  "String value of Buffer or Region line count.")
(defvar bufreg-word-count nil
  "String value of Buffer or Region word count.")
(defvar bufreg-byte-count nil
  "String value of Buffer or Region byte count.")

(make-variable-buffer-local 'current-column)
(make-variable-buffer-local 'bufreg-line-count)
(make-variable-buffer-local 'bufreg-word-count)
(make-variable-buffer-local 'bufreg-byte-count)

(defvar u-encoding-alist (list (cons 'prefer-utf-8-unix            "Unix")
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

(defun u-modeline-encoding ()
  "Translate `buffer-file-coding-system' into a modeline string."
  (if buffer-file-name
      (or (cdr (assq buffer-file-coding-system u-encoding-alist))
          (capitalize (symbol-name buffer-file-coding-system)))
    ""))

(defvar current-encoding (u-modeline-encoding))

(defvar mode-line-encoding-format '("" current-encoding ""))

(defvar mode-line-row-column-format '(" Ln: %l Col: " current-column " ")
  "Format for displaying the column in the mode line.")

(defvar mode-line-wc-format '("" bufreg-line-count "," bufreg-word-count "," bufreg-byte-count " ")
  "Format for displaying the wc info in the mode line.")

(defvar caps-lock-format "")

(add-hook 'post-command-hook 'do-update-modeline-vars)

(defun do-update-modeline-vars ()
  "Update the string containing the current column."
  (if (use-region-p)
      (setq bufreg-line-count (int-to-string (abs (- (line-number-at-pos (point)) (line-number-at-pos (mark)))))
            bufreg-word-count (int-to-string (count-words-region (point) (mark)))
            bufreg-byte-count (int-to-string (abs (- (point) (mark)))))
    (setq bufreg-line-count (int-to-string (line-number-at-pos (point-max)))
          bufreg-word-count (int-to-string (count-words-region (point-min) (point-max)))
          bufreg-byte-count (int-to-string (point-max))))
  (setq current-column    (int-to-string (1+ (current-column)))
        current-encoding  (u-modeline-encoding))
  (set-buffer-modified-p (buffer-modified-p)))

(alias-face powerline-red-face fontaine/powerline-red)

(defun u-powerline-theme ()
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

                          (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list
                                ;; (powerline-raw mode-line-mule-info nil 'l)
                                ;;(powerline-raw mode-line-modified nil 'l)
                                (when (and (boundp 'global-anzu-mode) global-anzu-mode)
                                  (powerline-raw '(:eval (anzu--update-mode-line) anzu-mode-line 'l)))
                                (powerline-raw '(:eval
                                                 (if (and buffer-read-only (buffer-modified-p))
                                                     (propertize "RO/MOD" 'face 'powerline-red-face
                                                                 'help-echo "Buffer is read-only and has been modified."
                                                                 'mouse-face 'mode-line-highlight)
                                                   (if (buffer-modified-p)
                                                       (propertize "MOD" 'face 'powerline-red-face
                                                                   'help-echo "Buffer has been modified."
                                                                   'mouse-face 'mode-line-highlight)
                                                     (if buffer-read-only
                                                         (propertize "RO" 'face 'mode-line
                                                                     'help-echo "Buffer is read-only."
                                                                     'mouse-face 'mode-line-highlight)
                                                       (propertize "――" 'help-echo "Buffer is unmodified."
                                                                   'mouse-face 'mode-line-highlight))))))
                                ;; (when (and (boundp 'global-anzu-mode) global-anzu-mode)
                                ;;   (powerline-raw '(:eval (anzu--update-mode-line) anzu-mode-line 'l)))

                                ;; (:eval (cond (buffer-read-only
                                ;;               (propertize "%%%%" 'face 'my-yellow-face
                                ;;                           'help-echo "Buffer is read-only."
                                ;;                           'mouse-face 'mode-line-highlight))
                                ;;              ((buffer-modified-p)
                                ;;               (propertize "**" 'face 'my-red-face
                                ;;                           'help-echo "Buffer has been modified."
                                ;;                           'mouse-face 'mode-line-highlight))
                                ;;              (t (propertize "--" 'help-echo "Buffer is unmodified."
                                ;;                             'mouse-face 'mode-line-highlight))))
                                ;;)
                                (funcall separator-left mode-line face0)
                                (powerline-buffer-id face0 'l)
                                (when (and (boundp 'which-function-mode) (eq u-which-function-mode 'on))
                                  (powerline-raw which-func-format face0 'l))
                                ;; (powerline-raw " " face0)
                                (powerline-vc face0) ;; !!!!
                                (funcall separator-left face0 face1)
                                (powerline-major-mode face1 'l)
                                (powerline-process face1)
                                ;; (powerline-minor-modes face1 'l)
                                (powerline-narrow face1 'l)
                                (powerline-raw " " face1)
                                (funcall separator-left face1 face2)
                                (powerline-raw (if (is-caps-lock-on? (x-led-mask)) " CAPS-LOCK" "") face2 'l)
                                ;; (powerline-vc face2 'r)
                                ))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
				     (powerline-raw mode-line-row-column-format face1 'r)
				     (funcall separator-right face1 face0)
				     (powerline-raw mode-line-wc-format face0 'r)
				     (funcall separator-right face0 mode-line)
                                     ;; (powerline-raw mode-line-mule-info nil 'l)
                                     (powerline-raw mode-line-encoding-format nil 'l)
                                     )))
		     (concat (powerline-render lhs)
			     (powerline-fill face2 (powerline-width rhs))
			     (powerline-render rhs))))
                  ))
)

(setq powerline-default-separator 'wave)
(u-powerline-theme)
;;
(message "Loading u-powerline...done")
(provide 'u-powerline)

;;; u-powerline.el ends here
