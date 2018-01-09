;;; colors.el --- [description] -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2017-2018 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   25-Jan-2017

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

;;; Code:

(message "Configuring from colors...")
;;
(defun color-bg/red ()
  "Return a red background color value."
  (let* ((red   (random-value 228 255 1))
         (green (random-value (round (* 0.58 red)) (round (* 0.95 red))  1))
         (blue  (random-value (round (* 0.98 green)) green 1))
         (color (format "#%x%x%x" red green blue)))
    color))

(defun color-bg/pink ()
  "Return a pink background color value."
  (let* ((red   (random-value 240 255 1))
         (blue  (random-value (round (* 0.67 red))  (round (* 0.94 red)) 1))
         (green (random-value (round (* 0.75 blue)) (round (* 0.97 blue))  1))
         (color (format "#%x%x%x" red green blue)))
    color))

(defun color-bg/green ()
  "Return a green background color value."
  (let* ((green (random-value 190 255 1))
         (blue  (random-value (round (* 0.66 green)) (round (* 0.95 green)) 1))
         (red   (random-value (round (* 0.98 blue)) blue  1))
         (color (format "#%x%x%x" red green blue)))
    color))

(defun color-bg/blue ()
  "Return a blue background color value."
  (let* ((blue  (random-value 240 255 1))
         (green (random-value (round (* 0.67 blue)) (round (* 0.95 blue)) 1))
         (red   (random-value (round (* 0.5 green)) (round (* 0.9 green)) 1))
         (color (format "#%x%x%x" red green blue)))
    color))

(defun color-bg/indigo ()
  "Return an indigo background color value."
  (let* ((blue  (random-value 218 255 1))
         (red   (random-value (round (* 0.73 blue)) (round (* 0.95 blue)) 1))
         (green (round (* .91 red)))
         (color (format "#%x%x%x" red green blue)))
    color))
;;
(message "Loading colors...done")
(provide 'colors)

;;; colors.el ends here
