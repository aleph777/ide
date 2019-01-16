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

;; Revision: 11-Jun-2018 Added functions from ‘u-frame.el’

;;; Code:

(message "Loading colors...")
;;
(defsubst random-value (min max step)
  "Return a random number between MIN and MAX by STEP."
  (let ((scale (/ (- max min) step)))
    (+ (* step (random scale)) min)
    ))

(defun random-background-color ()
  "Return a random background color."
  (let ((min   160)
        (max   248)
        (step    4)
        (red     0)
        (green   0)
        (blue    0))
    (while (and (<= red 192) (<= green 192) (<= blue 192))
      (setq red   (random-value min max step)
            green (random-value min max step)
            blue  (random-value min max step)))
    (format "#%x%x%x" red green blue)))

(defun set-random-background-color ()
  "Set a random background color."
  (interactive)
  (if window-system (set-background-color (random-background-color))))

(defun color-bg/red ()
  "Return a red background color value."
  (let* ((red   (random-value 192 248 4))
         (green (random-value 160 188 4))
         (blue  (random-value 128 188 4))
         (color (format "#%x%x%x" red green blue)))
    color))

(defun color-bg/green ()
  "Return a green background color value."
  (let* ((red   (random-value 128 188 4))
         (green (random-value 192 248 4))
         (blue  (random-value 128 188 4))
         (color (format "#%x%x%x" red green blue)))
    color))

(defun color-bg/blue ()
  "Return a blue background color value."
  (let* ((red   (random-value 128 188 4))
         (green (random-value 128 188 4))
         (blue  (random-value 192 248 4))
         (color (format "#%x%x%x" red green blue)))
    color))

(defun color-bg/cyan ()
  "Return a cyan background color value."
  (let* ((red  (random-value 128 188 4))
         (cyan (random-value 192 248 4))
         (color (format "#%x%x%x" red cyan cyan)))
    color))

(defun color-bg/magenta ()
  "Return a magenta background color value."
  (let* ((magenta (random-value 192 248 4))
         (green   (random-value 128 188 4))
         (color (format "#%x%x%x" magenta green magenta)))
    color))

(defun color-bg/yellow ()
  "Return a yellow background color value."
  (let* ((yellow (random-value 192 248 4))
         (blue   (random-value 128 188 4))
         (color (format "#%x%x%x" yellow yellow blue)))
    color))

(defun color-bg/pink ()
  "Return a pink background color value."
  (let* ((red   (random-value 192 248 4))
         (green (random-value 128 156 4))
         (blue  (random-value 160 188 4))
         (color (format "#%x%x%x" red green blue)))
    color))

(defun color-bg/indigo ()
  "Return an indigo background color value."
  (let* ((red   (random-value 160 188 4))
         (green (random-value 128 156 4))
         (blue  (random-value 192 248 4))
         (color (format "#%x%x%x" red green blue)))
    color))

(defun color-bg/gray ()
  "Return an gray background color value."
  (let* ((red   (random-value 192 248 4))
         (green (random-value (round (* 0.975 red)) (round (* 1.025 red)) 4))
         (blue  (random-value (round (* 0.975 red)) (round (* 1.025 red)) 4))
         (color (format "#%x%x%x" red green blue)))
    color))
;;
(message "Loading colors...done")
(provide 'colors)

;;; colors.el ends here
