;;; colors.el --- Color functions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2017-2019 Tom Fontaine

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
;;           30-Jul-2019 Added ‘get-background-color’

;;; Code:

(message "Loading colors...")

(eval-when-compile
  (require 'color)
  (require 'cl-macs))
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

(defun get-background-color ()
  "Return the rgb hexadecimal value of the background color of the current frame."
  (cdr (assq 'background-color (frame-parameters))))

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

(defun color-hex-to-rgb (hex)
  "Convert a HEX color to an RGB-triplet."
  (setq hex (replace-regexp-in-string "#" "" hex))
  (mapcar #'(lambda (s) (/ (string-to-number s 16) 255.0))
          (list (substring hex 0 2)
                (substring hex 2 4)
                (substring hex 4 6))))

(defun u/color-rgb-to-hex (rgb)
  "Convert RGB to a hex color."
  (destructuring-bind
    (red green blue) rgb (color-rgb-to-hex red green blue 2)))

;; (defun color-rgb-to-hex  (red green blue &optional digits-per-component)
;;   "Return hexadecimal #RGB notation for the color specified by RED GREEN BLUE.
;; RED, GREEN, and BLUE should be numbers between 0.0 and 1.0, inclusive.
;; Optional argument DIGITS-PER-COMPONENT can be either 4 (the default)
;; or 2; use the latter if you need a 24-bit specification of a color."
;;   (or digits-per-component (setq digits-per-component 4))
;;   (let* ((maxval (if (= digits-per-component 2) 255 65535))
;;          (fmt (if (= digits-per-component 2) "#%02x%02x%02x" "#%04x%04x%04x")))
;;     (format fmt (* red maxval) (* green maxval) (* blue maxval))))

(defun u/color-rgb-to-hsv (rgb)
  "Convert RGB to an HSV-triplet."
  (destructuring-bind
    (red green blue) rgb (color-rgb-to-hsv red green blue)))

(defun u/color-rgb-to-hsl (rgb)
   "Convert RGB to an HSL-triplet."
   (destructuring-bind
    (red green blue) rgb (color-rgb-to-hsl red green blue))
 )

(defun scale-to-8bit (zero-to-one)
  "Scale ZERO-TO-ONE (0.0-1.0) from 0-255."
  (* zero-to-one 255.0))

;;
(message "Loading colors...done")
(provide 'colors)

;;; colors.el ends here
