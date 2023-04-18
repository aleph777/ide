;;; tjf-color.el --- Color functions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2017-2023 Tom Fontaine

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
;;           17-Sep-2020 Reworked color functions and renamed to `color/'
;;           03-Feb-2021 ‘tjf’ overhaul
;;

;;; Code:

(message "Loading tjf-color...")
(eval-when-compile
  (require 'color)
  (require 'cl-macs))

;;
(defvar tjf:color/gray-saturation-minimum-bg nil "The lower limit of gray saturation (integer 0 - 100).")
(setq   tjf:color/gray-saturation-minimum-bg 0)

(defvar tjf:color/gray-saturation-maximum-bg nil "The upper limit of gray saturation (integer 0 - 100).")
(setq   tjf:color/gray-saturation-maximum-bg 25)

(defvar tjf:color/saturation-minimum-bg nil "The lower limit of color saturation (integer 0 - 100).")
(setq   tjf:color/saturation-minimum-bg 50)

(defvar tjf:color/saturation-maximum-bg nil "The upper limit of color saturation (integer 0 - 100).")
(setq   tjf:color/saturation-maximum-bg 75)

(defvar tjf:color/luminance-minimum-bg nil "The lower limit of color luminance (integer 0 - 100).")
(setq   tjf:color/luminance-minimum-bg 75)

(defvar tjf:color/luminance-maximum-bg nil "The upper limit of color luminance (integer 0 - 100).")
(setq   tjf:color/luminance-maximum-bg 95)

(defvar tjf:color/hue-step nil "The amount to increment/decrement the hue value (float 0.0 - 1.0).")
(setq   tjf:color/hue-step 0.05)

(defvar tjf:color/luminance-step nil "The amount to increment/decrement the luminance value (float 0.0 - 1.0).")
(setq   tjf:color/luminance-step 0.05)

(defvar tjf:color/saturation-step nil "The amount to increment/decrement the saturation value (float 0.0 - 1.0).")
(setq   tjf:color/saturation-step 0.05)

(defun tjf:color/brighten (hex)
  "Increase the luminance of HEX."
  (let* ((hsl   (tjf:color/hex-to-hsl hex))
         (h     (nth 0 hsl))
         (s     (nth 1 hsl))
         (old-l (nth 2 hsl))
         (new-l (+ old-l tjf:color/luminance-step))
         (max   (/ tjf:color/luminance-maximum-bg 100.0))
         (l     (if (> new-l max) max new-l)))
    (list h s l)))

(defun tjf:color/brighten-background ()
  "Increase the luminance of the background color of the current frame."
  (interactive)
  (let* ((hex (tjf:color/get-background))
         (hsl (tjf:color/brighten hex)))
    (set-background-color (tjf:color/hsl-to-hex hsl))))

(defun tjf:color/darken (hex)
  "Decrease the luminance of HEX."
  (let* ((hsl   (tjf:color/hex-to-hsl hex))
         (h     (nth 0 hsl))
         (s     (nth 1 hsl))
         (old-l (nth 2 hsl))
         (new-l (- old-l tjf:color/luminance-step))
         (min   (/ tjf:color/luminance-minimum-bg 100.0))
         (l     (if (< new-l min) min new-l)))
    (list h s l)))

(defun tjf:color/darken-background ()
  "Decrease the luminance of the background color of the current frame."
  (interactive)
  (let* ((hex (tjf:color/get-background))
         (hsl (tjf:color/darken hex)))
    (set-background-color (tjf:color/hsl-to-hex hsl))))

(defun tjf:color/decrease-hue (hex)
  "Decrease the hue of HEX."
  (let* ((hsl   (tjf:color/hex-to-hsl hex))
         (old-h (nth 0 hsl))
         (new-h (- old-h tjf:color/hue-step))
         (h     (if (< new-h 0.0) 0.995 new-h)) ; it's a circle
         (s     (nth 1 hsl))
         (l     (nth 2 hsl)))
    (list h s l)))

(defun tjf:color/decrease-hue-background ()
  "Decrease the hue of the background color of the current frame."
  (interactive)
  (let* ((hex (tjf:color/get-background))
         (hsl (tjf:color/decrease-hue hex)))
    (set-background-color (tjf:color/hsl-to-hex hsl))))

(defun tjf:color/desaturate (hex)
  "Decrease the saturation of HEX."
  (let* ((hsl   (tjf:color/hex-to-hsl hex))
         (h     (nth 0 hsl))
         (old-s (nth 1 hsl))
         (new-s (- old-s tjf:color/saturation-step))
         (s     (if (< new-s 0.0) 0.0 new-s))
         (l     (nth 2 hsl)))
    (list h s l)))

(defun tjf:color/desaturate-background ()
  "Decrease the saturation of the background color of the current frame."
  (interactive)
  (let* ((hex (tjf:color/get-background))
         (hsl (tjf:color/desaturate hex)))
    (set-background-color (tjf:color/hsl-to-hex hsl))))

(defun tjf:color/get-background ()
  "Return the rgb hexadecimal value of the background color of the current frame."
  (cdr (assq 'background-color (frame-parameters))))

(defun tjf:color/hex-to-rgb (hex)
  "Convert a HEX color to an RGB-triplet."
  (setq hex (replace-regexp-in-string "#" "" hex))
  (mapcar #'(lambda (s) (/ (string-to-number s 16) 255.0))
          (list (substring hex 0 2)
                (substring hex 2 4)
                (substring hex 4 6))))

(defun tjf:color/hex-to-hsl (hex)
  "Convert a HEX color to an HSL-triplet."
  (setq hex (replace-regexp-in-string "#" "" hex))
  (let* ((rgb (tjf:color/hex-to-rgb hex)))
    (tjf:color/rgb-to-hsl rgb)))

(defun tjf:color/hsl-to-hex (hsl)
  "Convert HSL to HEX."
  (let* ((rgb (tjf:color/hsl-to-rgb hsl)))
    (tjf:color/rgb-to-hex rgb)))

(defun tjf:color/hsl-to-rgb (hsl)
  "Convert HSL to RGB."
  (cl-destructuring-bind
      (hue saturation luminance) hsl (color-hsl-to-rgb hue saturation luminance)))

(defun tjf:color/increase-hue (hex)
  "Increase the hue of HEX."
  (let* ((hsl   (tjf:color/hex-to-hsl hex))
         (old-h (nth 0 hsl))
         (new-h (+ old-h tjf:color/hue-step))
         (h     (if (>= new-h 1.0) 0.0 new-h)) ; it's a circle
         (s     (nth 1 hsl))
         (l     (nth 2 hsl)))
    (list h s l)))

(defun tjf:color/increase-hue-background ()
  "Increase the hue of the background color of the current frame."
  (interactive)
  (let* ((hex (tjf:color/get-background))
         (hsl (tjf:color/increase-hue hex)))
    (set-background-color (tjf:color/hsl-to-hex hsl))))

(defun tjf:color/random (min max)
  "Return a random number between MIN and MAX."
    (+ (random (- max min)) min))

(defun tjf:color/random-background ()
  "Return a random HSL background color."
  (list (/ (tjf:color/random 0 359) 360.0)
        (/ (tjf:color/random 0 100) 100.0)
        (/ (tjf:color/random tjf:color/luminance-minimum-bg tjf:color/luminance-maximum-bg) 100.0)))

(defun tjf:color/random-background-hex ()
  "Return a random HEX background color."
  (tjf:color/hsl-to-hex (tjf:color/random-background)))

(defun tjf:color/rgb-to-hex (rgb)
  "Convert RGB to a hex color."
  (cl-destructuring-bind
    (red green blue) rgb (color-rgb-to-hex red green blue 2)))

(defun tjf:color/rgb-to-hsl (rgb)
  "Convert RGB to HSL."
  (cl-destructuring-bind
    (red green blue) rgb (color-rgb-to-hsl red green blue)))

(defun tjf:color/saturate (hex)
  "Increase the saturation of HEX."
  (let* ((hsl   (tjf:color/hex-to-hsl hex))
         (h     (nth 0 hsl))
         (old-s (nth 1 hsl))
         (new-s (+ old-s tjf:color/saturation-step))
         (s     (if (> new-s 1.0) 1.0 new-s))
         (l     (nth 2 hsl)))
    (list h s l)))

(defun tjf:color/saturate-background ()
  "Increase the saturation of the background color of the current frame."
  (interactive)
  (let* ((hex (tjf:color/get-background))
         (hsl (tjf:color/saturate hex)))
    (set-background-color (tjf:color/hsl-to-hex hsl))))


(defun tjf:color/set-background (color)
  "Set background to COLOR."
  (interactive)
  (if window-system
      (let* ((hex (tjf:color/hsl-to-hex color)))
        (set-background-color hex))
    (error "Not a wondowed system!")))

(defun tjf:color/set-background-blue ()
  "Return a random blueish background color."
  (interactive)
  (tjf:color/set-background(list (/ (tjf:color/random 210 269) 360.0)
                                 (/ (tjf:color/random tjf:color/saturation-minimum-bg tjf:color/saturation-maximum-bg) 100.0)
                                 (/ (tjf:color/random tjf:color/luminance-minimum-bg  tjf:color/luminance-maximum-bg)  100.0))))

(defun tjf:color/set-background-cyan ()
  "Return a random cyanish background color."
  (interactive)
  (tjf:color/set-background(list (/ (tjf:color/random 150 209) 360.0)
                                 (/ (tjf:color/random tjf:color/saturation-minimum-bg tjf:color/saturation-maximum-bg) 100.0)
                                 (/ (tjf:color/random tjf:color/luminance-minimum-bg  tjf:color/luminance-maximum-bg)  100.0))))

(defun tjf:color/set-background-gray ()
  "Return a random grayish background color."
  (interactive)
  (tjf:color/set-background(list (/ (tjf:color/random 0 359) 360.0)
                                 (/ (tjf:color/random tjf:color/gray-saturation-minimum-bg tjf:color/gray-saturation-maximum-bg) 100.0)
                                 (/ (tjf:color/random tjf:color/luminance-minimum-bg       tjf:color/luminance-maximum-bg)       100.0))))

(defun tjf:color/set-background-green ()
  "Return a random greenish background color."
  (interactive)
  (tjf:color/set-background(list (/ (tjf:color/random 90 149) 360.0)
                                 (/ (tjf:color/random tjf:color/saturation-minimum-bg tjf:color/saturation-maximum-bg) 100.0)
                                 (/ (tjf:color/random tjf:color/luminance-minimum-bg  tjf:color/luminance-maximum-bg)  100.0))))

(defun tjf:color/set-background-magenta ()
  "Return a random magentaish background color."
  (interactive)
  (tjf:color/set-background(list (/ (tjf:color/random 270 329) 360.0)
                                 (/ (tjf:color/random tjf:color/saturation-minimum-bg tjf:color/saturation-maximum-bg) 100.0)
                                 (/ (tjf:color/random tjf:color/luminance-minimum-bg  tjf:color/luminance-maximum-bg)  100.0))))

(defun tjf:color/set-background-random ()
  "Set a random background color."
  (interactive)
  (tjf:color/set-background (tjf:color/random-background)))

(defun tjf:color/set-background-red ()
  "Return a random redish background color."
  (interactive)
  (let ((hue (tjf:color/random -30 29)))
    (tjf:color/set-background(list (/ (if (< hue 0) (+ hue 360) hue) 360.0)
                                   (/ (tjf:color/random tjf:color/saturation-minimum-bg tjf:color/saturation-maximum-bg) 100.0)
                                   (/ (tjf:color/random tjf:color/luminance-minimum-bg  tjf:color/luminance-maximum-bg)  100.0)))))

(defun tjf:color/set-background-yellow ()
  "Return a random yellowish background color."
  (interactive)
  (tjf:color/set-background(list (/ (tjf:color/random 30 89) 360.0)
                                 (/ (tjf:color/random tjf:color/saturation-minimum-bg tjf:color/saturation-maximum-bg) 100.0)
                                 (/ (tjf:color/random tjf:color/luminance-minimum-bg  tjf:color/luminance-maximum-bg)  100.0))))

;;
(message "Loading tjf-color...done")
(provide 'tjf-color)

;;; tjf-color.el ends here
