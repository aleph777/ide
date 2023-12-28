;;; tjf-frame.el --- Functions that control frame appearance -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 1999-2023 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   15-Dec-1999

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

;; Revision: 15-Sep-2000 added 1 column to c/c++ mode entries in usr-size-alist
;;                       to get a full 80 columns
;;           16-Nov-2001 revised for Emacs 21
;;           04-Feb-2005 restored newframe method for manpages
;;           29-May-2008 add ‘ssh-mode’ to ‘usr-size-alist’
;;           02-Feb-2010 added exit message
;;           03-Mar-2014 added ‘clips-mode’, ‘csharp-mode’, and ‘matlab-mode’ to ‘usr-size-alist’
;;           10-Apr-2014 added ‘nxml-mode’ to ‘usr-size-alist’
;;           06-May-2014 added log-mode to ‘usr-size-alist’
;;           27-Mar-2015 added java-mode to ‘usr-size-alist’
;;           30-Mar-2015 converted ‘current-frame’ to ‘alias selected-frame’
;;           16-Jan-2016 added ‘makefile-modes’
;;           26-Jan-2016 added ‘usr-fullscreen-p’ and ‘usr-maxmized-p’
;;           29-Feb-2016 changed from ‘usr-’ to ‘u-’
;;           02-Mar-2016 fixed problem with ‘query-frame-colors’
;;                       added ‘comint-mode’ and ‘shell-mode’ to ‘frame-mode-size-alist’
;;           03-Mar-2016 added ‘html-mode’, ‘html-helper-mode’, and ‘nxhtml-mode’ to ‘frame-mode-size-alist’
;;                       added ‘javascript-mode’, ‘js2-mode’, and ‘espresso-mode’ to ‘frame-mode-size-alist’
;;           13-Sep-2016 changed ‘default-frame-width’ to 120
;;                       changed ‘alt-frame-width’ to 96
;;           31-Jan-2017 added ‘json-mode’
;;           11-Jun-2018 moved color functions to ‘colors.el’
;;           14-Jun-2018 changed ‘alt-frame-width’ to 112 to accomodate toolbar
;;                       added new major modes to ‘frame-mode-size-alist’
;;           03-Feb-2021 ‘tjf’ overhaul
;;           09-Feb-2023 added ‘tjf:frame/half-size’
;;           15-Feb-2023 changed ‘default-frame-width’ and ‘default-frame-height’ to use monitor settings
;;                       fixed ‘tjf:frame/half-size’
;;           21-Sep-2023 removed ‘tjf/size-alist’
;;

;;; Code:

(message "Loading tjf-frame...")
(require 'tjf-flags)
(require 'tjf-color)

;; (push '(width  . (text-pixels . 1920)) default-frame-alist)
;; (push '(height . (text-pixels . 1080)) default-frame-alist)
;; ;;
(defvar tjf:frame/format-colors-modeline   "Background: %s, Cursor: %s, Mouse: %s")
(defvar tjf:frame/format-colors-properties "Background:  %s\nCursor:      %s\nMouse:       %s\n\n")

(defvar tjf:frame/format-size-modeline   "Window Size: %d colums x %d rows")
(defvar tjf:frame/format-size-properties "Window Size: %d colums, %d rows\n")

(defvar tjf:frame/format-font-modeline   "Font: %s")
(defvar tjf:frame/format-font-properties "Font:        %s\n")

(defvar tjf:frame/default-width)
(defvar tjf:frame/default-height)
(setq tjf:frame/default-width   (- (/ (x-display-pixel-width)  (frame-char-width) 4) 8))
(setq tjf:frame/default-height  (- (/ (x-display-pixel-height) (frame-char-height) ) 10))

(defvar tjf:frame/default-size (list (cons 'width tjf:frame/default-width) (cons 'height tjf:frame/default-height)))

;;
;; NOTE: frame size is usually set in switch-to-buffer-other-frame but are also set
;;       in ‘after-make-frame-functions’ for emacsclient
;;
(setq after-make-frame-functions
      '(lambda (frame)
         (fringe-mode (cons 8 4))
         (modify-frame-parameters frame (list (cons 'background-color (tjf:color/random-background-hex))
                                              (cons 'width  tjf:frame/default-width)
                                              (cons 'height tjf:frame/default-height)))))

(defalias 'current-frame 'selected-frame)

;;; overload
(defun mouse-tear-off-window (click)
  "Delete the window clicked on, and create a new frame displaying its buffer."
  (interactive "e")
  (mouse-minibuffer-check click)
  (let* ((window    (posn-window (event-start click)))
         (buf       (window-buffer window))
         (width     tjf:frame/default-height)                                                     ;;; tjf
         (height    tjf:frame/default-width)                                                      ;;; tjf
         (frame     (make-frame (list (cons 'background-color (tjf:color/random-background-hex))  ;;; tjf
                                      (cons 'width  width)                                        ;;; tjf
                                      (cons 'height height)))))                                   ;;; tjf
    (select-frame frame)
    (switch-to-buffer buf)
    (delete-window window)))

;;; overload
(defun switch-to-buffer-other-frame (buffer-or-name &optional norecord)
  "Switch to buffer BUFFER-OR-NAME in another frame.
BUFFER-OR-NAME may be a buffer, a string (a buffer name), or
nil.  Return the buffer switched to.

If called interactively, prompt for the buffer name using the
minibuffer.  The variable `confirm-nonexistent-file-or-buffer'
determines whether to request confirmation before creating a new
buffer.

If BUFFER-OR-NAME is a string and does not identify an existing
buffer, create a new buffer with that name.  If BUFFER-OR-NAME is
nil, switch to the buffer returned by `other-buffer'.

Optional second arg NORECORD non-nil means do not put this
buffer at the front of the list of recently selected ones.

This uses the function `display-buffer' as a subroutine; see its
documentation for additional customization information."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other frame: ")))
  (let* ((major-mode (with-current-buffer buffer-or-name major-mode))
         (pop-up-frames t)
         (pop-up-frame-alist (append (list (cons 'background-color (tjf:color/get-background))      ;;; tjf
                                           (cons 'width  tjf:frame/default-width)                   ;;; tjf
                                           (cons 'height tjf:frame/default-height)))))              ;;; tjf
    (pop-to-buffer buffer-or-name display-buffer--other-frame-action norecord)))

(defun tjf:frame/display-parameters ()
  "Displays the parameters of the current frame."
  (interactive)
  (with-output-to-temp-buffer "*frame parameters*"
    (prin1 (frame-parameters))))

(defun tjf:frame/display-configuration ()
  "Displays the current frame configuration."
  (interactive)
  (with-output-to-temp-buffer "*frame configuration*"
    (prin1 (current-frame-configuration))))

(defun tjf:frame/exit-buffer-and-frame ()
  "Kill the current buffer and delete its frame."
  (interactive)
  (if (delete-frame-enabled-p)
      (if (kill-buffer (current-buffer))
          (delete-frame)
        (error "Could not exit from current file")) ;; use standard terminology
    (error "Cannot delete this window")))           ;;  for error messages

(defun tjf:frame/make-new ()
  "Make a new frame for the current buffer."
  (interactive)
  (make-frame (append (list (cons 'background-color (tjf:color/random-background-hex))
                            (cons 'width  tjf:frame/default-width)
                            (cons 'height tjf:frame/default-height)))))

(defun tjf:frame/size (format-type)
  "Return the size from FORMAT-TYPE (as a string) of the current frame."
  (format (if (eq format-type 'modeline)
              tjf:frame/format-size-modeline
            tjf:frame/format-size-properties)
          (1- (cdr (assq 'width  (frame-parameters))))
          (-  (cdr (assq 'height (frame-parameters))) 3)))

(defun tjf:frame/font (format-type)
  "Return the font from FORMAT-TYPE (as a string) of the current frame."
  (format (if (eq format-type 'modeline)
              tjf:frame/format-font-modeline
            tjf:frame/format-font-properties)
              (cdr (assq 'font (frame-parameters)))))

(defun tjf:frame/colors (format-type)
  "Return the colors of the current frame from FORMAT-TYPE."
  (format (if (eq format-type 'modeline)
              tjf:frame/format-colors-modeline
            tjf:frame/format-colors-properties)
          (cdr (assq 'background-color (frame-parameters)))
          (cdr (assq 'cursor-color     (frame-parameters)))
          (cdr (assq 'mouse-color      (frame-parameters)))))

(defun tjf:frame/reset-size ()
  "Reset current frame size to default."
  (interactive)
    (if (tjf:flags/is-fullscreen?)
        (toggle-frame-fullscreen)
      (when (tjf:flags/is-maxmized?)
        (toggle-frame-maximized)))
    (set-frame-size (current-frame) tjf:frame/default-width tjf:frame/default-height))

;; (x-display-pixel-width)   ∑monitors
;; (x-display-pixel-height)
;; (frame-char-width)
;; (frame-char-height)
;;
(defun tjf:frame/half-size ()
  "Set frame to occupy half (top to bottom) of the screen."
  (interactive)
  (if (tjf:flags/is-maxmized?)
        (toggle-frame-maximized)
      (if (tjf:flags/is-fullscreen?)
          (toggle-frame-fullscreen)))
    (modify-frame-parameters
     (current-frame) (list (cons 'width  tjf:frame/default-width)
                           (cons 'height tjf:frame/default-height))))

;;
(message "Loading tjf-frame...done")
(provide 'tjf-frame)

;;; tjf-frame.el ends here
