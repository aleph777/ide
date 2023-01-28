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

;; Revision: 15-Sep-2000 Added 1 column to c/c++ mode entries in usr-size-alist
;;                       to get a full 80 columns
;;           16-Nov-2001 Revised for Emacs 21
;;           04-Feb-2005 Restored newframe method for manpages
;;           29-May-2008 Add ‘ssh-mode’ to ‘usr-size-alist’
;;           02-Feb-2010 Added exit message
;;           03-Mar-2014 Added ‘clips-mode’, ‘csharp-mode’, and ‘matlab-mode’ to ‘usr-size-alist’
;;           10-Apr-2014 Added ‘nxml-mode’ to ‘usr-size-alist’
;;           06-May-2014 Added log-mode to ‘usr-size-alist’
;;           27-Mar-2015 Added java-mode to ‘usr-size-alist’
;;           30-Mar-2015 Converted ‘current-frame’ to ‘alias selected-frame’
;;           16-Jan-2016 Added ‘makefile-modes’
;;           26-Jan-2016 Added ‘usr-fullscreen-p’ and ‘usr-maxmized-p’
;;           29-Feb-2016 Changed from ‘usr-’ to ‘u-’
;;           02-Mar-2016 Fixed problem with ‘query-frame-colors’
;;                       Added ‘comint-mode’ and ‘shell-mode’ to ‘frame-mode-size-alist’
;;           03-Mar-2016 Added ‘html-mode’, ‘html-helper-mode’, and ‘nxhtml-mode’ to ‘frame-mode-size-alist’
;;                       Added ‘javascript-mode’, ‘js2-mode’, and ‘espresso-mode’ to ‘frame-mode-size-alist’
;;           13-Sep-2016 Changed ‘default-frame-width’ to 120
;;                       Changed ‘alt-frame-width’ to 96
;;           31-Jan-2017 Added ‘json-mode’
;;           11-Jun-2018 Moved color functions to ‘colors.el’
;;           14-Jun-2018 Changed ‘alt-frame-width’ to 112 to accomodate toolbar
;;                       Added new major modes to ‘frame-mode-size-alist’
;;           03-Feb-2021 ‘tjf’ overhaul
;;

;;; Code:

(message "Loading tjf-frame...")
(require 'tjf-flags)
(require 'tjf-color)

;;
(defvar tjf:frame/format-colors-modeline   "Background: %s, Cursor: %s, Mouse: %s")
(defvar tjf:frame/format-colors-properties "Background:  %s\nCursor:      %s\nMouse:       %s\n\n")

(defvar tjf:frame/format-size-modeline   "Window Size: %d colums x %d rows")
(defvar tjf:frame/format-size-properties "Window Size: %d colums, %d rows\n")

(defvar tjf:frame/format-font-modeline   "Font: %s")
(defvar tjf:frame/format-font-properties "Font:        %s\n")

(defvar tjf:frame/default-width  128)
(defvar tjf:frame/default-height  56)

(defvar tjf:frame/alt-width  128)
(defvar tjf:frame/alt-height  40)

(defvar tjf:frame/default-size (list (cons 'width tjf:frame/default-width) (cons 'height tjf:frame/default-height)))
(defvar tjf:frame/alt-size     (list (cons 'width tjf:frame/alt-width)     (cons 'height tjf:frame/alt-height)))

(defvar tjf:frame/mode-size-alist (list (cons 'ada-mode               tjf:frame/default-size)
                                        (cons 'archive-mode           tjf:frame/alt-size)
                                        (cons 'asm-mode               tjf:frame/alt-size)
                                        (cons 'awk-mode               tjf:frame/alt-size)
                                        (cons 'bat-mode               tjf:frame/alt-size)
                                        (cons 'bibtex-mode            tjf:frame/alt-size)
                                        (cons 'bibtex-style-mode      tjf:frame/alt-size)
                                        (cons 'c-mode                 tjf:frame/default-size)
                                        (cons 'csharp-mode            tjf:frame/default-size)
                                        (cons 'c++-mode               tjf:frame/default-size)
                                        (cons 'clips-mode             tjf:frame/default-size)
                                        (cons 'clips-log-mode         tjf:frame/default-size)
                                        (cons 'comint-mode-map        tjf:frame/default-size)
                                        (cons 'cperl-mode             tjf:frame/default-size)
                                        (cons 'css-mode               tjf:frame/alt-size)
                                        (cons 'cuda-mode              tjf:frame/alt-size)
                                        (cons 'Custom-mode            tjf:frame/alt-size)
                                        (cons 'doctex-mode            tjf:frame/alt-size)
                                        (cons 'emacs-lisp-mode        tjf:frame/default-size)
                                        (cons 'espresso-mode          tjf:frame/default-size)
                                        (cons 'fortran-mode           tjf:frame/alt-size)
                                        (cons 'f90-mode               tjf:frame/alt-size)
                                        (cons 'help-mode              tjf:frame/alt-size)
                                        (cons 'html-mode              tjf:frame/default-size)
                                        (cons 'html-helper-mode       tjf:frame/default-size)
                                        (cons 'nxhtml-mode            tjf:frame/default-size)
                                        (cons 'icon-mode              tjf:frame/alt-size)
                                        (cons 'image-mode             tjf:frame/alt-size)
                                        (cons 'indented-text-mode     tjf:frame/alt-size)
                                        (cons 'java-mode              tjf:frame/default-size)
                                        (cons 'javascript-mode        tjf:frame/default-size)
                                        (cons 'js-mode                tjf:frame/default-size)
                                        (cons 'js2-mode               tjf:frame/default-size)
                                        (cons 'json-mode              tjf:frame/default-size)
                                        (cons 'latex-mode             tjf:frame/alt-size)
                                        (cons 'lisp-mode              tjf:frame/default-size)
                                        (cons 'lisp-interaction-mode  tjf:frame/default-size)
                                        (cons 'log-mode               tjf:frame/default-size)
                                        (cons 'lua-mode               tjf:frame/default-size)
                                        (cons 'makefile-automake-mode tjf:frame/default-size)
                                        (cons 'makefile-bsdmake-mode  tjf:frame/default-size)
                                        (cons 'makefile-gmake-mode    tjf:frame/default-size)
                                        (cons 'makefile-imake-mode    tjf:frame/default-size)
                                        (cons 'makefile-makepp-mode   tjf:frame/default-size)
                                        (cons 'makefile-mode          tjf:frame/default-size)
                                        (cons 'Man-mode               tjf:frame/alt-size)
                                        (cons 'matlab-mode            tjf:frame/default-size)
                                        (cons 'nxml-mode              tjf:frame/default-size)
                                        (cons 'org-mode               tjf:frame/alt-size)
                                        (cons 'pascal-mode            tjf:frame/alt-size)
                                        (cons 'perl-mode              tjf:frame/alt-size)
                                        (cons 'perl6-mode             tjf:frame/alt-size)
                                        (cons 'ps-mode                tjf:frame/alt-size)
                                        (cons 'python-mode            tjf:frame/default-size)
                                        (cons 'ruby-mode              tjf:frame/alt-size)
                                        (cons 'scheme-mode            tjf:frame/alt-size)
                                        (cons 'sh-mode                tjf:frame/default-size)
                                        (cons 'shell-mode             tjf:frame/default-size)
                                        (cons 'shell-script-mode      tjf:frame/default-size)
                                        (cons 'sql-mode               tjf:frame/alt-size)
                                        (cons 'tar-mode               tjf:frame/alt-size)
                                        (cons 'tcl-mode               tjf:frame/alt-size)
                                        (cons 'texinfo-mode           tjf:frame/alt-size)
                                        (cons 'text-mode              tjf:frame/alt-size)
                                        (cons 'vhdl-mode              tjf:frame/alt-size)
                                        (cons 'xml-mode               tjf:frame/default-size)))
;;
;; NOTE: frame size is usually set in switch-to-buffer-other-frame but are also set
;;       in ‘after-make-frame-functions’ for emacsclient
;;
(setq after-make-frame-functions
      '(lambda (frame)
         (let* ((mode-size (assq major-mode tjf:frame/mode-size-alist))
                (width     (if mode-size
                               (or (cdr (assq 'width  mode-size)) tjf:frame/alt-width)
                             tjf:frame/alt-width))
                (height    (if mode-size
                               (or (cdr (assq 'height mode-size)) tjf:frame/alt-height)
                             tjf:frame/alt-height)))
           (fringe-mode (cons 8 4))
           (modify-frame-parameters frame (list (cons 'background-color (tjf:color/random-background-hex))
                                                (cons 'width width)
                                                (cons 'height height)
                                                )))))

(defalias 'current-frame 'selected-frame)

;;; overload
(defun mouse-tear-off-window (click)
  "Delete the window clicked on, and create a new frame displaying its buffer."
  (interactive "e")
  (mouse-minibuffer-check click)
  (let* ((window    (posn-window (event-start click)))
         (buf       (window-buffer window))
         (mode-size (assq major-mode tjf:frame/mode-size-alist))                                  ;;; tjf
         (width     (or (cdr (assq 'width  mode-size)) tjf:frame/default-height))                 ;;; tjf
         (height    (or (cdr (assq 'height mode-size)) tjf:frame/default-width))                  ;;; tjf
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
         (pop-up-frame-alist (append (cdr  (assq major-mode tjf:frame/mode-size-alist))             ;;; tjf
                                     (list (cons 'background-color (tjf:color/get-background))))))  ;;; tjf
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
  (make-frame (append (cdr  (assq major-mode tjf:frame/mode-size-alist))
                      (list (cons 'background-color (tjf:color/random-background-hex))))))

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
  (let* ((mode-size (assq major-mode tjf:frame/mode-size-alist))
         (width     (if mode-size
                        (or (cdr (assq 'width  mode-size)) tjf:frame/alt-width)
                      tjf:frame/alt-width))
         (height    (if mode-size
                        (or (cdr (assq 'height mode-size)) tjf:frame/alt-height)
                      tjf:frame/alt-height)))
    (if (tjf:flags/is-fullscreen?)
        (toggle-frame-fullscreen)
      (when (tjf:flags/is-maxmized?)
        (toggle-frame-maximized)))
    (message "columns: %s, rows: %s" width height)
    (set-frame-size (current-frame) width height)))

;;
(message "Loading tjf-frame...done")
(provide 'tjf-frame)

;;; tjf-frame.el ends here
