;;; u-frame.el --- Functions that control frame appearance -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 1999-2021 Tom Fontaine

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
;;

;;; Code:

(message "Loading u-frame...")
(require 'u-flags)
(require 'colors)
;;
(defvar format-frame-colors-modeline   "Background: %s, Cursor: %s, Mouse: %s")
(defvar format-frame-colors-properties "Background:  %s\nCursor:      %s\nMouse:       %s\n\n")

(defvar format-frame-size-modeline   "Window Size: %d colums x %d rows")
(defvar format-frame-size-properties "Window Size: %d colums, %d rows\n")

(defvar format-frame-font-modeline   "Font: %s")
(defvar format-frame-font-properties "Font:        %s\n")

(defvar default-frame-width  120)
(defvar default-frame-height  40)

(defvar alt-frame-width  112)
(defvar alt-frame-height 40)

(defvar default-frame-size (list (cons 'width default-frame-width) (cons 'height default-frame-height)))
(defvar alt-frame-size     (list (cons 'width alt-frame-width)     (cons 'height alt-frame-height)))

(defvar frame-mode-size-alist (list (cons 'ada-mode               default-frame-size)
                                    (cons 'archive-mode           alt-frame-size)
                                    (cons 'asm-mode               alt-frame-size)
                                    (cons 'awk-mode               alt-frame-size)
                                    (cons 'bat-mode               alt-frame-size)
                                    (cons 'bibtex-mode            alt-frame-size)
                                    (cons 'bibtex-style-mode      alt-frame-size)
                                    (cons 'c-mode                 default-frame-size)
                                    (cons 'csharp-mode            default-frame-size)
                                    (cons 'c++-mode               default-frame-size)
                                    (cons 'clips-mode             default-frame-size)
                                    (cons 'clips-log-mode         default-frame-size)
                                    (cons 'comint-mode-map        default-frame-size)
                                    (cons 'cperl-mode             default-frame-size)
                                    (cons 'css-mode               alt-frame-size)
                                    (cons 'cuda-mode              alt-frame-size)
                                    (cons 'Custom-mode            alt-frame-size)
                                    (cons 'doctex-mode            alt-frame-size)
                                    (cons 'emacs-lisp-mode        default-frame-size)
                                    (cons 'espresso-mode          default-frame-size)
                                    (cons 'fortran-mode           alt-frame-size)
                                    (cons 'f90-mode               alt-frame-size)
                                    (cons 'help-mode              alt-frame-size)
                                    (cons 'html-mode              default-frame-size)
                                    (cons 'html-helper-mode       default-frame-size)
                                    (cons 'nxhtml-mode            default-frame-size)
                                    (cons 'icon-mode              alt-frame-size)
                                    (cons 'image-mode             alt-frame-size)
                                    (cons 'indented-text-mode     alt-frame-size)
                                    (cons 'java-mode              default-frame-size)
                                    (cons 'javascript-mode        default-frame-size)
                                    (cons 'js-mode                default-frame-size)
                                    (cons 'js2-mode               default-frame-size)
                                    (cons 'json-mode              default-frame-size)
                                    (cons 'latex-mode             alt-frame-size)
                                    (cons 'lisp-mode              default-frame-size)
                                    (cons 'lisp-interaction-mode  default-frame-size)
                                    (cons 'log-mode               default-frame-size)
                                    (cons 'lua-mode               default-frame-size)
                                    (cons 'makefile-automake-mode default-frame-size)
                                    (cons 'makefile-bsdmake-mode  default-frame-size)
                                    (cons 'makefile-gmake-mode    default-frame-size)
                                    (cons 'makefile-imake-mode    default-frame-size)
                                    (cons 'makefile-makepp-mode   default-frame-size)
                                    (cons 'makefile-mode          default-frame-size)
                                    (cons 'Man-mode               alt-frame-size)
                                    (cons 'matlab-mode            default-frame-size)
                                    (cons 'nxml-mode              default-frame-size)
                                    (cons 'org-mode               alt-frame-size)
                                    (cons 'pascal-mode            alt-frame-size)
                                    (cons 'perl-mode              alt-frame-size)
                                    (cons 'perl6-mode             alt-frame-size)
                                    (cons 'ps-mode                alt-frame-size)
                                    (cons 'python-mode            default-frame-size)
                                    (cons 'ruby-mode              alt-frame-size)
                                    (cons 'scheme-mode            alt-frame-size)
                                    (cons 'sh-mode                default-frame-size)
                                    (cons 'shell-mode             default-frame-size)
                                    (cons 'shell-script-mode      default-frame-size)
                                    (cons 'sql-mode               alt-frame-size)
                                    (cons 'tar-mode               alt-frame-size)
                                    (cons 'tcl-mode               alt-frame-size)
                                    (cons 'texinfo-mode           alt-frame-size)
                                    (cons 'text-mode              alt-frame-size)
                                    (cons 'vhdl-mode              alt-frame-size)
                                    (cons 'xml-mode               default-frame-size)))
;;
;; NOTE: frame size is usually set in switch-to-buffer-other-frame but are also set
;;       in ‘after-make-frame-functions’ for emacsclient
;;
(setq after-make-frame-functions
      '(lambda (frame)
         (let* ((mode-size (assq major-mode frame-mode-size-alist))
                (width     (if mode-size
                               (or (cdr (assq 'width  mode-size)) alt-frame-width)
                             alt-frame-width))
                (height    (if mode-size
                               (or (cdr (assq 'height mode-size)) alt-frame-height)
                             alt-frame-height)))
           (fringe-mode (cons 8 4))
           (modify-frame-parameters frame (list (cons 'background-color (random-background-color))
                                                (cons 'width width)
                                                (cons 'height height)
                                                )))))

(defalias 'current-frame 'selected-frame)

(defun make-new-frame ()
  "Make a new frame for the current buffer."
  (interactive)
  (make-frame (append (cdr (assq major-mode frame-mode-size-alist))
                      (list (cons 'background-color (random-background-color))))))

(defun reset-frame-size ()
  "Reset current frame size to default."
  (interactive)
  (let* ((mode-size (assq major-mode frame-mode-size-alist))
         (width     (if mode-size
                        (or (cdr (assq 'width  mode-size)) alt-frame-width)
                      alt-frame-width))
         (height    (if mode-size
                        (or (cdr (assq 'height mode-size)) alt-frame-height)
                      alt-frame-height)))
    (if (is-fullscreen?)
        (toggle-frame-fullscreen)
      (when (is-maxmized?)
        (toggle-frame-maximized)))
    (message "columns: %s, rows: %s" width height)
    (set-frame-size (current-frame) width height)))

(defun display-frame-parameters ()
  "Displays the parameters of the current frame."
  (interactive)
  (with-output-to-temp-buffer "*frame configuration*"
    (prin1 (frame-parameters))))

(defun display-frame-configuration ()
  "Displays the current frame configuration."
  (interactive)
  (with-output-to-temp-buffer "*frame configuration*"
    (prin1 (current-frame-configuration))))

(defun query-frame-size (format-type)
  "Return the size from FORMAT-TYPE (as a string) of the current frame."
  (format (if (eq format-type 'modeline)
              format-frame-size-modeline
            format-frame-size-properties)
          (1- (cdr (assq 'width  (frame-parameters))))
          (-  (cdr (assq 'height (frame-parameters))) 3)))

(defun query-frame-font (format-type)
  "Return the font from FORMAT-TYPE (as a string) of the current frame."
  (format (if (eq format-type 'modeline)
              format-frame-font-modeline
            format-frame-font-properties)
              (cdr (assq 'font (frame-parameters)))))

(defun query-frame-colors (format-type)
  "Return the colors of the current frame from FORMAT-TYPE."
  (format (if (eq format-type 'modeline)
              format-frame-colors-modeline
            format-frame-colors-properties)
          (cdr (assq 'background-color (frame-parameters)))
          (cdr (assq 'cursor-color     (frame-parameters)))
          (cdr (assq 'mouse-color      (frame-parameters)))))

(defun exit-buffer-and-frame ()
  "Kill the current buffer and delete its frame."
  (interactive)
  (if (delete-frame-enabled-p)
      (if (kill-buffer (current-buffer))
          (delete-frame)
        (error "Could not exit from current file")) ;; use standard terminology
    (error "Cannot delete this window")))           ;;  for error messages
;;
(message "Loading u-frame...done")
(provide 'u-frame)

;;; u-frame.el ends here
