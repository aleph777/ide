;;; u-macro.el --- Global macro definitions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2018 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   10-Jun-2016

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

;; Revision:    18-Sep-2016 Added ‘alias-face’
;;              14-Jan-2017 Added ‘get-shell-command’
;;              16-Jan-2017 Added ‘on-*’
;;              17-Jan-2017 Fixed ‘with-word-or-region’
;;                          Added ‘paragraph-beginning-position’, ‘paragraph-end-position’, and ‘with-paragraph-or-region’
;;              28-Apr-2017 Added ‘basename*’ and ‘dirname’
;;              17-Apr-2018 Added ‘diminish-modelist’
;;

;;; Code:

;;
(message "Loading u-macro...")
;;
(require 'diminish)

;; (on-osx
;;  (setq mac-control-modifier 'control)
;;  (setq mac-command-modifier 'meta)
;;  (setq mac-option-modifier 'super))
;;
(defmacro on-cygwin (statement &rest statements)
  "Evaluate the enclosed body (STATEMENT & STATEMENTS) only when run on Cygwin."
  `(when (eq system-type 'cygwin)
     ,statement
     ,@statements))

(defmacro on-gnu/linux (statement &rest statements)
  "Evaluate the enclosed body (STATEMENT & STATEMENTS) only when run on GNU/Linux."
  `(when (eq system-type 'gnu/linux)
     ,statement
     ,@statements))

(defmacro on-gnu/linux-gui (statement &rest statements)
  "Evaluate the enclosed body (STATEMENT & STATEMENTS) only when run on GNU/Linux."
  `(when (and (eq system-type 'gnu/linux) (display-graphic-p))
     ,statement
     ,@statements))

(defmacro on-windows (statement &rest statements)
  "Evaluate the enclosed body (STATEMENT & STATEMENTS) only when run on Microsoft Windows."
  `(when (eq system-type 'windows-nt)
     ,statement
     ,@statements))

(defmacro on-daemon (statement &rest statements)
  "Evaluate the enclosed body (STATEMENT & STATEMENTS) only when run as Emacs daemon."
  `(when (daemonp)
     ,statement
     ,@statements))

(defmacro on-gui (statement &rest statements)
  "Evaluate the enclosed body (STATEMENT & STATEMENTS) only when run on GUI."
  `(when (display-graphic-p)
     ,statement
     ,@statements))

(defmacro not-on-gui (statement &rest statements)
  "Evaluate the enclosed body (STATEMENT & STATEMENTS) only when run on GUI."
  `(when (not (display-graphic-p))
     ,statement
     ,@statements))

(defmacro alias-face (name face)
  "Create an alias ‘NAME’ to face ‘FACE’."
  `(progn (defface ,name '((default :inherit ,face :slant r))
            "A macro-defined alias face."
            :group 'default)
          (defvar ,name ',name)))

(defmacro with-buffer-or-region (args &rest body)
  "Determine ‘buffer’ or ‘region’ using ARGS.  Execute BODY over chosen area.
This macro replaces similar code using (interactive \"r\"), which can fail
when there is no mark set.

\(fn (BEG END) BODY...)"
  `(let ((,(car  args) (if (use-region-p) (region-beginning) (point-min)))
         (,(cadr args) (if (use-region-p) (region-end)       (point-max))))
     ,@body))

(defmacro with-line-or-region (args &rest body)
  "Determine ‘line’ or ‘region’ using ARGS.  Execute BODY over chosen area.
This macro replaces similar code using (interactive \"r\"), which can fail
when there is no mark set.

\(fn (BEG END) BODY...)"
  `(let ((,(car  args) (if (use-region-p) (region-beginning) (line-beginning-position)))
         (,(cadr args) (if (use-region-p) (region-end)       (line-beginning-position 2))))
     ,@body))

(defmacro with-word-or-region (args &rest body)
  "Determine ‘word’ or ‘region’ using ARGS.  Execute BODY over chosen area.
This macro replaces similar code using (interactive \"r\"), which can fail
when there is no mark set.

\(fn (BEG END) BODY...)"
  `(let ((,(car  args) (if (use-region-p) (region-beginning) (word-beginning-position)))
         (,(cadr args) (if (use-region-p) (region-end)       (word-end-position))))
     ,@body))

(defmacro with-paragraph-or-region (args &rest body)
  "Determine ‘paragraph’ or ‘region’ using ARGS.  Execute BODY over chosen area.
This macro replaces similar code using (interactive \"r\"), which can fail
when there is no mark set.

\(fn (BEG END) BODY...)"
  `(let ((,(car  args) (if (use-region-p) (region-beginning) (paragraph-beginning-position)))
         (,(cadr args) (if (use-region-p) (region-end)       (paragraph-end-position))))
     ,@body))

(defsubst hook-into-modes (func &rest modes)
  "Add FUNC to each one of MODES."
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(defsubst diminish-mode-list (lighter &rest modes)
  "Set LIGHTER for  each one of MODES."
  (dolist (mode modes) (diminish mode lighter)))

(defsubst word-beginning-position ()
  "Return the character position of the first character of the current word."
  (save-excursion
    (skip-chars-backward "[:alnum:]")
    (point)))

(defsubst word-end-position ()
  "Return the character position of the end of the current word."
  (save-excursion
    (skip-chars-forward "[:alnum:]")
    (point)))

(defsubst paragraph-beginning-position ()
  "Return the character position of the first character of the current paragraph."
  (save-excursion
    (backward-paragraph)
    (point)))

(defsubst paragraph-end-position ()
  "Return the character position of the end of the current paragraph."
  (save-excursion
    (forward-paragraph)
    (point)))

(defsubst get-shell-command (command)
  "Return the output of COMMAND minus the line terminator."
  (nth 0 (split-string (shell-command-to-string command) nil)))

(defsubst basename-full (&optional filename)
  "Return the base name of the FILENAME: no directory, with extension.
FILENAME defaults to `buffer-file-name'."
  (file-name-nondirectory (or filename (buffer-file-name))))

(defsubst dirname (&optional filename)
  "Return the directory of FILENAME. FILENAME defaults to the current buffer file name."
  (file-name-directory (or filename (buffer-file-name))))

(defsubst file-extension (&optional filename)
  "Return the extension of the FILENAME. FILENAME defaults to the current buffer file name."
  (file-name-extension (or filename (buffer-file-name))))

(defalias 'basename 'file-name-base)
;;
(message "Loading u-macro...done")
(provide 'u-macro)

;;; u-macro.el ends here
