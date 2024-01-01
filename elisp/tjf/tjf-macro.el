;;; tjf-macro.el --- Global macro definitions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2024 Tom Fontaine

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

;; Revision: 18-Sep-2016 Added ‘alias-face’
;;           14-Jan-2017 Added ‘get-shell-command’
;;           16-Jan-2017 Added ‘on-*’
;;           17-Jan-2017 Fixed ‘with-word-or-region’
;;                       Added ‘paragraph-beginning-position’, ‘paragraph-end-position’, and ‘with-paragraph-or-region’
;;           28-Apr-2017 Added ‘basename*’ and ‘dirname’
;;           17-Apr-2018 Added ‘diminish-modelist’
;;           27-Jun-2019 Added ‘append-to-list’
;;           03-Feb-2021 ‘tjf’ overhaul
;;           11-Mar-2021 Added ‘looking-at-word-or-symbol’
;;                       Updated ‘word-beginning-position’ and ‘word-end-position’ to look for variables and function names
;;           01-Oct-2022 Added ‘symbol-to-string’ and ‘string-to-symbol’
;;

;;; Code:

(message "Loading tjf-macro...")
(require 'diminish)
(require 's)

;; aliases, not macros
(defalias 'join  's-join)
(defalias 'split 's-split)

;; macros

(defvar word-symbol-syntax     "w_")
(defvar not-word-symbol-syntax (concat "^" word-symbol-syntax))

(defmacro alias-face (name face)
  "Create an alias ‘NAME’ to face ‘FACE’."
  `(progn (defface ,name '((default :inherit ,face :slant r))
            "A macro-defined alias face."
            :group 'default)
          (defvar ,name ',name)))

(defmacro append-to-list (target suffix)
  "Append SUFFIX to TARGET in place."
  `(setq ,target (append ,target ,suffix)))

(defmacro get-shell-output (body)
  "Return the output of BODY minus the line terminator."
  (nth 0 (split-string (shell-command-to-string ,@body) nil)))

(defmacro on-cygwin (statement &rest statements)
  "Evaluate the enclosed body (STATEMENT & STATEMENTS) only when run on Cygwin."
  `(when is-cygwin?
     ,statement
     ,@statements))

(defmacro on-daemon (statement &rest statements)
  "Evaluate the enclosed body (STATEMENT & STATEMENTS) only when run
as Emacs daemon."
  `(when is-daemon?
     ,statement
     ,@statements))

(defmacro on-gnu/linux (statement &rest statements)
  "Evaluate the enclosed body (STATEMENT & STATEMENTS) only when run
on GNU/Linux."
  `(when is-linux?
     ,statement
     ,@statements))

(defmacro on-gnu/linux-gui (statement &rest statements)
  "Evaluate the enclosed body (STATEMENT & STATEMENTS) only when run
on a GNU/Linux GUI."
  `(when is-linux-gui?
     ,statement
     ,@statements))

(defmacro on-gui (statement &rest statements)
  "Evaluate the enclosed body (STATEMENT & STATEMENTS) only when run on GUI."
  `(when is-gui?
     ,statement
     ,@statements))

(defmacro on-macos (statement &rest statements)
  "Evaluate the enclosed body (STATEMENT & STATEMENTS) only when run
on Mac OS."
  `(when is-macos?
     ,statement
     ,@statements))

(defmacro on-windows (statement &rest statements)
  "Evaluate the enclosed body (STATEMENT & STATEMENTS) only when run
on Microsoft Windows."
  `(when is-windows
     ,statement
     ,@statements))

(defmacro on-not-gui (statement &rest statements)
  "Evaluate the enclosed body (STATEMENT & STATEMENTS) only when run on GUI."
  `(when (not is-gui?)
     ,statement
     ,@statements))

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

(defmacro with-paragraph-or-region (args &rest body)
  "Determine ‘paragraph’ or ‘region’ using ARGS.  Execute BODY over chosen area.
This macro replaces similar code using (interactive \"r\"), which can fail
when there is no mark set.

\(fn (BEG END) BODY...)"
  `(let ((,(car  args) (if (use-region-p) (region-beginning) (paragraph-beginning-position)))
         (,(cadr args) (if (use-region-p) (region-end)       (paragraph-end-position))))
     ,@body))

(defmacro with-word-or-region (args &rest body)
  "Determine ‘word’ or ‘region’ using ARGS.  Execute BODY over chosen area.
This macro replaces similar code using (interactive \"r\"), which can fail
when there is no mark set.

\(fn (BEG END) BODY...)"
  `(let ((,(car  args) (if (use-region-p) (region-beginning) (word-beginning-position)))
         (,(cadr args) (if (use-region-p) (region-end)       (word-end-position))))
     ,@body))

;; defsubsts

(defsubst diminish-mode-list (lighter &rest modes)
  "Set LIGHTER for  each one of MODES."
  (dolist (mode modes t) (diminish mode lighter)))

(defsubst hook-into-modes (func &rest modes)
  "Add FUNC to each one of MODES."
  (dolist (mode-hook modes t) (add-hook mode-hook func)))

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

(defsubst replace-matches (regexp replacement-string &optional point-end)
  "Replace all occurences of REGEXP with REPLACEMENT-STRING from point
to POINT-END (defaults to ‘point-max’)."
  (while (re-search-forward regexp (or point-end (point-max)) t)
      (replace-match replacement-string t)))

(defsubst string-to-symbol (string)
  "Convert STRING to a symbol."
  (intern string))

(defsubst symbol-to-string (symbol)
  "Convert SYMBOL to a string."
  (symbol-name 'symbol))

(defsubst replace-matches-nre (str replacement-string &optional point-end)
  "Replace all occurences of STR with REPLACEMENT-STRING from point to
POINT-END (defaults to ‘point-max’)."
  (while (search-forward str (or point-end (point-max)) t)
      (replace-match replacement-string t)))

(defun looking-at-word-or-symbol ()
  "Return t if character after point is a word character or a symbol character."
  (let ((syntax (string (char-syntax (char-after)))))
    (or (equal syntax "w") (equal syntax "_"))))

(defun word-beginning-position ()
  "Return the character position of the first character of the current word."
  (interactive)
  (save-excursion
    (if (looking-at-word-or-symbol)
        (skip-syntax-backward word-symbol-syntax)
      (skip-syntax-forward not-word-symbol-syntax))
    (point)))

(defun word-end-position ()
  "Return the character position of the end of the current word."
  (interactive)
  (save-excursion
    (if (looking-at-word-or-symbol)
        (skip-syntax-forward word-symbol-syntax)
      (skip-syntax-forward not-word-symbol-syntax)
      (skip-syntax-forward word-symbol-syntax))
    (point)))
;;
(message "Loading tjf-macro...done")
(provide 'tjf-macro)

;;; tjf-macro.el ends here
