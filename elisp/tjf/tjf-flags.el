;;; tjf-flags.el --- Defintion of boolean flags used in menus and toolbars -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;          Copyright © 2016-2024 Tom Fontaine

;; Author:  Tom Fontaine
;; Date:    26-Jan-2016

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

;; Revision: 03-Feb-2016 changed ‘usr-enable-revert-p’ to not require buffer modification
;;           04-Feb-2016 updated ‘usr-enable-undo-p’ and ‘usr-enable-redo-p’ for new ‘undo-tree’ version
;;           25-Feb-2016 renamed as ‘u-flags’
;;           25-Feb-2016 added ‘is-html-modes’ and ‘is-html-mode?’
;;                       added ‘is-javascript-modes’ and ‘is-javascript-mode?’
;;           16-Dec-2016 added support for mode-line CAPS LOCK indicator
;;           14-Jan-2017 added ‘is-bookmark?’
;;           17-Jan-2017 changed ‘defvar’s to ‘defconst’s
;;           03-Feb-2021 ‘tjf’ overhaul
;;           02-Jul-2021 reworked ‘tjf:flags/enable-undo-redo?’
;;           28-Apr-2022 added ‘tjf:flags/using-tabs’
;;           10-Apr-2023 added ‘is-feature?’
;;           21-Jul-2023 removed modes
;;

;;; Code:

(message "Loading tjf-flags...")
(require 'tjf-mode)
;;
;; enable- functions are for :enable in menu entries

(defun tjf:flags/enable-buffer-operations? ()
  "Boolean: should buffer operations be enabled?"
  (and (not mark-active) (tjf:flags/enable-write?)))

(defun tjf:flags/enable-comment? ()
  "Boolean: should comment functions on region be enabled?"
  (and mark-active comment-start (tjf:flags/is-rw?)))

(defalias 'tjf:flags/enable-encoding? 'tjf:flags/is-not-shell-mode-or-read-only?)

(defun tjf:flags/enable-enriched-mode? ()
  "Boolean: should ‘enriched-mode’?"
  (memq major-mode tjf:flags/enriched-modes))

(defun tjf:flags/enable-modify-region? ()
  "Boolean: should menu entries that change region be enabled?"
  (and mark-active (not buffer-read-only)))

(defun tjf:flags/enable-paste? ()
  "Boolean: should ‘paste’ menu entry be enabled?"
  (and (or
        (and (fboundp 'x-selection-exists-p)
             (x-selection-exists-p 'CLIPBOARD))
        (if (featurep 'ns) ; like paste-from-menu
            (cdr yank-menu)
          kill-ring))
       (not buffer-read-only)))

(defun tjf:flags/enable-revert? ()
  "Boolean: should ‘revert’ be enabled?"
  (or (not (eq revert-buffer-function 'revert-buffer--default))
      (not (eq revert-buffer-insert-file-contents-function 'revert-buffer-insert-file-contents--default-function))
      buffer-file-number))

(defun tjf:flags/enable-save? ()
  "Boolean: should ‘save’ be enabled?"
  (and (buffer-modified-p)
       (buffer-file-name)
       (menu-bar-non-minibuffer-window-p)))

(defun tjf:flags/enable-saveas? ()
  "Boolean: should ‘save as’ be enabled?"
  (and (menu-bar-menu-frame-live-and-visible-p)
       (menu-bar-non-minibuffer-window-p)))

(defun tjf:flags/enable-space-region? ()
  "Boolean: should ‘canonically-space-region’ be enabled?"
  (and mark-active (tjf:flags/is-rw?) (memq major-mode tjf:flags/space-modes)))

(defun tjf:flags/enable-undo-redo? ()
  "Boolean: should ‘undo/redo’ be enabled?"
  (and undo-tree-mode (not buffer-read-only)))

(defalias 'tjf:flags/enable-write? 'tjf:flags/is-rw?)

(defun tjf:flags/is-bookmark? ()
  "Return TRUE if point is located at a bookmark."
  (bm-bookmarkp (bm-bookmark-at (point))))

(defun tjf:flags/is-caps-lock-on? (led-mask)
  "Return t if LED-MASK indicates caps lock is on."
  (eq (logand led-mask 1) 1))

(defun is-feature? (sym)
  "Prints message showing if SYM is a feature."
  (interactive "Mfeature: ")
  (message "%s is %s" sym (if (featurep (intern sym)) "a feature" "NOT a feature")))

(defun tjf:flags/is-fullscreen? ()
  "Boolean: is the current frame fullscreen?"
  (memq (frame-parameter nil (quote fullscreen)) (quote (fullscreen fullboth))))

(defun tjf:flags/is-inside-comment? ()
  "Return non-nil if inside comment, else nil.
This depends on major mode having setup syntax table properly."
  (nth 4 (syntax-ppss)))

(defun tjf:flags/is-inside-string? ()
  "Return non-nil if inside string, else nil.
This depends on major mode having setup syntax table properly."
  (nth 3 (syntax-ppss)))

(defun tjf:flags/is-maxmized? ()
  "Boolean: is the current frame maximized?"
  (memq (frame-parameter nil (quote fullscreen)) (quote (maximized))))

(defun tjf:flags/is-not-fullscreen? ()
  "Boolean: is the current frame not fullscreen?"
  (not (tjf:flags/is-fullscreen?)))

(defun tjf:flags/is-not-shell-mode-or-read-only? ()
  "Boolean: is the current major mode not a shell mode and not read-only?"
  (not (or (tjf:mode/is-shell-mode?) buffer-read-only)))

(defun tjf:flags/is-rw? ()
  "Boolean: is the current buffer read/write?"
  (not buffer-read-only))

(defun tjf:flags/is-tabs-on-line? ()
  "Boolean: are there TABs on the current-line?"
  (save-excursion
    (beginning-of-line)
    (search-forward "\t" (line-end-position) t)))

(defun tjf:flags/is-utf-16be-bom-dos? ()
  "Boolean: is file coding system ‘utf-16be-with-signature-dos’?"
  (eq buffer-file-coding-system 'utf-16be-with-signature-dos))

(defun tjf:flags/is-utf-16be-bom-mac? ()
  "Boolean: is file coding system ‘utf-16be-with-signature-mac’?"
  (eq buffer-file-coding-system 'utf-16be-with-signature-mac))

(defun tjf:flags/is-utf-16be-bom-unix? ()
  "Boolean: is file coding system ‘utf-16be-with-signature-unix’?"
  (eq buffer-file-coding-system 'utf-16be-with-signature-unix))

(defun tjf:flags/is-utf-16le-bom-dos? ()
  "Boolean: is file coding system ‘utf-16le-with-signature-dos’?"
  (eq buffer-file-coding-system 'utf-16le-with-signature-dos))

(defun tjf:flags/is-utf-16le-bom-mac? ()
  "Boolean: is file coding system ‘utf-16le-with-signature-mac’?"
  (eq buffer-file-coding-system 'utf-16le-with-signature-mac))

(defun tjf:flags/is-utf-16le-bom-unix? ()
  "Boolean: is file coding system ‘utf-16le-with-signature-unix’?"
  (eq buffer-file-coding-system 'utf-16le-with-signature-unix))

(defun tjf:flags/is-utf-8-bom-dos? ()
  "Boolean: is file coding system ‘utf-8-with-signature-dos’?"
  (eq buffer-file-coding-system 'utf-8-with-signature-dos))

(defun tjf:flags/is-utf-8-bom-mac? ()
  "Boolean: is file coding system ‘utf-8-with-signature-mac’?"
  (eq buffer-file-coding-system 'utf-8-with-signature-mac))

(defun tjf:flags/is-utf-8-bom-unix? ()
  "Boolean: is file coding system ‘utf-8-with-signature-unix’?"
  (eq buffer-file-coding-system 'utf-8-with-signature-unix))

(defun tjf:flags/is-utf-8-dos? ()
  "Boolean: is file coding system ‘utf-8-dos’?"
  (eq buffer-file-coding-system 'utf-8-dos))

(defun tjf:flags/is-utf-8-mac? ()
  "Boolean: is file coding system ‘utf-8-mac’?"
  (eq buffer-file-coding-system 'utf-8-mac))

(defun tjf:flags/is-utf-8-unix? ()
  "Boolean: is file coding system ‘utf-8-unix’?"
  (eq buffer-file-coding-system 'utf-8-unix))

;; visible- functions are for :visible in menu/toolbar entries

(defalias 'tjf:flags/visible-replace? 'tjf:flags/is-not-shell-mode-or-read-only?)

(defun tjf:flags/visible-convert-to-perl? ()
  "Boolean: should ‘convert-to-perl’ be visible?"
  (or (eq major-mode 'fundamental-mode) (tjf:flags/is-text-mode?)))

(defalias 'tjf:flags/visible-convert-to-python? 'tjf:flags/visible-convert-to-perl?)

(defun tjf:flags/x-led-mask ()
  "Get the current status of the LED mask from X."
  (with-temp-buffer
    (call-process "xset" nil t nil "q")
    ;; ignore the flycheck/flymake warnings below
    (let ((led-mask-string
           (->> (buffer-string)
                s-lines
                (--first (s-contains? "LED mask" it))
                s-split-words
                -last-item)))
      (string-to-number led-mask-string 16))))

;;
(message "Loading tjf-flags...done")
(provide 'tjf-flags)

;;; tjf-flags.el ends here
