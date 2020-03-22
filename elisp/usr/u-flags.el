;;; u-flags.el --- Defintion of boolean flags used in menus and toolbars -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;          Copyright © 2016-2020 Tom Fontaine

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

;; Revision: 03-Feb-2016 Changed ‘usr-enable-revert-p’ to not require buffer modification
;;           04-Feb-2016 Updated ‘usr-enable-undo-p’ and ‘usr-enable-redo-p’ for new ‘undo-tree’ version
;;           25-Feb-2016 Renamed as ‘u-flags’
;;           25-Feb-2016 Added ‘is-html-modes’ and ‘is-html-mode?’
;;                       Added ‘is-javascript-modes’ and ‘is-javascript-mode?’
;;           16-Dec-2016 Added support for mode-line CAPS LOCK indicator
;;           14-Jan-2017 Added ‘is-bookmark?’
;;           17-Jan-2017 Changed ‘defvar’s to ‘defconst’s
;;

;;; Code:

(message "Loading u-flags...")
(require 'bm)
(require 'dash)
(require 's)

(defconst shell-modes '(shell-mode
                        ssh-mode
                        eshell-mode))

(defconst make-modes '(makefile-automake-mode
                       makefile-bsdmake-mode
                       makefile-gmake-mode
                       makefile-imake-mode
                       makefile-makepp-mode
                       makefile-mode))

(defconst perl-modes '(cperl-mode
                       perl-mode))

(defconst shell-script-modes '(sh-mode
                               shell-script-mode))

(defconst text-modes '(text-mode
                       indented-text-mode))

(defconst xml-modes '(nxml-mode
                      xml-mode))

(defconst html-modes '(html-mode
                       html-helper-mode
                       nxhtml-mode))

(defconst javascript-modes '(javascript-mode
                             js-mode
                             js2-mode
                             espresso-mode))

(defconst enable-enriched-modes '(fundamental-mode
                                  text-mode
                                  indented-text-mode))

(defconst enable-space-modes '(fundamental-mode
                               text-mode
                               indented-text-mode))

(defun inside-string? ()
  "Return non-nil if inside string, else nil.
This depends on major mode having setup syntax table properly."
  (nth 3 (syntax-ppss)))

(defun inside-comment? ()
  "Return non-nil if inside comment, else nil.
This depends on major mode having setup syntax table properly."
  (nth 4 (syntax-ppss)))

(defun is-shell? ()
  "Boolean: is the current mode a shell mode?"
  (memq major-mode shell-modes))

(defun is-not-shell? ()
  "Boolean: is the current mode not a shell mode?"
  (not (is-shell?)))

(defun is-not-shell-or-read-only? ()
  "Boolean: is the current mode not a shell mode and the buffer
not read only?"
  (not (or (is-shell?) buffer-read-only)))

(defun is-rw? ()
  "Boolean: is the current buffer read/write?"
  (not buffer-read-only))

(defun is-bookmark? ()
  "Return TRUE if point is located at a bookmark."
  (bm-bookmarkp (bm-bookmark-at (point))))

(defun is-make-mode? ()
  "Boolean: is the current major mode a ‘make’ mode?"
  (memq major-mode make-modes))

(defun is-perl-mode? ()
  "Boolean: is the current major mode a ‘perl’ mode?"
  (memq major-mode perl-modes))

(defun is-shell-script-mode? ()
  "Boolean: is the current major mode a ‘shell-script’ mode?"
  (memq major-mode shell-script-modes))

(defun is-text-mode? ()
  "Boolean: is the current major mode a ‘text’ mode?"
  (memq major-mode text-modes))

(defun is-xml-mode? ()
  "Boolean: is the current major mode an ‘xml’ mode?"
  (memq major-mode xml-modes))

(defun is-html-mode? ()
  "Boolean: is the current major mode an ‘html’ mode?"
  (memq major-mode html-modes))

(defun is-javascript-mode? ()
  "Boolean: is the current major mode a ‘javascript’ mode?"
  (memq major-mode javascript-modes))

(defun is-fullscreen? ()
  "Boolean: is the current frame fullscreen?"
  (memq (frame-parameter nil (quote fullscreen)) (quote (fullscreen fullboth))))

(defun is-not-fullscreen? ()
  "Boolean: is the current frame not fullscreen?"
  (not (is-fullscreen?)))

(defun is-maxmized? ()
  "Boolean: is the current frame maximized?"
  (memq (frame-parameter nil (quote fullscreen)) (quote (maximized))))

(defun enable-modify-region? ()
  "Boolean: should changes to region be enabled?"
  (and mark-active (not buffer-read-only)))

(defun enable-space-region? ()
  "Boolean: should ‘canonically-space-region’ be enabled?"
  (and mark-active (is-rw?) (memq major-mode enable-space-modes)))

(defun enable-paste? ()
  "Boolean: should ‘paste’ be enabled?"
  (and (or
        (and (fboundp 'x-selection-exists-p)
             (x-selection-exists-p 'CLIPBOARD))
        (if (featurep 'ns) ; like paste-from-menu
            (cdr yank-menu)
          kill-ring))
       (not buffer-read-only)))

(defun enable-revert? ()
  "Boolean: should ‘revert’ be enabled?"
  (or (not (eq revert-buffer-function
               'revert-buffer--default))
      (not (eq
            revert-buffer-insert-file-contents-function
            'revert-buffer-insert-file-contents--default-function))
      buffer-file-number))

(defun enable-save? ()
  "Boolean: should ‘save’ be enabled?"
  (and (buffer-modified-p)
       (buffer-file-name)
       (menu-bar-non-minibuffer-window-p)))

(defun enable-saveas? ()
  "Boolean: should ‘save as’ be enabled?"
  (and (menu-bar-menu-frame-live-and-visible-p)
       (menu-bar-non-minibuffer-window-p)))

(defun enable-undo? ()
  "Boolean: should ‘undo’ be enabled?"
  (and (not buffer-read-only)
       (not (eq t buffer-undo-list))))

(defun enable-redo? ()
  "Boolean: should ‘redo’ be enabled?"
  (and (not buffer-read-only)
       (not (eq t buffer-undo-list))))

(defun enable-comment? ()
  "Boolean: should comment functions on region be enabled?"
  (and mark-active comment-start (is-rw?)))

(defun enable-encoding? ()
  "Boolean: should ‘encoding’ be enabled?"
  (is-not-shell-or-read-only?))

(defun enable-enriched-mode? ()
  "Boolean: should ‘enriched-mode’?"
  (memq major-mode enable-enriched-modes))

(defun tabs-on-line? ()
  "Returns non-nil if are there TABs on the current-line?"
  (save-excursion
    (beginning-of-line)
    (search-forward "\t" (line-end-position) t)))

(defun visible-replace? ()
  "Boolean: should ‘replace’ be visible?"
  (is-not-shell-or-read-only?))

(defun visible-lock? ()
  "Boolean: should ‘lock’ be visible?"
  (and (is-not-shell?) (not buffer-read-only)))

(defun visible-unlock? ()
  "Boolean: should ‘unlock’ be visible?"
  (and (is-not-shell?) buffer-read-only))

(defun visible-convert-to-perl? ()
  "Boolean: should ‘convert-to-perl’ be visible?"
  (or (eq major-mode 'fundamental-mode) (is-text-mode?)))

(defun is-utf-8-unix? ()
  "Boolean: is file coding system ‘utf-8-unix’?"
  (eq buffer-file-coding-system 'utf-8-unix))

(defun is-utf-8-dos? ()
  "Boolean: is file coding system ‘utf-8-dos’?"
  (eq buffer-file-coding-system 'utf-8-dos))

(defun is-utf-8-mac? ()
  "Boolean: is file coding system ‘utf-8-mac?’"
  (eq buffer-file-coding-system 'utf-8-mac))

(defun is-utf-8-bom-unix? ()
  "Boolean: is file coding system ‘utf-8-with-signature-unix’?"
  (eq buffer-file-coding-system 'utf-8-with-signature-unix))

(defun is-utf-8-bom-dos? ()
  "Boolean: is file coding system ‘utf-8-with-signature-dos?’"
  (eq buffer-file-coding-system 'utf-8-with-signature-dos))

(defun is-utf-8-bom-mac? ()
  "Boolean: is file coding system ‘utf-8-with-signature-mac’?"
  (eq buffer-file-coding-system 'utf-8-with-signature-mac))

(defun is-utf-16be-bom-unix? ()
  "Boolean: is file coding system ‘utf-16be-with-signature-unix’?"
  (eq buffer-file-coding-system 'utf-16be-with-signature-unix))

(defun is-utf-16be-bom-dos? ()
  "Boolean: is file coding system ‘utf-16be-with-signature-dos’?"
  (eq buffer-file-coding-system 'utf-16be-with-signature-dos))

(defun is-utf-16be-bom-mac? ()
  "Boolean: is file coding system ‘utf-16be-with-signature-mac’?"
  (eq buffer-file-coding-system 'utf-16be-with-signature-mac))

(defun is-utf-16le-bom-unix? ()
  "Boolean: is file coding system ‘utf-16le-with-signature-unix’?"
  (eq buffer-file-coding-system 'utf-16le-with-signature-unix))

(defun is-utf-16le-bom-dos? ()
  "Boolean: is file coding system ‘utf-16le-with-signature-dos’?"
  (eq buffer-file-coding-system 'utf-16le-with-signature-dos))

(defun is-utf-16le-bom-mac? ()
  "Boolean: is file coding system ‘utf-16le-with-signature-mac’?"
  (eq buffer-file-coding-system 'utf-16le-with-signature-mac))

(defun x-led-mask ()
  "Get the current status of the LED mask from X."
  (with-temp-buffer
    (call-process "xset" nil t nil "q")
    (let ((led-mask-string
           (->> (buffer-string)
                s-lines
                (--first (s-contains? "LED mask" it))
                s-split-words
                -last-item)))
      (string-to-number led-mask-string 16))))

(defun is-caps-lock-on? (led-mask)
  "Return non-nil if LED-MASK indicates caps lock is on."
  (eq (logand led-mask 1) 1))
;;
(message "Loading u-flags...done")
(provide 'u-flags)

;;; u-flags.el ends here
