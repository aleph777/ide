;;; u-toolbar.el --- Emacs toolbar revision -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;              Copyright © 2001-2017 Tom Fontaine

;;
;; Author:      Tom Fontaine
;; Date:        30-Nov-2001
;; Time-stamp: <26-Apr-2017 12:26:01 EDT, modified by Tom Fontaine>
;;

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

;; Revision: 09-May-2006 upgraded for ssh
;;                       added visible/enable functions
;;           11-May-2008 revised for Emacs 22
;;           13-Feb-2010 Added exit message
;;                       Changed goto-line icon name
;;           31-Aug-2012 Reworked for new icon set
;;                       Added PageUp and PageDown
;;           20-Sep-2012 Reworked usr-paste-p for MS-Windows
;;           04-Jan-2013 Added eshell-mode to shell-p
;;           04-Jun-2013 Added separators and null labels
;;           30-May-2014 Added button for dabbrev-expand/lisp-complete-symbol
;;           11-Jan-2016 Changed lisp-complete-symbol to completion-at-point
;;           18-Jan-2016 Updated for new user interface
;;           26-Jan-2016 Moved internal functions to usr-flags
;;           22-Feb-2016 New icons
;;                       Add "New file", "Lock/Unlock", and "Hide/Show"
;;           26-Jan-2016 Changed to require ‘u-flags’, ‘u-navigate’, and ‘u-search’
;;           09-May-2016 Changed ‘query-replace’ and ‘query-replace-regexp’ with ‘anzu’ versions
;;           17-Nov-2016 Changed first icon to ‘view-file’
;;           05-Jan-2017 Changed to Material icons
;;           13-Jan-2017 Added ‘winner-undo’ amd ‘winner-redo’
;;                       Added bookmark support
;;           16-Jan-2017 Removed ‘tinysearch-search-word-*’
;;           26-Apr-2017 Added ‘previous-input’ and ‘next-input’
;;

;;; Code:

;;
(message "Loading u-toolbar...")
(require 'u-flags)
(require 'u-frame)
(require 'u-navigate)
(require 'u-search)
;;
(setq tool-bar-map (make-sparse-keymap))

;;
;; File/Buffer operations
;;
(tool-bar-add-item "mi-view"  'view-file        'view  :label "" :help "View file/Toggle View mode")
(tool-bar-add-item "mi-new"   'new-empty-buffer 'new   :label "" :help "New file")
(tool-bar-add-item "mi-edit"  'find-file        'open  :label "" :help "Open file/Open file in new window...")
(tool-bar-add-item "mi-close" 'kill-this-buffer 'close :label "" :help "Discard current buffer/Discard current buffer & window" :visible '(is-not-shell?))
(define-key-after (default-value 'tool-bar-map) [separator-1] menu-bar-separator)

(tool-bar-add-item "mi-lock"   'read-only-mode   'lock   :label "" :help "Toggle read-only" :visible '(visible-lock?))
(tool-bar-add-item "mi-unlock" 'read-only-mode   'unlock :label "" :help "Toggle read-only" :visible '(visible-unlock?))
(tool-bar-add-item "mi-save"   'save-buffer      'save   :label "" :help "Save buffer"      :visible '(is-not-shell?) :enable '(enable-save?) )
(tool-bar-add-item "mi-saveas" 'write-file       'saveas :label "" :help "Save buffer as/Write region...")
(tool-bar-add-item "mi-revert" 'revert-buffer    'revert :label "" :help "Revert buffer"    :visible '(is-not-shell?) :enable '(enable-revert?))
(define-key-after (default-value 'tool-bar-map) [separator-2] menu-bar-separator)
;;
;; Undo/Redo
;;
(tool-bar-add-item "mi-undo" 'undo 'undo :label "" :help "Undo last operation" :enable '(enable-undo?))
(tool-bar-add-item "mi-redo" 'redo 'redo :label "" :help "Redo last undo"      :enable '(enable-redo?))
(define-key-after (default-value 'tool-bar-map) [separator-3] menu-bar-separator)
;;
;; Cut/Copy/Paste
;;
(tool-bar-add-item "mi-cut"   'kill-region    'cut   :enable '(enable-modify-region?) :label "" :help "Cut/Cut Rectangle")

(tool-bar-add-item "mi-copy"  'kill-ring-save 'copy  :enable 'mark-active             :label "" :help "Copy/Copy Rectangle")
(tool-bar-add-item "mi-paste" 'yank           'paste :enable '(enable-paste?)         :label "" :help "Paste/Paste Rectangle")
(define-key-after (default-value 'tool-bar-map) [separator-4] menu-bar-separator)
;;
;; Cursor movement
;;
(tool-bar-add-item "mi-home"     'beginning-of-buffer  'home      :label "" :help "Home")
(tool-bar-add-item "mi-end"      'end-of-buffer        'end       :label "" :help "End")
(tool-bar-add-item "mi-up"       'scroll-down-command  'page-up   :label "" :help "Page Up")
(tool-bar-add-item "mi-down"     'scroll-up-command    'page-down :label "" :help "Page Down")
(tool-bar-add-item "mi-left"     'search-word-backward 'backward  :label "" :help "Search backward/Previous bookmark")
(tool-bar-add-item "mi-right"    'search-word-forward  'forward   :label "" :help "Search forward/Next bookmark")
(tool-bar-add-item "mi-goto"     'goto-line            'goto      :label "" :help "Goto line.../Saved point")
(tool-bar-add-item "mi-bookmark" 'bm-toggle            'bookmark  :label "" :help "Bookmark toggle" :visible '(is-not-shell?))
(define-key-after (default-value 'tool-bar-map) [separator-5] menu-bar-separator)
;;
;; Misc
;;
(tool-bar-add-item "mi-repeat"  'repeat-complex-command 'repeat  :label "" :help "Repeat Command...")
;; (tool-bar-add-item "00-hide"    'hs-hide-block          'hide    :label "" :help "Hide block" :visible 'hs-minor-mode)
;; (tool-bar-add-item "00-show"    'hs-show-block          'show    :label "" :help "Show block" :visible 'hs-minor-mode)
(tool-bar-add-item "mi-find"    'u-occur                'search  :label "" :help "Show matching lines...")
(tool-bar-add-item "mi-replace" 'anzu-query-replace     'replace :label "" :help "Find & replace/Find & replace regexp..." :visible '(visible-replace?))
(define-key-after  (default-value 'tool-bar-map) [separator-6] menu-bar-separator)

(tool-bar-add-item "mi-zoom-in"  'text-scale-increase 'zoom-in  :label "")
(tool-bar-add-item "mi-zoom-out" 'text-scale-decrease 'zoom-out :label "")

;; (tool-bar-add-item "00-expand" 'dabbrev-expand 'expand :label "" :help  "Expand abbrev/Complete Symbol" :visible '(is-rw?))
;;
;; Comint functions
;;
(defsubst previous-input ()
  "Retrieve previous input without the drama."
  (interactive)
  (if (not (= (point) (point-max)))
      (goto-char (point-max)))
  (comint-previous-input 1))

(defsubst next-input ()
  "Retrieve next input without the drama."
  (interactive)
  (if (not (= (point) (point-max)))
      (goto-char (point-max)))
  (comint-previous-input 1))

(tool-bar-add-item "mi-previous" 'previous-input 'up-arrow   :visible '(is-shell?) :label "" :help "Previous input")
(tool-bar-add-item "mi-next"     'next-input     'down-arrow :visible '(is-shell?) :label "" :help "Next input")
(tool-bar-add-item "mi-cancel"   'comint-delete-output  'cancel     :visible '(is-shell?) :label "" :help "Flush output")
;;
;; Control
;;
(define-key tool-bar-map [(control view)]      'view-mode)
(define-key tool-bar-map [(control open)]      'find-file-other-frame)
(define-key tool-bar-map [(control close)]     'exit-buffer-and-frame)
(define-key tool-bar-map [(control saveas)]    'write-region)
(define-key tool-bar-map [(control undo)]      'winner-undo)
(define-key tool-bar-map [(control redo)]      'winner-redo)
(define-key tool-bar-map [(control cut)]       'kill-rectangle)
(define-key tool-bar-map [(control copy)]      'copy-rectangle-as-kill)
(define-key tool-bar-map [(control paste)]     'yank-rectangle)
(define-key tool-bar-map [(control goto)]      'goto-saved-point)
(define-key tool-bar-map [(control backward)]  'bm-previous)
(define-key tool-bar-map [(control forward)]   'bm-next)
(define-key tool-bar-map [(control search)]    'u-multi-occur)
(define-key tool-bar-map [(control replace)]   'anzu-query-replace-regexp)
(define-key tool-bar-map [(control page-up)]   'comint-previous-prompt)
(define-key tool-bar-map [(control page-down)] 'comint-next-prompt)
(define-key tool-bar-map [(control expand)]    'completion-at-point)
;;
;; Control Meta
;;
(define-key tool-bar-map [(control meta paste)] 'clipboard-yank)

;;
(message "Loading u-toolbar...done")
(provide 'u-toolbar)

;;; u-toolbar.el ends here
