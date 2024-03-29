;;; tjf-toolbar.el --- Emacs toolbar revision -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;              Copyright © 2001-2024 Tom Fontaine

;;
;; Author:      Tom Fontaine
;; Date:        30-Nov-2001
;; Time-stamp: <09-Jan-2018 11:25:06 EST, modified by Tom Fontaine>
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
;;           06-May-2019 Added ‘treemacs’
;;           24-Jun-2019 Added ‘toggle-char-case-at-point’
;;           20-Jul-2019 Changed tooltip on ‘view-file’ icon
;;           03-Feb-2021 ‘tjf’ overhaul
;;           10-Mar-2021 added SVG graphics
;;           16-Apr-2022 added visibility control for ‘toggle-case’
;;

;;; Code:

;;
(message "Loading tjf-toolbar...")
(require 'tjf-color)
(require 'tjf-flags)
(require 'tjf-frame)
(require 'tjf-navigate)
(require 'tjf-search)

;;; overload
(defun tool-bar--image-expression (icon)
  "Return an expression to evaluate an image spec for ICON."
  (let ((xpm-spec (list :type 'xpm :file (concat icon ".xpm")))
        (png-spec (list :type 'png :file (concat icon ".png")))
        (svg-spec (list :type 'svg :file (concat icon ".svg"))))
    `(find-image ',(list svg-spec png-spec xpm-spec))))

;;
;; comint functions
;;
(defun tjf:toolbar/previous-input ()
  "Retrieve previous input without the drama."
  (interactive)
  (if (not (= (point) (point-max)))
      (goto-char (point-max)))
  (comint-previous-input 1))

(defun tjf:toolbar/next-input ()
  "Retrieve next input without the drama."
  (interactive)
  (if (not (= (point) (point-max)))
      (goto-char (point-max)))
  (comint-previous-input 1))

;; lock/unlock
;;
(defun tjf:toolbar/visible-lock? ()
  "Boolean: should ‘lock’ be visible?"
  (and (tjf:mode/is-not-shell-mode?) (not buffer-read-only)))

(defun tjf:toolbar/visible-unlock? ()
  "Boolean: should ‘unlock’ be visible?"
  (and (tjf:mode/is-not-shell-mode?) buffer-read-only))

(setq tool-bar-map (make-sparse-keymap))

;;
;; File/Buffer operations
;;
(tool-bar-add-item "sg-view"  'view-file                 'view  :label "" :help "Browse file/Toggle Browse mode")
(tool-bar-add-item "sg-edit"  'find-file                 'open  :label "" :help "Open file/Open file in new window...")
(tool-bar-add-item "sg-new"   'tjf:file/new-empty-buffer 'new   :label "" :help "New file")
(tool-bar-add-item "sg-close" 'kill-this-buffer          'close :label "" :help "Discard current buffer/Discard current buffer & window" :visible '(tjf:mode/is-not-shell-mode?))
(define-key-after (default-value 'tool-bar-map) [separator-1] menu-bar-separator)

(tool-bar-add-item "sg-lock"    'read-only-mode   'lock   :label "" :help "Toggle read-only" :visible '(tjf:toolbar/visible-lock?))
(tool-bar-add-item "sg-unlock"  'read-only-mode   'unlock :label "" :help "Toggle read-only" :visible '(tjf:toolbar/visible-unlock?))
(tool-bar-add-item "sg-save"    'save-buffer      'save   :label "" :help "Save buffer"      :visible '(tjf:mode/is-not-shell-mode?) :enable '(tjf:flags/enable-save?) )
(tool-bar-add-item "sg-saveall" 'write-file       'saveas :label "" :help "Save buffer as/Write region...")
(tool-bar-add-item "sg-revert"  'revert-buffer    'revert :label "" :help "Revert buffer"    :visible '(tjf:mode/is-not-shell-mode?) :enable '(tjf:flags/enable-revert?))
(define-key-after (default-value 'tool-bar-map) [separator-2] menu-bar-separator)

;;
;; Undo/Redo
;;
(tool-bar-add-item "sg-undo" 'undo 'undo :label "" :help "Undo last operation" :visible '(tjf:flags/is-rw?) :enable '(tjf:flags/enable-undo-redo?))
(tool-bar-add-item "sg-redo" 'redo 'redo :label "" :help "Redo last undo"      :visible '(tjf:flags/is-rw?) :enable '(tjf:flags/enable-undo-redo?))

(define-key-after (default-value 'tool-bar-map) [separator-3] menu-bar-separator)

;;
;; Cut/Copy/Paste
;;
(tool-bar-add-item "sg-cut"   'kill-region    'cut   :label ""  :visible '(tjf:flags/is-rw?) :help "Cut/Cut Rectangle"     :enable '(tjf:flags/enable-modify-region?))
(tool-bar-add-item "sg-copy"  'kill-ring-save 'copy  :label ""                               :help "Copy/Copy Rectangle"   :enable 'mark-active)
(tool-bar-add-item "sg-paste" 'yank           'paste :label ""  :visible '(tjf:flags/is-rw?) :help "Paste/Paste Rectangle" :enable '(tjf:flags/enable-paste?))

(define-key-after (default-value 'tool-bar-map) [separator-4] menu-bar-separator)

;;
;; Cursor movement
;;
(tool-bar-add-item "sg-home"     'beginning-of-buffer  'home      :label "" :help "Home")
(tool-bar-add-item "sg-end"      'end-of-buffer        'end       :label "" :help "End")
(tool-bar-add-item "sg-up"       'scroll-down-command  'page-up   :label "" :help "Page Up")
(tool-bar-add-item "sg-down"     'scroll-up-command    'page-down :label "" :help "Page Down")
(tool-bar-add-item "sg-goto"     'goto-line            'goto      :label "" :help "Goto line.../Saved point")

(define-key-after (default-value 'tool-bar-map) [separator-5] menu-bar-separator)

;;
;; Misc
;;
;; (tool-bar-add-item "00-hide"    'hs-hide-block             'hide :label "" :help "Hide block" :visible 'hs-minor-mode)
;; (tool-bar-add-item "00-show"    'hs-show-block             'show :label "" :help "Show block" :visible 'hs-minor-mode)
(tool-bar-add-item "sg-search-up"   'search-word-backward 'backward :label "" :help "Search backward")
(tool-bar-add-item "sg-search-down" 'search-word-forward  'forward  :label "" :help "Search forward")
(tool-bar-add-item "sg-find"        'tjf:search/occur     'search   :label "" :help "Show matching lines...")
(tool-bar-add-item "sg-replace"     'anzu-query-replace   'replace  :label "" :help "Find & replace/Find & replace regexp..." :visible '(tjf:flags/visible-replace?))

(define-key-after  (default-value 'tool-bar-map) [separator-6] menu-bar-separator)

(tool-bar-add-item "sg-repeat"   'repeat-complex-command             'repeat   :label "" :help "Repeat Command...")
(tool-bar-add-item "sg-case"     'tjf:edit/toggle-char-case-at-point 'case     :label "" :help "Toggle case"     :visible '(tjf:flags/is-rw?))
(tool-bar-add-item "sg-bookmark" 'bm-toggle                          'bookmark :label "" :help "Bookmark toggle" :visible '(tjf:mode/is-not-shell-mode?))

(define-key-after  (default-value 'tool-bar-map) [separator-7] menu-bar-separator)

(tool-bar-add-item "sg-zoom-in"  'text-scale-increase                'zoom-in  :label "")
(tool-bar-add-item "sg-zoom-out" 'text-scale-decrease                'zoom-out :label "")

(define-key-after  (default-value 'tool-bar-map) [separator-8] menu-bar-separator)

(tool-bar-add-item "sg-tree"     'treemacs                           'tree     :label "" :help "Toggle treemacs")

(tool-bar-add-item "mi-previous" 'tjf:toolbar/previous-input 'up-arrow   :visible '(tjf:mode/is-shell-mode?) :label "" :help "Previous input")
(tool-bar-add-item "mi-next"     'tjf:toolbar/next-input     'down-arrow :visible '(tjf:mode/is-shell-mode?) :label "" :help "Next input")
(tool-bar-add-item "sg-cancel"   'comint-delete-output       'cancel     :visible '(tjf:mode/is-shell-mode?) :label "" :help "Flush output")

;;
;; Control
;;
(define-key tool-bar-map [(control backward)]  'bm-previous)
(define-key tool-bar-map [(control case)]      'tjf:edit/downcase)
(define-key tool-bar-map [(control close)]     'exit-buffer-and-frame)
(define-key tool-bar-map [(control copy)]      'copy-rectangle-as-kill)
(define-key tool-bar-map [(control cut)]       'kill-rectangle)
(define-key tool-bar-map [(control forward)]   'bm-next)
(define-key tool-bar-map [(control goto)]      'goto-saved-point)
(define-key tool-bar-map [(control open)]      'find-file-other-frame)
(define-key tool-bar-map [(control page-down)] 'comint-next-prompt)
(define-key tool-bar-map [(control page-up)]   'comint-previous-prompt)
(define-key tool-bar-map [(control paste)]     'yank-rectangle)
(define-key tool-bar-map [(control redo)]      'winner-redo)
(define-key tool-bar-map [(control replace)]   'anzu-query-replace-regexp)
(define-key tool-bar-map [(control saveas)]    'write-region)
(define-key tool-bar-map [(control search)]    'tjf:search/multi-occur)
(define-key tool-bar-map [(control undo)]      'winner-undo)
(define-key tool-bar-map [(control view)]      'view-mode)
(define-key tool-bar-map [(control zoom-in)]   'tjf:color/brighten-background)
(define-key tool-bar-map [(control zoom-out)]  'tjf:color/darken-background)

;;
;; Control Meta
;;
(define-key tool-bar-map [(control meta case)]     'xah-toggle-letter-case)
(define-key tool-bar-map [(control meta paste)]    'clipboard-yank)
(define-key tool-bar-map [(control meta zoom-in)]  'tjf:color/saturate-background)
(define-key tool-bar-map [(control meta zoom-out)] 'tjf:color/desaturate-background)

;;
;; Control Super
;;
(define-key tool-bar-map [(control super case)]     'ergoemacs-toggle-camel-case)
(define-key tool-bar-map [(control super zoom-in)]  'tjf:color/increase-hue-background)
(define-key tool-bar-map [(control super zoom-out)] 'tjf:color/decrease-hue-background)

;;
(message "Loading tjf-toolbar...done")
(provide 'tjf-toolbar)

;;; tjf-toolbar.el ends here
