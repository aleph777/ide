;;; tjf-toolbar.el --- Emacs toolbar revision -*-lexical-binding: t-*- ;; -*-Emacs-Lisp-*-

;;              Copyright © 2001-2026 Tom Fontaine

;;
;; Author:      Tom Fontaine
;; Date:        30-Nov-2001
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

;;; Code:

;;
(message "Loading tjf-toolbar...")
(require 'tjf-color)
(require 'tjf-flags)
(require 'tjf-frame)
(require 'tjf-navigate)
(require 'tjf-search)
(require 'undo-fu)

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
(tool-bar-add-item "view"   'view-file                 'view  :label "" :help "Browse file/Toggle Browse mode")
(tool-bar-add-item "edit"   'find-file                 'open  :label "" :help "Open file/Open file in new window...")
(tool-bar-add-item "add"    'tjf:file/new-empty-buffer 'new   :label "" :help "New file")
(tool-bar-add-item "cancel" 'kill-current-buffer       'close :label "" :help "Discard current buffer/Discard current buffer & window" :visible '(tjf:mode/is-not-shell-mode?))
(define-key-after (default-value 'tool-bar-map) [separator-1] menu-bar-separator)

(tool-bar-add-item "lock"        'read-only-mode   'lock   :label "" :help "Toggle read-only" :visible '(tjf:toolbar/visible-lock?))
(tool-bar-add-item "unlock"      'read-only-mode   'unlock :label "" :help "Toggle read-only" :visible '(tjf:toolbar/visible-unlock?))
(tool-bar-add-item "save"        'save-buffer      'save   :label "" :help "Save buffer"      :visible '(tjf:mode/is-not-shell-mode?) :enable '(tjf:flags/enable-save?) )
(tool-bar-add-item "save-as"     'write-file       'saveas :label "" :help "Save buffer as/Write region...")
(tool-bar-add-item "revert"      'revert-buffer    'revert :label "" :help "Revert buffer"    :visible '(tjf:mode/is-not-shell-mode?) :enable '(tjf:flags/enable-revert?))
(define-key-after (default-value 'tool-bar-map) [separator-2] menu-bar-separator)

;;
;; Undo/Redo
;;
(tool-bar-add-item "undo" 'undo-fu-only-undo 'undo :label "" :help "Undo last operation" :visible '(tjf:flags/is-rw?) :enable '(tjf:flags/enable-undo-redo?))
(tool-bar-add-item "redo" 'undo-fu-only-redo 'redo :label "" :help "Redo last undo"      :visible '(tjf:flags/is-rw?) :enable '(tjf:flags/enable-undo-redo?))

(define-key-after (default-value 'tool-bar-map) [separator-3] menu-bar-separator)

;;
;; Cut/Copy/Paste
;;
(tool-bar-add-item "cut"   'kill-region    'cut   :label ""  :visible '(tjf:flags/is-rw?) :help "Cut/Cut Rectangle"     :enable '(tjf:flags/enable-modify-region?))
(tool-bar-add-item "copy"  'kill-ring-save 'copy  :label ""                               :help "Copy/Copy Rectangle"   :enable 'mark-active)
(tool-bar-add-item "paste" 'yank           'paste :label ""  :visible '(tjf:flags/is-rw?) :help "Paste/Paste Rectangle" :enable '(tjf:flags/enable-paste?))

(define-key-after (default-value 'tool-bar-map) [separator-4] menu-bar-separator)

;;
;; Cursor movement
;;
(tool-bar-add-item "top"     'beginning-of-buffer  'home      :label "" :help "Home")
(tool-bar-add-item "bottom"  'end-of-buffer        'end       :label "" :help "End")
(tool-bar-add-item "north"   'scroll-down-command  'page-up   :label "" :help "Page Up")
(tool-bar-add-item "south"   'scroll-up-command    'page-down :label "" :help "Page Down")
(tool-bar-add-item "goto"    'goto-line            'goto      :label "" :help "Goto line.../Saved point")

(define-key-after (default-value 'tool-bar-map) [separator-5] menu-bar-separator)

;;
;; Misc
;;
;; (tool-bar-add-item "00-hide"    'hs-hide-block             'hide :label "" :help "Hide block" :visible 'hs-minor-mode)
;; (tool-bar-add-item "00-show"    'hs-show-block             'show :label "" :help "Show block" :visible 'hs-minor-mode)
(tool-bar-add-item "sb"   'search-word-backward 'backward :label "" :help "Search backward")
(tool-bar-add-item "sf" 'search-word-forward  'forward  :label "" :help "Search forward")
(tool-bar-add-item "search"      'tjf:search/occur     'search   :label "" :help "Show matching lines...")
(tool-bar-add-item "replace"     'anzu-query-replace   'replace  :label "" :help "Find & replace/Find & replace regexp..." :visible '(tjf:flags/visible-replace?))

(define-key-after  (default-value 'tool-bar-map) [separator-6] menu-bar-separator)

(tool-bar-add-item "repeat"   'repeat-complex-command             'repeat   :label "" :help "Repeat Command...")
(tool-bar-add-item "case"     'tjf:edit/toggle-char-case-at-point 'case     :label "" :help "Toggle case"     :visible '(tjf:flags/is-rw?))
;; (tool-bar-add-item "bookmark" 'bm-toggle                          'bookmark :label "" :help "Bookmark toggle" :visible '(tjf:mode/is-not-shell-mode?))

(define-key-after  (default-value 'tool-bar-map) [separator-7] menu-bar-separator)

(tool-bar-add-item "text+" 'text-scale-increase                'zoom-in  :label "")
(tool-bar-add-item "text-" 'text-scale-decrease                'zoom-out :label "")

(define-key-after  (default-value 'tool-bar-map) [separator-8] menu-bar-separator)

(tool-bar-add-item "tree"     'treemacs                           'tree     :label "" :help "Toggle treemacs")

(tool-bar-add-item "prev" 'tjf:toolbar/previous-input 'up-arrow   :visible '(tjf:mode/is-shell-mode?) :label "" :help "Previous input")
(tool-bar-add-item "next" 'tjf:toolbar/next-input     'down-arrow :visible '(tjf:mode/is-shell-mode?) :label "" :help "Next input")
(tool-bar-add-item "del"  'comint-delete-output       'cancel     :visible '(tjf:mode/is-shell-mode?) :label "" :help "Flush output")

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
(define-key tool-bar-map [(control meta search)]   'noccur-project)
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
