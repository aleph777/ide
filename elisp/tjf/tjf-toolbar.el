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

;; File/Buffer operations
;;
(tool-bar-add-item "edit-file"     'find-file                 'open  :label "" :help "Open file/Open file in new window...")
(tool-bar-add-item "text-snippet"  'view-file                 'view  :label "" :help "Browse file/Toggle Browse mode")
(define-key tool-bar-map [(control view)]                     'view-mode)
(tool-bar-add-item "new-file-2"    'tjf:file/new-empty-buffer 'new   :label "" :help "New file")
(tool-bar-add-item "delete-buffer" 'kill-current-buffer       'close :label "" :help "Discard current buffer/Discard current buffer & window" :visible '(tjf:mode/is-not-shell-mode?))

(define-key-after (default-value 'tool-bar-map) [separator-1] menu-bar-separator)

(tool-bar-add-item "lock"                   'read-only-mode   'lock   :label "" :help "Toggle read-only" :visible '(tjf:toolbar/visible-lock?))
(tool-bar-add-item "unlock"                 'read-only-mode   'unlock :label "" :help "Toggle read-only" :visible '(tjf:toolbar/visible-unlock?))
(tool-bar-add-item "save"                   'save-buffer      'save   :label "" :help "Save buffer"      :visible '(tjf:mode/is-not-shell-mode?) :enable '(tjf:flags/enable-save?) )
(tool-bar-add-item "save-as"                'write-file       'saveas :label "" :help "Save buffer as/Write region...")
(define-key tool-bar-map [(control saveas)] 'write-region)
(tool-bar-add-item "revert"                 'revert-buffer    'revert :label "" :help "Revert buffer"    :visible '(tjf:mode/is-not-shell-mode?) :enable '(tjf:flags/enable-revert?))

(define-key-after (default-value 'tool-bar-map) [separator-2] menu-bar-separator)

;; Undo/Redo
;;
(tool-bar-add-item "undo" 'undo-fu-only-undo 'undo :label "" :help "Undo last operation" :visible '(tjf:flags/is-rw?) :enable '(tjf:flags/enable-undo-redo?))
(tool-bar-add-item "redo" 'undo-fu-only-redo 'redo :label "" :help "Redo last undo"      :visible '(tjf:flags/is-rw?) :enable '(tjf:flags/enable-undo-redo?))

(define-key-after (default-value 'tool-bar-map) [separator-3] menu-bar-separator)

;; Cut/Copy/Paste
;;
(tool-bar-add-item "cut"                        'kill-region              'cut       :label ""  :visible '(tjf:flags/is-rw?) :help "Cut/Cut Rectangle"    :enable '(tjf:flags/enable-modify-region?))
(define-key tool-bar-map [(control cut)]        'kill-rectangle)
(tool-bar-add-item "copy"                       'kill-ring-save           'copy      :label ""  :visible t                   :help "Copy/Copy Rectangle"  :enable 'mark-active)
(define-key tool-bar-map [(control copy)]       'copy-rectangle-as-kill)
(tool-bar-add-item "paste"                      'yank                     'paste     :label "" :visible '(tjf:flags/is-rw?) :help "Paste/Paste Rectangle" :enable '(tjf:flags/enable-paste?))
(define-key tool-bar-map [(control paste)]      'yank-rectangle)
(define-key tool-bar-map [(control meta paste)] 'clipboard-yank)
(tool-bar-add-item "move-up"                    'ergoemacs-move-text-up   'move-up   :label "" :visible '(tjf:flags/is-rw?) :help "Move text up"          :enable t)
(tool-bar-add-item "move-down"                  'ergoemacs-move-text-down 'move-down :label "" :visible '(tjf:flags/is-rw?) :help "Move text down"        :enable t)
(tool-bar-add-item "align"                      'tjf:edit/align-columns   'align     :label "" :visible '(tjf:flags/is-rw?) :help "Align"                 :enable 'mark-active)
(define-key tool-bar-map [(control align)]      'align-regexp)

(define-key-after (default-value 'tool-bar-map) [separator-4] menu-bar-separator)

;; Movement
;;
(tool-bar-add-item "top"                       'beginning-of-buffer  'home      :label "" :help "Home")
(tool-bar-add-item "bottom"                    'end-of-buffer        'end       :label "" :help "End")
(tool-bar-add-item "scroll-backward"           'scroll-down-command  'page-up   :label "" :help "Page Up")
(define-key tool-bar-map [(control page-up)]   'comint-previous-prompt)
(tool-bar-add-item "scroll-forward"            'scroll-up-command    'page-down :label "" :help "Page Down")
(define-key tool-bar-map [(control page-down)] 'comint-next-prompt)
(tool-bar-add-item "goto-bol"                  'beginning-of-line    'bol       :label "" :help "Goto beginning of line")
(tool-bar-add-item "goto-eol"                  'end-of-line          'eol       :label "" :help "Goto end of line")
(tool-bar-add-item "goto"                      'goto-line            'goto      :label "" :help "Goto line.../Saved point")
(define-key tool-bar-map [(control goto)]      'goto-saved-point)

(define-key-after (default-value 'tool-bar-map) [separator-5] menu-bar-separator)

;; Misc
;;
(tool-bar-add-item "search-backward"             'search-word-backward 'backward :label "" :help "Search backward")
(tool-bar-add-item "search-forward"              'search-word-forward  'forward  :label "" :help "Search forward")
(tool-bar-add-item "search"                      'tjf:search/occur     'search   :label "" :help "Show matching lines...")
(define-key tool-bar-map [(control search)]      'tjf:search/multi-occur)
(define-key tool-bar-map [(control meta search)] 'noccur-project)
(tool-bar-add-item "find-replace"                'anzu-query-replace   'replace  :label "" :help "Find & replace (regexp)..." :visible '(tjf:flags/visible-replace?))
(define-key tool-bar-map [(control replace)]     'anzu-query-replace-regexp)

(define-key-after  (default-value 'tool-bar-map) [separator-6] menu-bar-separator)

(tool-bar-add-item "hide"                      'hs-hide-all            'hide   :label "" :help "Hide")
(define-key tool-bar-map [(control hide)]      'hs-hide-block)
(define-key tool-bar-map [(meta    hide)]      'hs-hide-level)
(tool-bar-add-item "show"                      'hs-show-all            'show   :label "" :help "Show")
(define-key tool-bar-map [(control show)]      'hs-show-block)
(tool-bar-add-item "filter"                    'flush-lines            'flush  :label "" :help "Flush lines"    :visible '(tjf:flags/is-rw?))
(define-key tool-bar-map [(control flush)]     'keep-lines)
(tool-bar-add-item "sort"                      'tjf:sort/alpha         'sort   :label "" :help "Sort"           :visible '(tjf:flags/is-rw?))
(define-key tool-bar-map [(control      sort)] 'tjf:sort/alpha-field)
(define-key tool-bar-map [(        meta sort)] 'tjf:sort/numeric)
(define-key tool-bar-map [(control meta sort)] 'tjf:sort/numeric-field)
(tool-bar-add-item "case"                      'tjf:edit/downcase      'case   :label "" :help "Down case"      :visible '(tjf:flags/is-rw?))
(define-key tool-bar-map [(control      case)] 'tjf:edit/upcase)
(define-key tool-bar-map [(        meta case)] 'tjf:edit/capitalize)
(define-key tool-bar-map [(control meta case)] 'xah-toggle-letter-case)
(tool-bar-add-item "repeat"                    'repeat-complex-command  'repeat :label "" :help "Repeat Command...")
(tool-bar-add-item "format"                    'tjf:cc/format           'ccfmtt :label "" :help "Format"          :visible '(or (tjf:mode/is-mode? tjf:mode/c++-mode) (tjf:mode/is-mode? tjf:mode/c-mode)))
(tool-bar-add-item "checkup"                   'tjf:c/syntax-check      'cschk  :label "" :help "Syntax check"    :visible '(tjf:mode/is-mode? tjf:mode/c-mode))
(tool-bar-add-item "checkup"                   'tjf:cpp/syntax-check    'cpschk :label "" :help "Syntax check"    :visible '(tjf:mode/is-mode? tjf:mode/c++-mode))
(define-key tool-bar-map [(control cpschk)]    'tjf:cpp/check)
(tool-bar-add-item "build"                     'tjf:cpp/compile-program 'cpcprg :label "" :help "Compile"         :visible '(tjf:mode/is-mode? tjf:mode/c++-mode))
(define-key tool-bar-map [(control cpcprg)]    'tjf:cpp/compile-file)
(tool-bar-add-item "make"                      'tjf:cpp/make            'cpmake :label "" :help "Make"            :visible '(tjf:mode/is-mode? tjf:mode/c++-mode))
(define-key tool-bar-map [(control cpmake)]    'compile)
(tool-bar-add-item "checkup"              'tjf:perl/syntax-check   'pschk  :label "" :help "Syntax check"   :visible '(tjf:mode/is-mode? tjf:mode/perl-mode))

(define-key-after  (default-value 'tool-bar-map) [separator-7] menu-bar-separator)


(tool-bar-add-item "text-bigger"                   'text-scale-increase 'zoom-in  :label "")
(define-key tool-bar-map [(control zoom-in)]       'tjf:color/brighten-background)
(define-key tool-bar-map [(control meta zoom-in)]  'tjf:color/saturate-background)
(tool-bar-add-item "text-smaller"                  'text-scale-decrease 'zoom-out :label "")
(define-key tool-bar-map [(control zoom-out)]      'tjf:color/darken-background)
(define-key tool-bar-map [(control meta zoom-out)] 'tjf:color/desaturate-background)

(define-key-after  (default-value 'tool-bar-map) [separator-8] menu-bar-separator)

(tool-bar-add-item "treemacs"   'treemacs                 'tree   :label "" :help "Toggle treemacs")
(tool-bar-add-item "terminal"   'tjf:tools/open-new-shell 'shell  :label "" :help "Open new shell")
(tool-bar-add-item "diff"       'ediff-buffers            'diff   :label "" :help "Diff")
(define-key tool-bar-map [(control diff)] 'ediff-files)
(tool-bar-add-item "menu"       'tjf:menubar/update       'update :label "" :help "Force umenu update")
(tool-bar-add-item "reset-size" 'tjf:frame/reset-size     'size   :label "" :help "Reset window")
(define-key tool-bar-map [(control size)] 'tjf:frame/half-size)

(tool-bar-add-item "previous-input" 'tjf:toolbar/previous-input 'up-arrow   :visible '(tjf:mode/is-shell-mode?) :label "" :help "Previous input")
(tool-bar-add-item "next-input"     'tjf:toolbar/next-input     'down-arrow :visible '(tjf:mode/is-shell-mode?) :label "" :help "Next input")
(tool-bar-add-item "trash"          'comint-delete-output       'cancel     :visible '(tjf:mode/is-shell-mode?) :label "" :help "Flush output")

;; Control Super
;;
;; (define-key tool-bar-map [(control super case)]     'ergoemacs-toggle-camel-case)
;; (define-key tool-bar-map [(control super zoom-in)]  'tjf:color/increase-hue-background)
;; (define-key tool-bar-map [(control super zoom-out)] 'tjf:color/decrease-hue-background)

;;
(message "Loading tjf-toolbar...done")
(provide 'tjf-toolbar)

;;; tjf-toolbar.el ends here
