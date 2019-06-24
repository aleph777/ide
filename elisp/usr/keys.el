;;; keys.el --- Global key definitions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 1999-2019 Tom Fontaine

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

;; Revision: 22-Mar-2007 Major revision/overhaul
;;           16-May-2008 Update for Emacs 22
;;           11-Feb-2011 Removed ‘redo’ - added ‘redo+’
;;           13-Sep-2012 Update for Emacs 24
;;           26-Sep-2013 Added mouse definitions from default.el
;;           27-Sep-2013 Removed entries for Sun function keys
;;           13-Jun-2014 Added ‘usr-tab-close-paren’
;;           24-Mar-2015 Bound ‘usr-open-line’ to M-return
;;           25-Mar-2015 Added ‘usr-capitalize-word’, ‘usr-downcase-word’, and ‘usr-upcase-word’
;;           26-Mar-2015 Fixed [mode-line C-mouse-2] definition
;;                       Removed [super tab] definition
;;                       Found problems in mode-line mouse bindings (not fixed!!!)
;;           01-Apr-2015 Fixed mode-line mouse bindings
;;                       Added secondary selection to ‘C-M-mouse-’
;;           02-Apr-2015 Added loading messages
;;           04-May-2015 Added ‘usr-move-line-down’
;;           06-May-2015 Changed ‘kp-N’ and ‘M-kp-N’ bindings to use lambda functions
;;           03-Dec-2015 Changed ‘M-kp-.’ to ‘completion-at-point’
;;                       Added ‘S-SPC’
;;                       Added ‘Scroll_Lock’ as ‘completion-at-point’
;;           05-Jan-2016 Added ‘tinyeat’, removed C-delete
;;           18-Jan-2016 Updated for new user interface
;;           29-Jan-2016 Added ‘eval-after-load’ for isearch definitions
;;           03-Feb-2016 Updated for more ergo
;;                       Added ‘xah-search-current-word’ for ‘C-f’
;;           04-Feb-2016 Added ‘usr-newline-and-indent’ for ‘C-S-return’
;;           06-Feb-2016 Added ‘usr-end-of-line’ and ‘usr-beginning-of-line’
;;           23-Feb-2016 Added ‘usr-occur’ and ‘usr-moccur’
;;           25-Feb-2016 Added ‘text-scale-increase’ and ‘text-scale-decrease’
;;           26-Feb-2016 Added ‘C-b’
;;                       Added ‘C-d’ as ‘duplicate-line-or-region’
;;           28-Feb-2016 Replaced ‘usr-’ functions with ‘u-’ functions
;;           02-Mar-2016 Changed ‘f6’ to ‘capitalize-word-or-region’
;;           03-Mar-2016 Removed ‘M-f’
;;                       Added ‘s-f1’ as ‘u-forward-symbol’
;;                       Added ‘s-f2’ as ‘backward-symbol’
;;                       Added ‘C-f1’ as ‘forward-word’
;;                       Added ‘kp-divide’ as ‘toggle-fill-paragraph-or-region’
;;           19-Apr-2016 Added ‘mouse-delete-window’ to "control left-fringe" and "control right-fringe"
;;           21-Apr-2016 Removed ‘modeline’ ‘mouse-1’" binding
;;                       Changed ‘modeline’ ‘C-mouse-1’ to delete-window
;;                       Changed ‘modeline’ ‘C-mouse-3’ to delete-other-windows
;;                       Changed ‘s-f6’ to ‘xah-toggle-letter-case’
;;           26-Apr-2016 Changed ‘C-e’ to ‘ergoemacs-extend-selection’ lambda function
;;                       Changed ‘C-kp-+’ to ‘shift-number-up’
;;                       Changed ‘C-kp--’ to ‘shift-number-down’
;;           27-Apr-2016 Changed ‘C-p’ to ‘pop-to-mark-command’
;;           28-Apr-2016 Changed ‘insert’ to ‘u-paste-clipboard’
;;                       Added ‘C-insert’ as ‘overwrite-mode’
;;                       Added ‘C-S-d’ as ‘duplicate-as-comment’
;;           14-Aug-2016 Added ‘M-insert’ as ‘insert-dd-mon-yyyy’
;;                       Added ‘s-insert’ as ‘insert-month-day-year’
;;           25-Aug-2016 Added ‘C-S-n’ as ‘narrow-or-widen-dwim’
;;           29-Aug-2016 Added ‘s-(’ as ‘xah-insert-paren’
;;                       Added ‘s-[’ as ‘xah-insert-bracket’
;;                       Added ‘s-{’ as ‘xah-insert-brace’
;;                       Added ‘s-\`’ as ‘xah-insert-emacs-quote’
;;                       Added ‘s-\’' as ‘xah-insert-single-quote’
;;                       Added ‘s-\"’ as ‘xah-insert-double-quote’
;;                       Added ‘C-i’ as ‘indent-region’
;;           21-Sep-2016 Added ‘s-r’ as ‘cua-rectangle-mark-mode’
;;           12-Oct-2016 Changed ‘s-r’ to ‘rectangle-mark-mode’
;;           16-Dec-2016 Changed ‘C-i’ to either ‘indent-region’ or ‘indent-for-tab-command’
;;           13-Jan-2017 Changed ‘C-N’ to ‘fancy-narrow-or-widen-dwim’
;;                       Added ‘C-L’ as ‘loccur-current’
;;                       Added ‘S-<’ as ‘xah-insert-lt’
;;                       Added ‘S->’ as ‘xah-insert-tag’
;;                       Changed ‘C-b’ to ‘bm-toggle’
;;           16-Jan-2017 Removed ‘tinysearch-search-word-*’
;;           18-Jan-2017 Changed ‘C-f1' to ‘insert-chs’
;;                       Added ‘C-f2’ as ‘insert-che’
;;           19-Jan-2017 Changed ‘C-N’ to ‘narrow-or-widen-dwim’
;;                       Added ‘C-S-mouse-1’ as ‘hs-mouse-toggle-hiding’
;;           19-Sep-2017 Added ‘M-pause’ as ‘sdcv-search’
;;           16-Jan-2019 Added ‘C-`’ (s-` does not register for Mint 19.1/Cinnamon 4)
;;           24-Jun-2019 Added ‘C-M-insert’ to insert user-full-name
;;

;;; Code:

(message "Loading keys...")
(require 'bm)
(require 'duplicate)
(require 'u-search)
(require 'u-edit)
(require 'u-file)
(require 'undo-tree)
(require 'u-navigate)
(require 'xah)
;;
;; Standardization keys
;;
(global-set-key [(control a)] 'mark-whole-buffer)
(global-set-key [(control b)] 'bm-toggle)
;;lobal-set-key [(control c)] 'COPY
(global-set-key [(control d)] 'duplicate-line-or-region)
(global-set-key [(control e)] '(lambda () (interactive) (ergoemacs-extend-selection 1 1)))
(global-set-key [(control f)] 'xah-search-current-word)
(global-set-key [(control g)] 'goto-line)
;;lobal-set-key [(control h)] HELP...
(global-set-key [(control i)] '(lambda () (interactive) (if mark-active (indent-region (region-beginning) (region-end)) (indent-for-tab-command))))
;;lobal-set-key [(control j)] '
(global-set-key [(control k)] 'comment-line)
;;lobal-set-key [(control l)] '
;;lobal-set-key [(control m)] '
(global-set-key [(control n)] 'new-empty-buffer)
(global-set-key [(control o)] 'find-file)
(global-set-key [(control p)] 'pop-to-mark-command)
(global-set-key [(control q)] 'keyboard-quit)
;;(global-set-key [(control r)] ')  # isearch
;;(global-set-key [(control s)] ')  # isearch
;;lobal-set-key [(control t)] '
;;lobal-set-key [(control u)] 'usr-upcase-word)
;;lobal-set-key [(control v)] 'PASTE)
;;lobal-set-key [(control x)] 'CUT)
(global-set-key [(control w)] 'kill-this-buffer)
(global-set-key [(control y)] 'undo-tree-redo)
;;lobal-set-key [(control z)] 'UNDO)

(global-set-key [(control +)] 'text-scale-increase)
(global-set-key [(control -)] 'text-scale-decrease)

(global-set-key [(control shift d)] 'duplicate-as-comment)
(global-set-key [(control shift f)] 'u-search-buffer)
(global-set-key [(control shift h)] 'query-replace)
(global-set-key [(control shift l)] 'loccur-current)
(global-set-key [(control shift n)] 'narrow-or-widen-dwim)
(global-set-key [(control shift q)] 'quoted-insert)
(global-set-key [(control shift s)] 'save-buffer)
;;(global-set-key [(control shift z)] 'undo-tree-redo)

(global-set-key [(control shift up)]    'ergoemacs-move-text-up)
(global-set-key [(control shift down)]  'ergoemacs-move-text-down)
(global-set-key [(control shift right)] 'ergoemacs-forward-open-bracket)
(global-set-key [(control shift left)]  'ergoemacs-backward-open-bracket) ;; autoloaded

(global-set-key [(meta s)] 'write-file)

(global-set-key [(super f)]  'u-search-all-files)
(global-set-key [(super r)]  'rectangle-mark-mode)

(global-set-key [(super \()] 'xah-insert-paren)
(global-set-key [(super \[)] 'xah-insert-bracket)
(global-set-key [(super \{)] 'xah-insert-brace)
(global-set-key [(super \')] 'xah-insert-single-quote)
(global-set-key [(super \")] 'xah-insert-double-quote)
(global-set-key [(super \`)] 'xah-insert-emacs-quote)
(global-set-key [(control \`)] 'xah-insert-emacs-quote)
(global-set-key [(super \<)] 'xah-insert-lt)
(global-set-key [(super \>)] 'xah-insert-tag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [(control return)]       'insert-newline-after)
(global-set-key [(control shift return)] 'insert-newline-after-and-indent)
(global-set-key [(meta return)]          'insert-newline-before)
(global-set-key [(super return)]         'insert-newline-before-and-indent)

(global-set-key [insert]           'u-paste-clipboard)
(global-set-key [(control insert)] 'overwrite-mode)
(global-set-key [(meta  insert)]   'insert-dd-mon-yyyy)
(global-set-key [(super insert)]   'insert-month-day-year)
(global-set-key [(control super insert)] '(lambda () (interactive) (insert user-full-name)))

(global-set-key [(meta \;)] 'xah-comment-dwim)

(global-set-key [f1]  'u-forward-word)
(global-set-key [f2]  'backward-word)
(global-set-key [f3]  'u-end-of-line)
(global-set-key [f4]  'u-beginning-of-line)
(global-set-key [f5]  'transpose-chars)
(global-set-key [f6]  'capitalize-word)
(global-set-key [f7]  'downcase-word)
(global-set-key [f8]  'upcase-word)
(global-set-key [f9]  'kill-word)
(global-set-key [f10] 'kill-whole-line)
(global-set-key [f11] 'delete-to-eol)
(global-set-key [f12] 'delete-to-bol)

(global-set-key [(super f1)] 'u-forward-symbol)
(global-set-key [(super f2)] 'backward-symbol)
(global-set-key [(super f6)] 'xah-toggle-letter-case)

;;
;; control-Function Keys
;;
(global-set-key [(control f1)]  'insert-chs)
(global-set-key [(control f2)]  'insert-che)
(global-set-key [(control f6)]  'capitalize-word-or-region)
(global-set-key [(control f7)]  'downcase-word-or-region)
(global-set-key [(control f8)]  'upcase-word-or-region)
(global-set-key [(control f9)]  'tinyeat-delete-whole-word) ;; autoloaded
(global-set-key [(control f10)] 'delete-line-text)
(global-set-key [(control f11)] 'join-line-1)
(global-set-key [(control f12)] 'delete-indentation)

;;
;; meta-Function Keys
;;
(global-set-key [(meta f10)] 'toggle-frame-fullscreen)
(global-set-key [(meta f11)] 'toggle-frame-maximized)

;;
;; "Middle" keys
;;
(global-set-key [home]   'beginning-of-buffer)
(global-set-key [end]    'end-of-buffer)
(global-set-key [delete] 'delete-char)

(global-set-key [pause]       'dabbrev-expand)
(global-set-key [f21]         'dabbrev-expand)
(global-set-key [Scroll_Lock] 'completion-at-point)
;;
;; Keypad keys
;;
(global-set-key [kp-1] 'duplicate-previous)
(global-set-key [kp-2] '(lambda () (interactive "*") (duplicate -2)))
(global-set-key [kp-3] '(lambda () (interactive "*") (duplicate -3)))
(global-set-key [kp-4] '(lambda () (interactive "*") (duplicate -4)))
(global-set-key [kp-5] '(lambda () (interactive "*") (duplicate -5)))
(global-set-key [kp-6] '(lambda () (interactive "*") (duplicate -6)))
(global-set-key [kp-7] '(lambda () (interactive "*") (duplicate -7)))
(global-set-key [kp-8] '(lambda () (interactive "*") (duplicate -8)))
(global-set-key [kp-9] '(lambda () (interactive "*") (duplicate -9)))
(global-set-key [kp-0] 'duplicate-line-or-region)

(global-set-key [(meta kp-1)] 'duplicate-next)
(global-set-key [(meta kp-2)] '(lambda () (interactive "*") (duplicate 2)))
(global-set-key [(meta kp-3)] '(lambda () (interactive "*") (duplicate 3)))
(global-set-key [(meta kp-4)] '(lambda () (interactive "*") (duplicate 4)))
(global-set-key [(meta kp-5)] '(lambda () (interactive "*") (duplicate 5)))
(global-set-key [(meta kp-6)] '(lambda () (interactive "*") (duplicate 6)))
(global-set-key [(meta kp-7)] '(lambda () (interactive "*") (duplicate 7)))
(global-set-key [(meta kp-8)] '(lambda () (interactive "*") (duplicate 8)))
(global-set-key [(meta kp-9)] '(lambda () (interactive "*") (duplicate 9)))
(global-set-key [(meta kp-0)] 'duplicate-line-or-region)

(global-set-key [kp-divide]   'toggle-fill-paragraph-or-region)
(global-set-key [kp-multiply] 'dabbrev-expand)
(global-set-key [kp-add]      'search-word-forward)
(global-set-key [kp-enter]    'duplicate-previous)
(global-set-key [kp-decimal]  'delete-horizontal-space)

(global-set-key [kp-subtract]           'search-word-backward)
(global-set-key [(control kp-subtract)] 'shift-number-down)
(global-set-key [(super   kp-subtract)] 'toggle-char-case-at-point)

(global-set-key [(control kp-add)]      'shift-number-up)

(global-set-key [(meta kp-enter)] 'insert-newline-before)
(global-set-key [(meta pause)]    'sdcv-search)

;;
;; control-"Middle" keys
;;

;;(global-set-key [(control delete)]    'usr-delete-forward-space)

(global-set-key [(control delete)]    'tinyeat-forward-preserve) ;; autoloaded
(global-set-key [(control backspace)] 'tinyeat-backward-preserve)
(global-set-key [(super backspace)]   'tinyeat-delete-paragraph)

(global-set-key [(control tab)] 'set-random-background-color)
;;(global-set-key [(super tab)]         'usr-tab-close-paren)

(global-set-key [(control up)]   'scroll-down-line)
(global-set-key [(control down)] 'scroll-up-line)

;;
;; Other Keys
;;
(global-set-key [?\s- ] 'ergoemacs-shrink-whitespaces)  ;; autoloaded — [(super SPC)]

;;; ================================================================================
;;;; Bindings for mouse commands.

;;; (define-key global-map [down-mouse-1] 'mouse-drag-region)
;;; (global-set-key [mouse-1]    'mouse-set-point)
;;; (global-set-key [drag-mouse-1]       'mouse-set-region)

;;; ;; These are tested for in mouse-drag-region.
;;; (global-set-key [double-mouse-1] 'mouse-set-point)
;;; (global-set-key [triple-mouse-1] 'mouse-set-point)

;;; (defun mouse--strip-first-event (_prompt)
;;;   (substring (this-single-command-raw-keys) 1))

;;; (define-key function-key-map [left-fringe mouse-1] 'mouse--strip-first-event)
;;; (define-key function-key-map [right-fringe mouse-1] 'mouse--strip-first-event)

;;; (global-set-key [mouse-2]    'mouse-yank-primary)
;;; ;; Allow yanking also when the corresponding cursor is "in the fringe".
;;; (define-key function-key-map [right-fringe mouse-2] 'mouse--strip-first-event)
;;; (define-key function-key-map [left-fringe mouse-2] 'mouse--strip-first-event)
;;; (global-set-key [mouse-3]    'mouse-save-then-kill)
;;; (define-key function-key-map [right-fringe mouse-3] 'mouse--strip-first-event)
;;; (define-key function-key-map [left-fringe mouse-3] 'mouse--strip-first-event)

;;; ;; By binding these to down-going events, we let the user use the up-going
;;; ;; event to make the selection, saving a click.
;;; (global-set-key [C-down-mouse-1] 'mouse-buffer-menu)
;;; (if (not (eq system-type 'ms-dos))
;;;     (global-set-key [S-down-mouse-1] 'mouse-appearance-menu))
;;; ;; C-down-mouse-2 is bound in facemenu.el.
;;; (global-set-key [C-down-mouse-3]
;;;   `(menu-item ,(purecopy "Menu Bar") ignore
;;;     :filter (lambda (_)
;;;               (if (zerop (or (frame-parameter nil 'menu-bar-lines) 0))
;;;                   (mouse-menu-bar-map)
;;;                 (mouse-menu-major-mode-map)))))

;;; ;; Binding mouse-1 to mouse-select-window when on mode-, header-, or
;;; ;; vertical-line prevents Emacs from signaling an error when the mouse
;;; ;; button is released after dragging these lines, on non-toolkit
;;; ;; versions.
;;; (global-set-key [mode-line mouse-1] 'mouse-select-window)
;;; (global-set-key [mode-line drag-mouse-1] 'mouse-select-window)
;;; (global-set-key [mode-line down-mouse-1] 'mouse-drag-mode-line)
;;; (global-set-key [header-line down-mouse-1] 'mouse-drag-header-line)
;;; (global-set-key [header-line mouse-1] 'mouse-select-window)
;;; (global-set-key [mode-line mouse-2] 'mouse-delete-other-windows)
;;; (global-set-key [mode-line mouse-3] 'mouse-delete-window)
;;; (global-set-key [mode-line C-mouse-2] 'mouse-split-window-horizontally)
;;; (global-set-key [vertical-scroll-bar C-mouse-2] 'mouse-split-window-vertically)
;;; (global-set-key [vertical-line C-mouse-2] 'mouse-split-window-vertically)
;;; (global-set-key [vertical-line down-mouse-1] 'mouse-drag-vertical-line)
;;; (global-set-key [right-divider down-mouse-1] 'mouse-drag-vertical-line)
;;; (global-set-key [bottom-divider down-mouse-1] 'mouse-drag-mode-line)
;;; (global-set-key [vertical-line mouse-1] 'mouse-select-window)

;;; ================================================================================
(global-set-key [(control mouse-4)]    'search-word-backward)
(global-set-key [(control mouse-5)]    'search-word-forward)
(global-set-key [(control wheel-up)]   'search-word-backward)
(global-set-key [(control wheel-down)] 'search-word-forward)
;;
;; Mouse side buttons
;;
(global-set-key [mouse-8] 'scroll-up-command)
(global-set-key [mouse-9] 'scroll-down-command)

(global-set-key [(control shift mouse-1)] '(lambda (e) (interactive "e") (hs-minor-mode 1)(hs-mouse-toggle-hiding e)))
(global-set-key [(control shift mouse-3)] 'ffap-at-mouse)
(global-set-key [(meta    shift mouse-3)] '(lambda (e) (interactive "e")(let ((ffap-file-finder 'find-file-other-frame)) (ffap-at-mouse e))))
;;
;; Mode Line Mouse
;;
;;(global-set-key [mode-line mouse-1] 'mouse-drag-mode-line)
(global-set-key [mode-line mouse-2] 'split-window-vertically)
(global-set-key [mode-line mouse-3] 'balance-windows)

;;
;; Fringe Mouse
;;
(global-set-key [left-fringe  mouse-1] 'bm-toggle-mouse)
(global-set-key [left-fringe  mouse-4] 'bm-previous-mouse)
(global-set-key [left-fringe  mouse-5] 'bm-next-mouse)
(global-set-key [right-fringe mouse-1] 'mouse-delete-other-windows)
(global-set-key [right-fringe mouse-2] 'split-window-horizontally)

(global-set-key [right-fringe (control mouse-1)] 'mouse-delete-window)

;;
;; Secondary Selection
;;
(global-set-key [(control meta mouse-1)] 'mouse-set-secondary)
(global-set-key [(control meta mouse-2)] 'mouse-yank-secondary)
(global-set-key [(control meta mouse-3)] 'mouse-secondary-save-then-kill)

;;
;; Mode Line C-mouse
;;
(global-set-key [mode-line C-mouse-1] 'mouse-delete-other-windows)
(global-set-key [mode-line C-mouse-3] 'mouse-delete-window)

;;; (global-set-key [mode-line C-mouse-1] 'mouse-delete-other-windows)
;;; ;(global-set-key [mode-line C-mouse-2] 'mouse-tear-off-window)
;;; (global-set-key [mode-line C-mouse-3] 'balance-windows)
;;; ;
;; C-Mouse wheel
;;

;;
;; menu - between R-Windows (i.e. "super") and R-Ctrl
;;
;;(global-set-key [menu] ')
;;(global-set-key [(control menu)] ')
;;(global-set-key [(meta menu)] ')
;;(global-set-key [(super menu)] ')
;;(global-set-key [(control meta menu)] ')
;;(global-set-key [(control super menu)] ')
;;(global-set-key [(meta super menu)] ')

;;
;; Print Screen
;;
;;(global-set-key [(super print)] ')
;;(global-set-key [(control super print)] ')
;;(global-set-key [(meta super print)] ')

;;
;; Scroll Lock
;;
;;(global-set-key [Scroll_Lock] ')
;;(global-set-key [(control Scroll_Lock)] ')
;;(global-set-key [(meta Scroll_Lock)] ')
;;(global-set-key [(super Scroll_Lock)] ')
;;(global-set-key [(control meta Scroll_Lock)] ')
;;(global-set-key [(control super Scroll_Lock)] ')
;;(global-set-key [(meta super Scroll_Lock)] ')

;;
;; Pause
;;
;;(global-set-key [(control pause)] ')
;;(global-set-key [(meta pause)] ')
;;(global-set-key [(super pause)] ')
;;(global-set-key [(control meta pause)] ')
;;(global-set-key [(control super pause)] ')
;;(global-set-key [(meta super pause)] ')

;;
(message "Loading keys...done")
(provide 'keys)

;;; keys.el ends here
