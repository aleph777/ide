;;; tjf-keys.el --- Global key definitions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-


;;         Copyright © 1999-2023 Tom Fontaine

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
;;           16-Jun-2019 Reformatted/reorganized
;;           03-Sep-2019 Added ‘C-S-mouse-3’ as ‘minions-minor-modes-menu’
;;           15-Dec-2020 Added ‘kp-*' for non-numlocked keypad keys
;;           03-Feb-2021 ‘tjf’ overhaul
;;           16-Mar-2021 Removed ‘tinyeat-delete-whole-word’, ‘tinyeat-backward-preserve’, and ‘tinyeat-forward-preserve’
;;                       Fixed warnings on lambda functions
;;           24-Mar-2021 Added ‘consult-outline’ to ‘C-M-o’
;;           28-Apr-2022 Added ‘tjf:duplicate/tabs’
;;           30-Sep-2022 Added mode-line bindings for ‘mouse-8’, ‘mouse-9’, and ‘C-mouse-3’

;;; Code:

(message "Loading tjf-keys...")
(require 'anzu)
(require 'bm)
(require 'consult)
(require 'tjf-clipboard)
(require 'tjf-duplicate)
(require 'tjf-edit)
(require 'tjf-file)
(require 'tjf-frame)
(require 'tjf-navigate)
(require 'tjf-search)
(require 'tjf-tools)
(require 'undo-tree)
(require 'xah)

;; (unbind-key "C-h n")
;; (unbind-key "C-h C-n")
;; (unbind-key "C-x C-d")
;; (unbind-key "M-o")

;; ==================== a ====================
;;
(global-set-key [(control a)]            'mark-whole-buffer)
;;lobal-set-key [(meta a)]               'backward-sentence)
;;lobal-set-key [(super a)]              '
;;lobal-set-key [(control meta  a)]      '
;;lobal-set-key [(control shift a)]      '
;;lobal-set-key [(control super a)]      '
;;lobal-set-key [(meta    super a)]      '
;;lobal-set-key [(control meta super a)] '

;; ==================== b ====================
;;
(global-set-key [(control b)]            'bm-toggle)
;;lobal-set-key [(meta    b)]            'backward-word)
;;lobal-set-key [(super   b)]            '
;;lobal-set-key [(control meta  b)]      '
;;lobal-set-key [(control shift b)]      '
;;lobal-set-key [(control super b)]      '
;;lobal-set-key [(meta    super b)]      '
;;lobal-set-key [(control meta super b)] '

;; ==================== c ====================
;;
;;lobal-set-key [(control c)]            'COPY
;;lobal-set-key [(meta    c)]            'capitalize-word)
;;lobal-set-key [(super   c)]            '
;;lobal-set-key [(control meta  c)]      '
;;lobal-set-key [(control shift c)]      '
(global-set-key [(control super c)]      'copy-rectangle-as-kill)
;;lobal-set-key [(meta    super c)]      '
;;lobal-set-key [(control meta super c)] '

;; ==================== d ====================
;;
(global-set-key [(control d)]            'tjf:duplicate/line-or-region)
;;lobal-set-key [(meta    d)]            'kill-word)
;;lobal-set-key [(super   d)]            'DO NOT USE ... closes all windows
;;lobal-set-key [(control meta  d)]      '
(global-set-key [(control shift d)]      'tjf:duplicate/as-comment)
;;lobal-set-key [(control super d)]      '
;;lobal-set-key [(meta    super d)]      '
;;lobal-set-key [(control meta super d)] '

;; ==================== e ====================
;;
(global-set-key [(control e)]            #'(lambda () (interactive) (ergoemacs-extend-selection 1 1)))
;;lobal-set-key [(meta    e)]            'forward-sentence)
;;lobal-set-key [(super   e)]            'DO NOT USE ... opens file manager
;;lobal-set-key [(control meta  e)]      '
;;lobal-set-key [(control shift e)]      '
;;lobal-set-key [(control super e)]      '
;;lobal-set-key [(meta    super e)]      '
;;lobal-set-key [(control meta super e)] '

;; ==================== f ====================
;;
(global-set-key [(control f)]            'tjf:search/current-word-or-region)
;;lobal-set-key [(meta    f)]            'forward-word)
(global-set-key [(super   f)]            'tjf:search/all-files)
;;lobal-set-key [(control meta  f)]      '
(global-set-key [(control shift f)]      'tjf:search/buffer)
;;lobal-set-key [(control super f)]      '
;;lobal-set-key [(meta    super f)]      '
;;lobal-set-key [(control meta super f)] '

;; ==================== g ====================
;;
(global-set-key [(control g)]            'goto-line)
;;lobal-set-key [(meta    g)]            'PREFIX
;;lobal-set-key [(super   g)]            ')
;;lobal-set-key [(control shift g)]      '
;;lobal-set-key [(control super g)]      '
;;lobal-set-key [(meta    super g)]      '
;;lobal-set-key [(control meta super g)] '

;; ==================== h ====================
;;
;;lobal-set-key [(control h)]            '...HELP...
;;lobal-set-key [(meta    h)]            'mark-paragraph)
;;lobal-set-key [(super   h)]            'DO NOT USE
;;lobal-set-key [(control meta  h)]      '
(global-set-key [(control shift h)]      'anzu-query-replace)
(global-set-key [(control super h)]      'hs-hide-block)
;;lobal-set-key [(meta    super h)]      '
(global-set-key [(control meta super h)] 'anzu-query-replace-regexp)

;; ==================== i ====================
;;
(global-set-key [(control i)]            #'(lambda () (interactive) (if mark-active (indent-region (region-beginning) (region-end)) (indent-for-tab-command))))
;;lobal-set-key [(meta    i)]            'tab-to-tab-stop
;;lobal-set-key [(super   i)]            '
;;lobal-set-key [(control meta  i)]      '
;;lobal-set-key [(control shift i)]      '
(global-set-key [(control super i)]      'tjf:edit/insert-chs)
(global-set-key [(meta    super i)]      'tjf:edit/insert-che)
;;lobal-set-key [(control meta super i)] '

;; ==================== j ====================
;;
;;lobal-set-key [(control j)]            'electric-newline-and-maybe-indent
;;lobal-set-key [(meta    j)]            'indent-new-comment-line
;;lobal-set-key [(super   j)]            '
;;lobal-set-key [(control meta  j)]      '
;;lobal-set-key [(control shift j)]      '
;;lobal-set-key [(control super j)]      '
;;lobal-set-key [(meta    super j)]      '
;;lobal-set-key [(control meta super j)] '

;; ==================== k ====================
;;
(global-set-key [(control k)]            'comment-line)
;;lobal-set-key [(meta    k)]            'kill-sentence)
;;lobal-set-key [(super   k)]            '
;;lobal-set-key [(control meta  k)]      '
;;lobal-set-key [(control shift k)]      '
;;lobal-set-key [(control super k)]      '
;;lobal-set-key [(meta    super k)]      '
;;lobal-set-key [(control meta super k)] '


;; ==================== l ====================
;;
;;lobal-set-key [(control l)]            'recenter-top-bottom
;;lobal-set-key [(meta    l)]            'downcase-word)
;;lobal-set-key [(super   l)]            ' DO NOT USE ... locks computer
;;lobal-set-key [(control meta  l)]      '
(global-set-key [(control shift l)]      'loccur-current)
;;lobal-set-key [(control super l)]      '
;;lobal-set-key [(meta    super l)]      '
;;lobal-set-key [(control meta super l)] '
(define-key help-map [(control k)]       'describe-key)

;
;; ==================== m ====================
;;
;;lobal-set-key [(control m)]            'newline
;;lobal-set-key [(meta    m)]            'back-to-indentation)
;;lobal-set-key [(super   m)]            '
;;lobal-set-key [(control meta  m)]      '
;;lobal-set-key [(control shift m)]      '
;;lobal-set-key [(control super m)]      ' DO NOT USE ...
;;lobal-set-key [(meta    super m)]      '
;;lobal-set-key [(control meta super m)] '

;; ==================== n ====================
;;
(global-set-key [(control n)]            'tjf:file/new-empty-buffer)
;;lobal-set-key [(meta    n)]            '
;;lobal-set-key [(super   n)]            '
;;lobal-set-key [(control meta  n)]      '
(global-set-key [(control shift n)]      'tjf:view/narrow-or-widen)
;;lobal-set-key [(control super n)]      '
;;lobal-set-key [(meta    super n)]      '
;;lobal-set-key [(control meta super n)] '

;; ==================== o ====================
;;
(global-set-key [(control o)]            'find-file)
;;lobal-set-key [(meta    o)]            ' PREFIX
;;lobal-set-key [(super   o)]            ' DO NOT USE ... ???
(global-set-key [(control meta  o)]      'consult-outline)
;;lobal-set-key [(control shift o)]      'find-file)
(global-set-key [(control super o)]      'open-rectangle)
;;lobal-set-key [(meta    super o)]      '
;;lobal-set-key [(control meta super o)] '

;; ==================== p ====================
;;
;;lobal-set-key [(control  p)]           'pop-to-mark-command)
;;lobal-set-key [(meta     p)]           '
;;lobal-set-key [(super    p)]           ' DO NOT USE ... changes screen resolution
;;lobal-set-key [(control meta  p)]      '
;;lobal-set-key [(control shift p)]      '
;;lobal-set-key [(control super p)]      '
;;lobal-set-key [(meta    super p)]      '
;;lobal-set-key [(control meta super p)] '

;; ==================== q ====================
;;
(global-set-key [(control q)]            'keyboard-quit)
;;lobal-set-key [(meta    q)]            'fill-paragraph)
;;lobal-set-key [(super   q)]            '
;;lobal-set-key [(control meta  q)]      '
(global-set-key [(control shift q)]      'quoted-insert)
;;lobal-set-key [(control super q)]      '
;;lobal-set-key [(meta    super q)]      '
;;lobal-set-key [(control meta super q)] '

;; ==================== r ====================
;;
;;lobal-set-key [(control r)]            'isearch-backward
;;lobal-set-key [(meta    r)]            'move-to-window-line-top-bottom)
(global-set-key [(super   r)]            'rectangle-mark-mode)
;;lobal-set-key [(control meta  r)]      '
;;lobal-set-key [(control shift r)]      '
;;lobal-set-key [(control super r)]      '
;;lobal-set-key [(meta    super r)]      '
;;lobal-set-key [(control meta super r)] '

;; ==================== s ====================
;;
;;lobal-set-key [(control s)]            'isearch-forward
(global-set-key [(meta    s)]            'write-file)
;;lobal-set-key [(super   s)]            ' DO NOT USE ... key not registered
(global-set-key [(control meta  s)]      'tjf:tools/open-new-shell)
(global-set-key [(control shift s)]      'save-buffer)
(global-set-key [(control super s)]      'hs-show-block)
;;lobal-set-key [(meta    super s)]      '
;;lobal-set-key [(control meta super s)] '

;; ==================== t ====================
;;
(global-set-key [(control t)]            'xah-toggle-letter-case)
;;lobal-set-key [(meta    t)]            'transpose-words)
;;lobal-set-key [(super   t)]            ' DO NOT USE
;;lobal-set-key [(control meta  t)]      ' DO NOT USE
(global-set-key [(control shift t)]      'ergoemacs-toggle-camel-case)
(global-set-key [(control super t)]      'tjf:edit/toggle-char-case-at-point)
;;lobal-set-key [(meta    super t)]      ')
;;lobal-set-key [(control meta super t)] '

;; ==================== u ====================
;;
;;lobal-set-key [(control u)]            '...user prefix...
;;lobal-set-key [(meta    u)]            'upcase-word)
;;lobal-set-key [(super   u)]            '
;;lobal-set-key [(control meta  u)]      '
;;lobal-set-key [(control shift u)]      '
;;lobal-set-key [(control super u)]      '
;;lobal-set-key [(meta    super u)]      '
;;lobal-set-key [(control meta super u)] '

;; ==================== v ====================
;;
;;lobal-set-key [(control v)]            'PASTE)
;;lobal-set-key [(meta    v)]            'delete-selection-repeat-replace-region)
;;lobal-set-key [(super   v)]            '
;;lobal-set-key [(control meta  v)]      '
;;lobal-set-key [(control shift v)]      '
(global-set-key [(control super v)]      'yank-rectangle)
;;lobal-set-key [(meta    super v)]      '
;;lobal-set-key [(control meta super v)] '

;; ==================== w ====================
;;
(global-set-key [(control w)]            'kill-this-buffer)
;;lobal-set-key [(meta    w)]            'kill-ring-save)
;;lobal-set-key [(super   w)]            '
;;lobal-set-key [(control meta  w)]      '
;;lobal-set-key [(control shift w)]      '
(global-set-key [(control super w)]      'winner-undo)
(global-set-key [(meta    super w)]      'winner-redo)
;;lobal-set-key [(control meta super w)] '

;; ==================== x ====================
;;
;;lobal-set-key [(control x)]            'CUT)
;;lobal-set-key [(meta    x)]            ' DUH ... M-x
;;lobal-set-key [(super   x)]            '
;;lobal-set-key [(control meta  x)]      '
;;lobal-set-key [(control shift x)]      ' PREFIX
(global-set-key [(control super x)]      'kill-rectangle)
;;lobal-set-key [(meta    super x)]      '
;;lobal-set-key [(control meta super x)] '

;; ==================== y ====================
;;
(global-set-key [(control y)]            'undo-tree-redo)
;;lobal-set-key [(meta    y)]            'cua-paste-pop)
;;lobal-set-key [(super   y)]            '
;;lobal-set-key [(control meta  y)]      '
;;lobal-set-key [(control shift y)]      '
;;lobal-set-key [(control super y)]      '
;;lobal-set-key [(meta    super y)]      '
;;lobal-set-key [(control meta super y)] '

;; ==================== z ====================
;;
;;lobal-set-key [(control z)]            'UNDO)
;;lobal-set-key [(meta    z)]            'zap-to-char)
;;lobal-set-key [(super   z)]            '
;;lobal-set-key [(control meta  z)]      '
;;lobal-set-key [(control shift z)]      'undo-tree-redo)
;;lobal-set-key [(control super z)]      '
;;lobal-set-key [(meta    super z)]      '
;;lobal-set-key [(control meta super z)] '

;; ==================== ! ====================
;;
;;lobal-set-key [(control !)]            '
;;lobal-set-key [(meta    !)]            'shell-command)
;;lobal-set-key [(super   !)]            'DO NOT USE
;;lobal-set-key [(control meta  !)]      '
;;lobal-set-key [(control super !)]      '
;;lobal-set-key [(meta    super !)]      '
;;lobal-set-key [(control meta super !)] '

;; ==================== # ====================
;;
;;lobal-set-key [(control #)]            '
;;lobal-set-key [(meta    #)]            '
;;lobal-set-key [(super   #)]            'DO NOT USE
;;lobal-set-key [(control meta  #)]      '
;;lobal-set-key [(control super #)]      '
;;lobal-set-key [(meta    super #)]      '
;;lobal-set-key [(control meta super #)] '

;; ==================== $ ====================
;;
;;lobal-set-key [(control $)]            '
;;lobal-set-key [(meta    $)]            'ispell-word)
;;lobal-set-key [(super   $)]            'DO NOT USE
;;lobal-set-key [(control meta  $)]      '
;;lobal-set-key [(control super $)]      '
;;lobal-set-key [(meta    super $)]      '
;;lobal-set-key [(control meta super $)] '

;; ==================== % ====================
;;
;;lobal-set-key [(control %)]            '
;;lobal-set-key [(meta    %)]            'query-replace)
;;lobal-set-key [(super   %)]            'KEY NOT REGISTERED
;;lobal-set-key [(control meta  %)]      '
;;lobal-set-key [(control super %)]      '
;;lobal-set-key [(meta    super %)]      '
;;lobal-set-key [(control meta super %)] '

;; ==================== & ====================
;;
;;lobal-set-key [(control &)]            '
;;lobal-set-key [(meta    &)]            'async-shell-command)
;;lobal-set-key [(super   &)]            'KEY NOT REGISTERED
;;lobal-set-key [(control meta  &)]      '
;;lobal-set-key [(control super &)]      '
;;lobal-set-key [(meta    super &)]      '
;;lobal-set-key [(control meta super &)] '

;; ==================== * ====================
;;
;;lobal-set-key [(control *)]            '
;;lobal-set-key [(meta    *)]            '
;;lobal-set-key [(super   *)]            'KEY NOT REGISTERED
;;lobal-set-key [(control meta  *)]      '
;;lobal-set-key [(control super *)]      '
;;lobal-set-key [(meta    super *)]      '
;;lobal-set-key [(control meta super *)] '

;; ==================== + ====================
;;
(global-set-key [(control +)]            'text-scale-increase)
;;lobal-set-key [(meta    +)]            ')
;;lobal-set-key [(super   +)]            '
;;lobal-set-key [(control meta  +)]      '
;;lobal-set-key [(control super +)]      '
;;lobal-set-key [(meta    super +)]      '
;;lobal-set-key [(control meta super +)] '

;; ==================== , ====================
;;
;;lobal-set-key [(control ,)]            '
;;lobal-set-key [(meta    ,)]            'xref-pop-marker-stack)
;;lobal-set-key [(super   ,)]            '
;;lobal-set-key [(control meta  ,)]      '
;;lobal-set-key [(control super ,)]      '
;;lobal-set-key [(meta    super ,)]      '
;;lobal-set-key [(control meta super ,)] '

;; ==================== - ====================
;;
(global-set-key [(control -)]            'text-scale-decrease)
;;lobal-set-key [(meta    -)]            'negative-argument)
;;lobal-set-key [(super   -)]            '
;;lobal-set-key [(control meta  -)]      '
;;lobal-set-key [(control super -)]      '
;;lobal-set-key [(meta    super -)]      '
;;lobal-set-key [(control meta super -)] '

;; ==================== . ====================
;;
;;lobal-set-key [(control .)]            '
;;lobal-set-key [(meta    .)]            'xref-find-definitions)
;;lobal-set-key [(super   .)]            '
;;lobal-set-key [(control meta  .)]      '
;;lobal-set-key [(control super .)]      '
;;lobal-set-key [(meta    super .)]      '
;;lobal-set-key [(control meta super .)] '

;; ==================== / ====================
;;
;;lobal-set-key [(control /)] 'undo-tree-undo)
;;lobal-set-key [(meta    /)] 'dabbrev-expand)
;;lobal-set-key [(super   /)] '
;;lobal-set-key [(control meta  /)]      '
(global-set-key [(control super /)]      'tjf:frame/half-size)
;;lobal-set-key [(meta    super /)]      '
;;lobal-set-key [(control meta super /)] '

;; ==================== 0 ====================
;;
;;lobal-set-key [(control 0)] ' DO NOT USE ... digit argument
;;lobal-set-key [(meta    0)] ' DO NOT USE ... digit argument
;;lobal-set-key [(super   0)] ' DO NOT USE
;;lobal-set-key [(control meta  0)]      '
;;lobal-set-key [(control super 0)]      '
;;lobal-set-key [(meta    super 0)]      '
;;lobal-set-key [(control meta super 0)] '

; ==================== 1 ====================
;;
;;lobal-set-key [(control 1)] ' DO NOT USE ... digit argument
;;lobal-set-key [(meta    1)] ' DO NOT USE ... digit argument
;;lobal-set-key [(super   1)] ' DO NOT USE
;;lobal-set-key [(control meta  1)]      '
;;lobal-set-key [(control super 1)]      '
;;lobal-set-key [(meta    super 1)]      '
;;lobal-set-key [(control meta super 1)] '

;; ==================== 2 ====================
;;
;;lobal-set-key [(control 2)] ' DO NOT USE ... digit argument
;;lobal-set-key [(meta    2)] ' DO NOT USE ... digit argument
;;lobal-set-key [(super   2)] ' DO NOT USE
;;lobal-set-key [(control meta  2)]      '
;;lobal-set-key [(control super 2)]      'DO NOT USE
;;lobal-set-key [(meta    super 2)]      '
;;lobal-set-key [(control meta super 2)] '

;; ==================== 3 ====================
;;
;;lobal-set-key [(control 3)] ' DO NOT USE ... digit argument
;;lobal-set-key [(meta    3)] ' DO NOT USE ... digit argument
;;lobal-set-key [(super   3)] ' DO NOT USE
;;lobal-set-key [(control meta  3)]      '
;;lobal-set-key [(control super 3)]      '
;;lobal-set-key [(meta    super 3)]      '
;;lobal-set-key [(control meta super 3)] '

;; ==================== 4 ====================
;;
;;lobal-set-key [(control 4)] ' DO NOT USE ... digit argument
;;lobal-set-key [(meta    4)] ' DO NOT USE ... digit argument
;;lobal-set-key [(super   4)] ' DO NOT USE
;;lobal-set-key [(control meta  4)]      '
;;lobal-set-key [(control super 4)]      '
;;lobal-set-key [(meta    super 4)]      '
;;lobal-set-key [(control meta super 4)] '

;; ==================== 5 ====================
;;
;;lobal-set-key [(control 5)] ' DO NOT USE ... digit argument
;;lobal-set-key [(meta    5)] ' DO NOT USE ... digit argument
;;lobal-set-key [(super   5)] ' DO NOT USE
;;lobal-set-key [(control meta  5)]      '
;;lobal-set-key [(control super 5)]      '
;;lobal-set-key [(meta    super 5)]      '
;;lobal-set-key [(control meta super 5)] '

;; ==================== 6 ====================
;;
;;lobal-set-key [(control 6)] ' DO NOT USE ... digit argument
;;lobal-set-key [(meta    6)] ' DO NOT USE ... digit argument
;;lobal-set-key [(super   6)] ' DO NOT USE
;;lobal-set-key [(control meta  6)]      '
;;lobal-set-key [(control super 6)]      '
;;lobal-set-key [(meta    super 6)]      '
;;lobal-set-key [(control meta super 6)] '

;; ==================== 7 ====================
;;
;;lobal-set-key [(control 7)] ' DO NOT USE ... digit argument
;;lobal-set-key [(meta    7)] ' DO NOT USE ... digit argument
;;lobal-set-key [(super   7)] ' DO NOT USE
;;lobal-set-key [(control meta  7)]      '
;;lobal-set-key [(control super 7)]      '
;;lobal-set-key [(meta    super 7)]      '
;;lobal-set-key [(control meta super 7)] '

;; ==================== 8 ====================
;;
;;lobal-set-key [(control 8)] ' DO NOT USE ... digit argument
;;lobal-set-key [(meta    8)] ' DO NOT USE ... digit argument
;;lobal-set-key [(super   8)] ' DO NOT USE
;;lobal-set-key [(control meta  8)]      '
;;lobal-set-key [(control super 8)]      '
;;lobal-set-key [(meta    super 8)]      '
;;lobal-set-key [(control meta super 8)] '

;; ==================== 9 ====================
;;
;;lobal-set-key [(control 9)] ' DO NOT USE ... digit argument
;;lobal-set-key [(meta    9)] ' DO NOT USE ... digit argument
;;lobal-set-key [(super   9)] ' DO NOT USE
;;lobal-set-key [(control meta  9)]      '
;;lobal-set-key [(control super 9)]      '
;;lobal-set-key [(meta    super 9)]      '
;;lobal-set-key [(control meta super 9)] '

;; ==================== SPC ====================
;;
;;lobal-set-key [(control    SPC)] 'cua-set-mark)
;;lobal-set-key [(meta       SPC)] ' KEY NOT REGISTERED
;;lobal-set-key [(super      SPC)] ' DO NOT USE
(global-set-key [?\M-\S- ] 'just-one-space)
(global-set-key [?\s- ]    'ergoemacs-shrink-whitespaces)  ;; autoloaded — [(super SPC)]
;;lobal-set-key [(control meta  SPC)] 'mark-sexp)
(global-set-key [?\C-\s- ] 'ergoemacs-shrink-whitespaces)
;;lobal-set-key [(meta    super SPC)] '
;;lobal-set-key [(control meta super SPC)] '

;; ==================== : ====================
;;
;;lobal-set-key [(control :)]  '
;;lobal-set-key [(meta    :)]  'eval-expression)
;;lobal-set-key [(super   :)]  '
;;lobal-set-key [(control meta  :)]      '
;;lobal-set-key [(control super :)]      '
;;lobal-set-key [(meta    super :)]      '
;;lobal-set-key [(control meta super :)] '

;; ==================== ; ====================
;;
;;lobal-set-key [(control \;)] '
(global-set-key [(meta    \;)] 'xah-comment-dwim)
;;lobal-set-key [(super   \;)] '
;;lobal-set-key [(control meta  \;)]      '
;;lobal-set-key [(control super \;)]      '
;;lobal-set-key [(meta    super \;)]      '
;;lobal-set-key [(control meta super \;)] '

;; ==================== < ====================
;;
(global-set-key [(control <)] 'xah-insert-lt)
;;lobal-set-key [(meta    <)] 'beginning-of-buffer)
(global-set-key [(super   <)] 'xah-insert-lt)
;;lobal-set-key [(control meta  <)]      '
;;lobal-set-key [(control super <)]      '
;;lobal-set-key [(meta    super <)]      '
;;lobal-set-key [(control meta super <)] '

;; ==================== = ====================
;;
;;lobal-set-key [(control =)]  '
;;lobal-set-key [(meta    =)]  'count-words-region)
;;lobal-set-key [(super   =)]  '
;;lobal-set-key [(control meta  =)]      '
;;lobal-set-key [(control super =)]      '
;;lobal-set-key [(meta    super =)]      '
;;lobal-set-key [(control meta super =)] '


;; ==================== > ====================
;;
(global-set-key [(control >)] 'xah-insert-tag)
;;lobal-set-key [(meta    >)] 'end-of-buffer)
(global-set-key [(super   >)] 'xah-insert-tag)
;;lobal-set-key [(control meta  >)]      '
;;lobal-set-key [(control super >)]      '
;;lobal-set-key [(meta    super >)]      '
;;lobal-set-key [(control meta super >)] '

;; ==================== 9 ====================
;;
;;lobal-set-key [(control ?)]  'undo-tree-redo)
;;lobal-set-key [(meta    ?)]  'xref-find-references)
;;lobal-set-key [(super   ?)]  '
;;lobal-set-key [(control meta  9)]      '
;;lobal-set-key [(control super 9)]      '
;;lobal-set-key [(meta    super 9)]      '
;;lobal-set-key [(control meta super 9)] '

;; ==================== @ ====================
;;
;;lobal-set-key [(control @)]  'cua-set-mark)
;;lobal-set-key [(meta    @)]  'mark-word)
;;lobal-set-key [(super   @)]  ' DO  NOT USE
;;lobal-set-key [(control meta  @)]      '
;;lobal-set-key [(control super @)]      '
;;lobal-set-key [(meta    super @)]      '
;;lobal-set-key [(control meta super @)] '

;; ==================== " ====================
;;
;;lobal-set-key [(control \")] '
;;lobal-set-key [(meta    \")] '
(global-set-key [(super   \")] 'xah-insert-double-quote)
;;lobal-set-key [(control meta  \")]      '
;;lobal-set-key [(control super \")]      '
;;lobal-set-key [(meta    super \")]      '
;;lobal-set-key [(control meta super \")] '

;; ==================== ' ====================
;;
;;lobal-set-key [(control \')] '
;;lobal-set-key [(meta    \')] 'abbrev-prefix-mark)
(global-set-key [(super   \')] 'xah-insert-single-quote)
;;lobal-set-key [(control meta  \')]      '
;;lobal-set-key [(control super \')]      '
;;lobal-set-key [(meta    super \')]      '
;;lobal-set-key [(control meta super \')] '

;; ==================== ( ====================
;;
;;lobal-set-key [(control \()] '
;;lobal-set-key [(meta    \()] 'insert-parentheses)
;;lobal-set-key [(super   \()] 'xah-insert-paren KEY NOT REGISTERED ???)
;;lobal-set-key [(control meta  \()]      '
;;lobal-set-key [(control super \()]      '
;;lobal-set-key [(meta    super \()]      '
;;lobal-set-key [(control meta super \()] '

;; ==================== ) ====================
;;
;;lobal-set-key [(control \))] '
;;lobal-set-key [(meta    \))] 'move-past-close-and-reindent)
;;lobal-set-key [(super   \))] 'move-past-close-and-reindent)
;;lobal-set-key [(control meta  \))]      '
;;lobal-set-key [(control super \))]      '
;;lobal-set-key [(meta    super \))]      '
;;lobal-set-key [(control meta super \))] '

;; ==================== [ ====================
;;
;;lobal-set-key [(control \[)] '
;;lobal-set-key [(meta    \[)] '
(global-set-key [(super   \[)] 'xah-insert-bracket)
;;lobal-set-key [(control meta  \[)]      '
;;lobal-set-key [(control super \[)]      '
;;lobal-set-key [(meta    super \[)]      '
;;lobal-set-key [(control meta super \[)] '

;; ==================== \ ====================
;;
;;lobal-set-key [(control \\)] 'toggle-input-method)
;;lobal-set-key [(meta    \\)] 'delete-horizontal-space)
;;lobal-set-key [(super   \\)] '
;;lobal-set-key [(control meta  \\)]      '
;;lobal-set-key [(control super \\)]      '
;;lobal-set-key [(meta    super \\)]      '
;;lobal-set-key [(control meta super \\)] '

;; ==================== ] ====================
;;
;;lobal-set-key [(control \])] 'abort-recursive-edit)
;;lobal-set-key [(meta    \])] '
;;lobal-set-key [(super   \])] '
;;lobal-set-key [(control meta  \])]      '
;;lobal-set-key [(control super \])]      '
;;lobal-set-key [(meta    super \])]      '
;;lobal-set-key [(control meta super \])] '

;; ==================== ` ====================
;;
(global-set-key [(control \`)] 'xah-insert-emacs-quote)
;;lobal-set-key [(meta    \`)] ' DO  NOT USE
;;lobal-set-key [(super   \`)] ')
;;lobal-set-key [(control meta  \`)]      '
;;lobal-set-key [(control super \`)]      '
;;lobal-set-key [(meta    super \`)]      '
;;lobal-set-key [(control meta super \`)] '

;; ==================== { ====================
;;
;;lobal-set-key [(control \{)] '
;;lobal-set-key [(meta    \{)] 'backward-paragraph)
(global-set-key [(super   \{)] 'xah-insert-brace)
;;lobal-set-key [(control meta  \{)]      '
;;lobal-set-key [(control super \{)]      '
;;lobal-set-key [(meta    super \{)]      '
;;lobal-set-key [(control meta super \{)] '

;; ==================== } ====================
;;
;;lobal-set-key [(control \})] '
;;lobal-set-key [(meta    \})] 'forward-paragraph)
;;lobal-set-key [(super   \})] '
;;lobal-set-key [(control meta  \})]      '
;;lobal-set-key [(control super \})]      '
;;lobal-set-key [(meta    super \})]      '
;;lobal-set-key [(control meta super \})] '

;; ==================== ^ ====================
;;
;;lobal-set-key [(control ^)]  '
;;lobal-set-key [(meta    ^)]  'delete-indentation)
;;lobal-set-key [(super   ^)]  ' KEY NOT REGISTERED
;;lobal-set-key [(control meta  ^)]      '
;;lobal-set-key [(control super ^)]      '
;;lobal-set-key [(meta    super ^)]      '
;;lobal-set-key [(control meta super ^)] '

;; ==================== _ ====================
;;
;;lobal-set-key [(control _)]  'undo-tree-undo)
;;lobal-set-key [(meta    _)]  'undo-tree-undo)
;;lobal-set-key [(super   _)]  '
;;lobal-set-key [(control meta  _)]      '
;;lobal-set-key [(control super _)]      '
;;lobal-set-key [(meta    super _)]      '
;;lobal-set-key [(control meta super _)] '

;; ==================== | ====================
;;
;;lobal-set-key [(control |)]  '
;;lobal-set-key [(meta    |)]  'shell-command-on-region)
;;lobal-set-key [(super   |)]  '
;;lobal-set-key [(control meta  |)]      '
;;lobal-set-key [(control super |)]      '
;;lobal-set-key [(meta    super |)]      '
;;lobal-set-key [(control meta super |)] '

;; ==================== ~ ====================
;;
;;lobal-set-key [(control ~)]  '
;;lobal-set-key [(meta    ~)]  ' DO  NOT USE
;;lobal-set-key [(super   ~)]  '
;;lobal-set-key [(control meta  ~)]      '
;;lobal-set-key [(control super ~)]      '
;;lobal-set-key [(meta    super ~)]      '
;;lobal-set-key [(control meta super ~)] '

;; ==================== TAB ====================
;;
;;lobal-set-key [TAB]           ' INDENT
(global-set-key [(control tab)] 'tjf:color/set-background-random)
(global-set-key [(meta    tab)] 'tjf:color/set-background-random)
;;lobal-set-key [(super   tab)] ' DO  NOT USE
;;lobal-set-key [(control meta  tab)] ' DO  NOT USE
(global-set-key [(control super tab)] 'clean-ndent--bsunindent)
;;lobal-set-key [(meta    super tab)] ')
;;lobal-set-key [(control meta super tab)] '

;;lobal-set-key [backtab] '

;; ==================== backspace ====================
;;
;;lobal-set-key [backspace]           'backward-delete-char-untabify
(global-set-key [(control backspace)] 'tjf:edit/delete-backward)
(global-set-key [(meta    backspace)] 'backward-kill-word)
;;lobal-set-key [(super   backspace)] 'tinyeat-delete-paragraph
;;lobal-set-key [(control meta  backspace)]      '
;;lobal-set-key [(control super backspace)]      '
;;lobal-set-key [(meta    super backspace)]      '
;;lobal-set-key [(control meta super backspace)] '

;; ==================== f1 ====================
;;
(global-set-key [f1]           'tjf:navigate/forward-word)
(global-set-key [(control f1)] 'tjf:edit/insert-chs)
;;lobal-set-key [(meta    f1)] ' DO NOT USE
(global-set-key [(super   f1)] 'tjf:navigate/forward-symbol)
;;lobal-set-key [(control meta  f1)] ' DO NOT USE
;;lobal-set-key [(control super f1)] '
;;lobal-set-key [(meta    super f1)] '
;;lobal-set-key [(control meta super f1)] ' DO NOT USE

;; ==================== f2 ====================
;;
(global-set-key [f2]           'backward-word)
(global-set-key [(control f2)] 'tjf:edit/insert-che)
;;lobal-set-key [(meta    f2)] ' DO NOT USE
(global-set-key [(super   f2)] 'backward-symbol)
;;lobal-set-key [(control meta  f2)]      ' DO NOT USE
;;lobal-set-key [(control super f2)]      '
;;lobal-set-key [(meta    super f2)]      '
;;lobal-set-key [(control meta super f2)] ' DO NOT USE

;; ==================== f3 ====================
;;
(global-set-key [f3]           'tjf:navigate/goto-end-of-line)
;;lobal-set-key [(control f3)] '
;;lobal-set-key [(meta    f3)] ' DO NOT USE
;;lobal-set-key [(super   f3)] '
;;lobal-set-key [(control meta  f3)]      ' DO NOT USE
;;lobal-set-key [(control super f3)]      '
;;lobal-set-key [(meta    super f3)]      '
;;lobal-set-key [(control meta super f3)] ' DO NOT USE

;; ==================== f4 ====================
;;
(global-set-key [f4]           'beginning-of-line)
;;lobal-set-key [(control f4)] '
;;lobal-set-key [(meta    f4)] ' DO NOT USE
;;lobal-set-key [(super   f4)] '
;;lobal-set-key [(control meta  f4)]      ' DO NOT USE
;;lobal-set-key [(control super f4)]      '
;;lobal-set-key [(meta    super f4)]      '
;;lobal-set-key [(control meta super f4)] ' DO NOT USE

;; ==================== f5 ====================
;;
(global-set-key [f5]           'transpose-chars)
;;lobal-set-key [(control f5)] '
;;lobal-set-key [(meta    f5)] ' DO NOT USE
;;lobal-set-key [(super   f5)] '
;;lobal-set-key [(control meta  f5)]      ' DO NOT USE
;;lobal-set-key [(control super f5)]      '
;;lobal-set-key [(meta    super f5)]      '
;;lobal-set-key [(control meta super f5)] ' DO NOT USE

;; ==================== f6 ====================
;;
(global-set-key [f6]           'tjf:edit/capitalize)
(global-set-key [(control f6)] 'capitalize-dwim)
;;lobal-set-key [(meta    f6)] ' DO NOT USE
;;lobal-set-key [(super f6)]   'xah-toggle-letter-case)
;;lobal-set-key [(control meta  f6)]      ' DO NOT USE
;;lobal-set-key [(control super f6)]      '
;;lobal-set-key [(meta    super f6)]      '
;;lobal-set-key [(control meta super f6)] ' DO NOT USE

;; ==================== f7 ====================
;;
(global-set-key [f7]           'tjf:edit/downcase)
;;lobal-set-key [(control f7)] 'downcase-word-or-region)
;;lobal-set-key [(meta    f7)] ' DO NOT USE
;;lobal-set-key [(super   f7)] '
;;lobal-set-key [(control meta  f7)]      ' DO NOT USE
;;lobal-set-key [(control super f7)]      '
;;lobal-set-key [(meta    super f7)]      '
;;lobal-set-key [(control meta super f7)] ' DO NOT USE

;; ==================== f8 ====================
;;
(global-set-key [f8]           'tjf:edit/upcase)
;;obal-set-key [(control f8)] 'upcase-word-or-region)
;;lobal-set-key [(meta    f8)] ' DO NOT USE
;;lobal-set-key [(super   f8)] '
;;lobal-set-key [(control meta  f8)]      ' DO NOT USE
;;lobal-set-key [(control super f8)]      '
;;lobal-set-key [(meta    super f8)]      '
;;lobal-set-key [(control meta super f8)] ' DO NOT USE

;; ==================== f9 ====================
;;
(global-set-key [f9]           'kill-word)
(global-set-key [(control f9)] 'tjf:edit/delete-word) ;; autoloaded
;;lobal-set-key [(meta    f9)] ' DO NOT        USE
;;lobal-set-key [(super   f9)] '
;;lobal-set-key [(control meta  f9)]      ' DO NOT USE
;;lobal-set-key [(control super f9)]      '
;;lobal-set-key [(meta    super f9)]      '
;;lobal-set-key [(control meta super f9)] ' DO NOT USE

;; ==================== f10 ====================
;;
(global-set-key [f10]           'tjf:edit/delete-line)
(global-set-key [(control f10)] 'tjf:edit/clear-line)
;;lobal-set-key [(meta    f10)] ' DO NOT USE
;;lobal-set-key [(super   f10)] '
;;lobal-set-key [(control meta  f10)]      ' DO NOT USE
;;lobal-set-key [(control super f10)]      '
;;lobal-set-key [(meta    super f10)]      '
;;lobal-set-key [(control meta super f10)] ' DO NOT USE

;; ==================== f11 ====================
;;
(global-set-key [f11]           'tjf:edit/delete-to-eol)
(global-set-key [(control f11)] 'tjf:edit/join-line-1)
;;lobal-set-key [(meta    f11)] 'toggle-maximized-frame
;;lobal-set-key [(super   f11)] '
;;lobal-set-key [(control meta  f11)]      ' DO NOT USE
;;lobal-set-key [(control super f11)]      '
;;lobal-set-key [(meta    super f11)]      '
;;lobal-set-key [(control meta super f11)] ' DO NOT USE

;; ==================== f12 ====================
;;
(global-set-key [f12]           'tjf:edit/delete-to-bol)
(global-set-key [(control f12)] 'delete-indentation)
;;lobal-set-key [(meta    f12)] ' DO NOT USE
;;lobal-set-key [(super   f12)] '
;;lobal-set-key [(control meta  f12)]      ' DO NOT USE
;;lobal-set-key [(control super f12)]      '
;;lobal-set-key [(meta    super f12)]      '
;;lobal-set-key [(control meta super f12)] ' DO NOT USE

;;  ==================== Print Screen ====================
;;
;;(global-set-key [(super print)] ')
;;(global-set-key [(control super print)] ')
;;(global-set-key [(meta    super print)] ')
;;lobal-set-key [(control meta  print)]      ' DO NOT USE
;;lobal-set-key [(control super print)]      '
;;lobal-set-key [(meta    super print)]      '
;;lobal-set-key [(control meta super print)] ' DO NOT USE

;; ==================== Scroll_Lock ====================
;;
(global-set-key [Scroll_Lock]           'completion-at-point)
;;lobal-set-key [(control Scroll_Lock)] '
;;lobal-set-key [(meta    Scroll_Lock)] '
;;lobal-set-key [(super   Scroll_Lock)] '
;;(global-set-key [(control meta  Scroll_Lock)] ')
;;(global-set-key [(control super Scroll_Lock)] ')
;;(global-set-key [(meta    super Scroll_Lock)] ')
;;lobal-set-key [(control   meta super Scroll_Lock)] '

;; ==================== pause ====================
;;
(global-set-key [pause]           'dabbrev-expand)
;;lobal-set-key [(control pause)] '
(global-set-key [(meta    pause)] 'sdcv-search)
;;lobal-set-key [(super   pause)] ' DO NOT USE
;;(global-set-key [(control meta  pause)] ')
;;(global-set-key [(control super pause)] ')
;;(global-set-key [(meta    super pause)] ')
;;lobal-set-key [(control   meta super pause)] '

;; ==================== return/enter/RET ====================
;;
;;lobal-set-key [RET]              'newline-and-indent)
(global-set-key [(control return)] 'tjf:edit/insert-newline-after)
(global-set-key [(meta    return)] 'tjf:edit/insert-newline-before)
;;lobal-set-key [(super   return)] '
;;lobal-set-key [(control meta  return)] ')
(global-set-key [(control super return)] 'tjf:edit/insert-newline-after-and-indent)
(global-set-key [(meta    super return)] 'tjf:edit/insert-newline-before-and-indent)
;;lobal-set-key [(control meta super return)] '

;; ==================== insert ====================
;;
(global-set-key [insert]           'tjf:clipboard/paste)
(global-set-key [(control insert)] 'overwrite-mode)
(global-set-key [(meta    insert)] 'tjf:date/insert-dd-mon-yyyy)
(global-set-key [(super   insert)] 'tjf:date/insert-month-day-year)
(global-set-key [(control meta  insert)] #'(lambda () (insert user-full-name)))
;;lobal-set-key [(control super insert)]      '
;;lobal-set-key [(meta    super insert)]      '
;;lobal-set-key [(control meta super insert)] '

;; ==================== return ====================
;;lobal-set-key [(return)]         'newline
(global-set-key [(control return)] 'tjf:edit/insert-newline-after)
(global-set-key [(meta    return)] 'tjf:edit/insert-newline-before)
;;lobal-set-key [(super   return)] 'insert-newline-before-and-indent
;;lobal-set-key [(control meta  return)] '
;;lobal-set-key [(control shift return)] 'insert-newline-after-and-indent
;;lobal-set-key [(control super return)] '
;;lobal-set-key [(meta    super return)] '
;;lobal-set-key [(control meta super return)] '

;; ==================== delete ====================
;;
(global-set-key [delete]           'delete-char)
(global-set-key [(control delete)] 'tjf:edit/delete-forward) ;; autoloaded
;;lobal-set-key [(meta    delete)] 'clean-aindent--bsunindent)
(global-set-key [(super   delete)] #'(lambda nil (interactive) (delete-region (point-min) (point-max))))
;;lobal-set-key [(control meta  delete)] ' DO NOT USE
(global-set-key [(control super delete)] #'(lambda nil (interactive) (delete-region (point) (point-max))))
(global-set-key [(meta    super delete)] #'(lambda nil (interactive) (delete-region (point-min) (point))))
;;lobal-set-key [(control meta super delete)] ' DOES NOT REGISTER

;; ==================== home ====================
;;
(global-set-key [home]           'beginning-of-buffer)
(global-set-key [(control home)] 'beginning-of-line)
(global-set-key [(meta    home)] 'move-to-window-line-top-bottom)
;;lobal-set-key [(super   home)] ' DOES NOT REGISTER
;;lobal-set-key [(control meta  home)] 'beginning-of-defun
;;lobal-set-key [(control super home)] '
;;lobal-set-key [(meta    super home)] '
;;lobal-set-key [(control meta super home)] '

;; ==================== end ====================
;;
(global-set-key [end]           'end-of-buffer)
(global-set-key [(control end)] 'end-of-line)
;;lobal-set-key [(meta    end)] 'end-of-buffer-other-window)
;;lobal-set-key [(super   end)] '
;;lobal-set-key [(control meta  end)]      '
;;lobal-set-key [(control super end)]      '
;;lobal-set-key [(meta    super end)]      '
;;lobal-set-key [(control meta super end)] '

;; ==================== prior ====================
;;
;;lobal-set-key [prior]           'scroll-down-command) ;; Page Up
;;lobal-set-key [(control prior)] 'scroll-right)
;;lobal-set-key [(meta    prior)] 'scroll-right)
;;lobal-set-key [(super   prior)] '
;;lobal-set-key [(control meta  prior)]      '
;;lobal-set-key [(control super prior)]      '
;;lobal-set-key [(meta    super prior)]      '
;;lobal-set-key [(control meta super prior)] '

;; ==================== next ====================
;;
;;lobal-set-key [next]           'scroll-up-command)   ;; Page Down
;;lobal-set-key [(control next)] 'scroll-left)
;;lobal-set-key [(meta    next)] 'scroll-left)
;;lobal-set-key [(super   next)] '
;;lobal-set-key [(control meta  next)]      '
;;lobal-set-key [(control super next)]      '
;;lobal-set-key [(meta    super next)]      '
;;lobal-set-key [(control meta super next)] '

;; ==================== up ====================
;;
;;lobal-set-key [up]           'previous-line
;;lobal-set-key [(control up)] 'scroll-down-line)
;;lobal-set-key [(meta    up)] 'scroll-down-line)
;;lobal-set-key [(super   up)] '
;;lobal-set-key [(control meta  up)] '
(global-set-key [(control shift up)] 'ergoemacs-move-text-up)
;;lobal-set-key [(control super up)] '
;;lobal-set-key [(meta    super up)] '
;;lobal-set-key [(control meta super up)] '

;; ==================== left ====================
;;
;;lobal-set-key [left]           'left-char)
;;lobal-set-key [(control left)] 'left-word)
;;lobal-set-key [(meta    left)] 'left-word)
;;lobal-set-key [(super   left)] 'search-word-backward)
;;lobal-set-key [(control meta  left)] ' DOES NOT REGISTER
;;lobal-set-key [(control shift left)] 'ergoemacs-backward-open-bracket
;;lobal-set-key [(control super left)] ' DO NOT USE
(global-set-key [(meta    super left)] 'backward-symbol)
;;lobal-set-key [(control meta super left)] '

;; ==================== down ====================
;;
;;lobal-set-key [down]           'next-line)
;;lobal-set-key [(control down)] 'scroll-up-line)
;;lobal-set-key [(meta    down)] 'scroll-up-line)
;;lobal-set-key [(super   down)] '
;;lobal-set-key [(control meta  down)] '
(global-set-key [(control shift down)] 'ergoemacs-move-text-down)
;;lobal-set-key [(control super down)] '
;;lobal-set-key [(meta    super down)] '
;;lobal-set-key [(control meta super down)] '

;; ==================== right ====================
;;
;;lobal-set-key [right]           'right-char)
;;lobal-set-key [(control right)] 'right-word)
(global-set-key [(meta    right)] 'tjf:navigate/forward-word)
;;lobal-set-key [(super   right)] 'search-word-forward)
;;lobal-set-key [(control meta  right)] ' DOES NOT REGISTER
;;lobal-set-key [(control shift right)] 'ergoemacs-forward-open-bracket
;;lobal-set-key [(control super right)] ' DO NOT USE
(global-set-key [(meta    super right)] 'tjf:navigate/forward-symbol)
;;lobal-set-key [(control meta super right)] '

;; ==================== kp-divide ====================
;;
(global-set-key [kp-divide]           'tjf:edit/toggle-fill)
;;lobal-set-key [(control kp-divide)] 'undo-tree-undo)
;;lobal-set-key [(meta    kp-divide)] 'dabbrev-expand)
;;lobal-set-key [(super   kp-divide)] '
;;lobal-set-key [(control meta  kp-divide)]      '
(global-set-key [(control super kp-divide)]      'tjf:frame/half-size)
;;lobal-set-key [(meta    super kp-divide)]      '
;;lobal-set-key [(control meta super kp-divide)] 'tjf:edit/

;; ==================== kp-multiply ====================
;;
(global-set-key [kp-multiply]           'dabbrev-expand)
;;lobal-set-key [(control kp-multiply)] '
;;lobal-set-key [(meta    kp-multiply)] '
;;lobal-set-key [(super   kp-multiply)] '
;;lobal-set-key [(control meta  kp-multiply)]      '
;;lobal-set-key [(control super kp-multiply)]      '
;;lobal-set-key [(meta    super kp-multiply)]      '
;;lobal-set-key [(control meta super kp-multiply)] '

;; ==================== kp-subtract ====================
;;
(global-set-key [kp-subtract]           'search-word-backward)
(global-set-key [(control kp-subtract)] 'shift-number-down)
;;lobal-set-key [(meta    kp-subtract)] 'negative-argument)
;;lobal-set-key [(super   kp-subtract)] '
;;lobal-set-key [(control meta  kp-subtract)]      '
;;lobal-set-key [(control super kp-subtract)]      '
;;lobal-set-key [(meta    super kp-subtract)]      '
;;lobal-set-key [(control meta super kp-subtract)] '

;; ==================== kp-add ====================
;;
(global-set-key [kp-add]           'search-word-forward)
(global-set-key [(control kp-add)] 'shift-number-up)
;;lobal-set-key [(meta    kp-add)] '
;;lobal-set-key [(super   kp-add)] '
;;lobal-set-key [(control meta  kp-add)]      '
;;lobal-set-key [(control super kp-add)]      '
;;lobal-set-key [(meta    super kp-add)]      '
;;lobal-set-key [(control meta super kp-add)] '

;; ==================== kp-enter ====================
;;
(global-set-key [kp-enter]           'tjf:duplicate/previous)
;;lobal-set-key [(control kp-enter)] ' DO NOT USE ... messes up C-enter
(global-set-key [(meta    kp-enter)] 'tjf:edit/insert-newline-before)
(global-set-key [(super   kp-enter)] 'tjf:duplicate/tabs)
;;lobal-set-key [(control meta  kp-enter)]      ' DO NOT USE
;;lobal-set-key [(control super kp-enter)]      '
;;lobal-set-key [(meta    super kp-enter)]      '
;;lobal-set-key [(control meta super kp-enter)] '

(global-set-key [kp-decimal]           'delete-horizontal-space)
;;lobal-set-key [(control kp-decimal)] '
;;lobal-set-key [(meta    kp-decimal)] 'xref-find-definitions)
;;lobal-set-key [(super   kp-decimal)] '

;; ==================== kp-0/kp-insert ====================
;;
(global-set-key [kp-0] 'tjf:duplicate/line-or-region)
;;lobal-set-key [(control kp-0)] ' DO NOT USE ... digit argument
(global-set-key [(meta    kp-0)] 'tjf:duplicate/line-or-region)
;;lobal-set-key [(super   kp-0)] '
;;lobal-set-key [(control meta  kp-0)] ' DO NOT USE
(global-set-key [(control super kp-0)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  0)))
(global-set-key [(meta    super kp-0)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 0)))
;;lobal-set-key [(control meta super kp-0)] '

(global-set-key [kp-insert] 'duplicate-line-or-region)
;;lobal-set-key [(control kp-insert)] ' DO NOT USE ... digit argument
(global-set-key [(meta    kp-insert)] 'duplicate-line-or-region)
;;lobal-set-key [(super   kp-insert)] '
;;lobal-set-key [(control meta  kp-insert)] ' DO NOT USE
(global-set-key [(control super kp-insert)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  0)))
(global-set-key [(meta    super kp-insert)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 0)))
;;lobal-set-key [(control meta super kp-insert)] '

;; ==================== kp-1/kp-end ====================
;;
(global-set-key [kp-1] 'tjf:duplicate/previous)
;;lobal-set-key [(control kp-1)] ' DO NOT USE ... digit argument
(global-set-key [(meta    kp-1)] 'tjf:duplicate/next)
;;lobal-set-key [(super   kp-1)] '
;;lobal-set-key [(control meta  kp-1)] ' DO NOT USE
(global-set-key [(control super kp-1)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  1)))
(global-set-key [(meta    super kp-1)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 1)))
;;lobal-set-key [(control meta super kp-1)] '

(global-set-key [kp-end] 'tjf:duplicate/previous)
;;lobal-set-key [(control kp-end)] ' DO NOT USE ... digit argument
(global-set-key [(meta    kp-end)] 'tjf:duplicate/next)
;;lobal-set-key [(super   kp-end)] '
;;lobal-set-key [(control meta  kp-end)] ' DO NOT USE
(global-set-key [(control super kp-end)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  1)))
(global-set-key [(meta    super kp-end)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 1)))
;;lobal-set-key [(control meta super kp-end)] '

;; ==================== kp-2/kp-down ====================
;;
(global-set-key [kp-2]           #'(lambda () (interactive "*") (tjf:duplicate/syntax -2)))
;;lobal-set-key [(control kp-2)] ' DO NOT USE  ... digit argument
(global-set-key [(meta    kp-2)] #'(lambda () (interactive "*") (tjf:duplicate/syntax 2)))
;;lobal-set-key [(super   kp-2)] '
;;lobal-set-key [(control meta  kp-2)] ' DO NOT USE
(global-set-key [(control super kp-2)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  2)))
(global-set-key [(meta    super kp-2)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 2)))
;;lobal-set-key [(control meta super kp-2)] '

(global-set-key [kp-down]           #'(lambda () (interactive "*") (tjf:duplicate/syntax -2)))
;;lobal-set-key [(control kp-down)] ' DO NOT USE  ... digit argument
(global-set-key [(meta    kp-down)] #'(lambda () (interactive "*") (tjf:duplicate/syntax 2)))
;;lobal-set-key [(super   kp-down)] '
;;lobal-set-key [(control meta  kp-down)] ' DO NOT USE
(global-set-key [(control super kp-down)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  2)))
(global-set-key [(meta    super kp-down)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 2)))
;;lobal-set-key [(control meta super kp-down)] '

;; ==================== kp-3/kp-next ====================
;;
(global-set-key [kp-3]           #'(lambda () (interactive "*") (tjf:duplicate/syntax -3)))
;;lobal-set-key [(control kp-3)] ' DO NOT USE ... digit argument
(global-set-key [(meta    kp-3)] #'(lambda () (interactive "*") (tjf:duplicate/syntax 3)))
;;lobal-set-key [(super   kp-3)] '
;;lobal-set-key [(control meta  kp-3)] ' DO NOT USE
(global-set-key [(control super kp-3)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  3)))
(global-set-key [(meta    super kp-3)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 3)))
;;lobal-set-key [(control meta super kp-3)] '

(global-set-key [kp-next]           #'(lambda () (interactive "*") (tjf:duplicate/syntax -3)))
;;lobal-set-key [(control kp-next)] ' DO NOT USE ... digit argument
(global-set-key [(meta    kp-next)] #'(lambda () (interactive "*") (tjf:duplicate/syntax 3)))
;;lobal-set-key [(super   kp-next)] '
;;lobal-set-key [(control meta  kp-next)] ' DO NOT USE
(global-set-key [(control super kp-next)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  3)))
(global-set-key [(meta    super kp-next)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 3)))
;;lobal-set-key [(control meta super kp-next)] '

;; ==================== kp-4/kp-left ====================
;;
(global-set-key [kp-4]           #'(lambda () (interactive "*") (tjf:duplicate/syntax -4)))
;;lobal-set-key [(control kp-4)] ' DO NOT USE ... digit argument
(global-set-key [(meta    kp-4)] #'(lambda () (interactive "*") (tjf:duplicate/syntax 4)))
;;lobal-set-key [(super   kp-4)] '
;;lobal-set-key [(control meta  kp-4)] ' DO NOT USE
(global-set-key [(control super kp-4)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  4)))
(global-set-key [(meta    super kp-4)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 4)))
;;lobal-set-key [(control meta super kp-4)] '

;(global-set-key [kp-left]           #'(lambda () (interactive "*") (tjf:duplicate/syntax -4)))
;;lobal-set-key [(control kp-left)] ' DO NOT USE ... digit argument
(global-set-key [(meta    kp-left)] #'(lambda () (interactive "*") (tjf:duplicate/syntax 4)))
;;lobal-set-key [(super   kp-left)] '
;;lobal-set-key [(control meta  kp-left)] ' DO NOT USE
(global-set-key [(control super kp-left)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  4)))
(global-set-key [(meta    super kp-left)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 4)))
;;lobal-set-key [(control meta super kp-left)] '

;; ==================== kp-5/kp-begin ====================
;;
(global-set-key [kp-5]           #'(lambda () (interactive "*") (tjf:duplicate/syntax -5)))
;;lobal-set-key [(control kp-5)] ' DO NOT USE ... digit argument
(global-set-key [(meta    kp-5)] #'(lambda () (interactive "*") (tjf:duplicate/syntax 5)))
;;lobal-set-key [(super   kp-5)] '
;;lobal-set-key [(control meta  kp-5)] ' DO NOT USE
(global-set-key [(control super kp-5)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  5)))
(global-set-key [(meta    super kp-5)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 5)))
;;lobal-set-key [(control meta super kp-5)] '

(global-set-key [kp-begin]           #'(lambda () (interactive "*") (tjf:duplicate/syntax -5)))
;;lobal-set-key [(control kp-begin)] ' DO NOT USE ... digit argument
(global-set-key [(meta    kp-begin)] #'(lambda () (interactive "*") (tjf:duplicate/syntax 5)))
;;lobal-set-key [(super   kp-begin)] '
;;lobal-set-key [(control meta  kp-begin)] ' DO NOT USE
(global-set-key [(control super kp-begin)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  5)))
(global-set-key [(meta    super kp-begin)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 5)))
;;lobal-set-key [(control meta super kp-begin)] '

;; ==================== kp-6/kp-right ====================
;;
(global-set-key [kp-6]           #'(lambda () (interactive "*") (tjf:duplicate/syntax -6)))
;;lobal-set-key [(control kp-6)] ' DO NOT USE ... digit argument
(global-set-key [(meta kp-6)]    #'(lambda () (interactive "*") (tjf:duplicate/syntax 6)))
;;lobal-set-key [(super   kp-6)] '
;;lobal-set-key [(control meta  kp-6)] ' DO NOT USE
(global-set-key [(control super kp-6)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  6)))
(global-set-key [(meta    super kp-6)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 6)))
;;lobal-set-key [(control meta super kp-6)] '

(global-set-key [kp-right]           #'(lambda () (interactive "*") (tjf:duplicate/syntax -6)))
;;lobal-set-key [(control kp-right)] ' DO NOT USE ... digit argument
(global-set-key [(meta kp-right)]    #'(lambda () (interactive "*") (tjf:duplicate/syntax 6)))
;;lobal-set-key [(super   kp-right)] '
;;lobal-set-key [(control meta  kp-right)] ' DO NOT USE
(global-set-key [(control super kp-right)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  6)))
(global-set-key [(meta    super kp-right)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 6)))
;;lobal-set-key [(control meta super kp-right)] '

;; ==================== kp-7/kp-home ====================
;;
(global-set-key [kp-7]           #'(lambda () (interactive "*") (tjf:duplicate/syntax -7)))
;;lobal-set-key [(control kp-7)] ' DO NOT USE ... digit argument
(global-set-key [(meta    kp-7)] #'(lambda () (interactive "*") (tjf:duplicate/syntax 7)))
;;lobal-set-key [(super   kp-7)] '
;;lobal-set-key [(control meta  kp-7)] ' DO NOT USE
(global-set-key [(control super kp-7)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  7)))
(global-set-key [(meta    super kp-7)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 7)))
;;lobal-set-key [(control meta super kp-7)] '

(global-set-key [kp-home]           #'(lambda () (interactive "*") (tjf:duplicate/syntax -7)))
;;lobal-set-key [(control kp-home)] ' DO NOT USE ... digit argument
(global-set-key [(meta    kp-home)] #'(lambda () (interactive "*") (tjf:duplicate/syntax 7)))
;;lobal-set-key [(super   kp-home)] '
;;lobal-set-key [(control meta  kp-home)] ' DO NOT USE
(global-set-key [(control super kp-home)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  7)))
(global-set-key [(meta    super kp-home)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 7)))
;;lobal-set-key [(control meta super kp-home)] '

;; ==================== kp-8/kp-up ====================
;;
(global-set-key [kp-8]           #'(lambda () (interactive "*") (tjf:duplicate/syntax -8)))
;;lobal-set-key [(control kp-8)] ' DO NOT USE ... digit argument
(global-set-key [(meta    kp-8)] #'(lambda () (interactive "*") (tjf:duplicate/syntax 8)))
;;lobal-set-key [(super   kp-8)] '
;;lobal-set-key [(control meta  kp-8)] ' DO NOT USE
(global-set-key [(control super kp-8)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  8)))
(global-set-key [(meta    super kp-8)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 8)))
;;lobal-set-key [(control meta super kp-8)] '

(global-set-key [kp-up]           #'(lambda () (interactive "*") (tjf:duplicate/syntax -8)))
;;lobal-set-key [(control kp-up)] ' DO NOT USE ... digit argument
(global-set-key [(meta    kp-up)] #'(lambda () (interactive "*") (tjf:duplicate/syntax 8)))
;;lobal-set-key [(super   kp-up)] '
;;lobal-set-key [(control meta  kp-up)] ' DO NOT USE
(global-set-key [(control super kp-up)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  8)))
(global-set-key [(meta    super kp-up)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 8)))
;;lobal-set-key [(control meta super kp-up)] '

;; ==================== kp-9/kp-prior ====================
;;
(global-set-key [kp-9]           #'(lambda () (interactive "*") (tjf:duplicate/syntax -9)))
;;lobal-set-key [(control kp-9)] ' DO NOT USE ... digit argument
(global-set-key [(meta    kp-9)] #'(lambda () (interactive "*") (tjf:duplicate/syntax 9)))
;;lobal-set-key [(super   kp-9)] '
;;lobal-set-key [(control meta  kp-9)] ' DO NOT USE
(global-set-key [(control super kp-9)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  9)))
(global-set-key [(meta    super kp-9)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 9)))
;;lobal-set-key [(control meta super kp-9)] '

(global-set-key [kp-prior]           #'(lambda () (interactive "*") (tjf:duplicate/syntax -9)))
;;lobal-set-key [(control kp-prior)] ' DO NOT USE ... digit argument
(global-set-key [(meta    kp-prior)] #'(lambda () (interactive "*") (tjf:duplicate/syntax 9)))
;;lobal-set-key [(super   kp-prior)] '
;;lobal-set-key [(control meta  kp-prior)] ' DO NOT USE
(global-set-key [(control super kp-prior)] #'(lambda () (interactive "*") (tjf:clipboard/copy-n  9)))
(global-set-key [(meta    super kp-prior)] #'(lambda () (interactive "*") (tjf:clipboard/paste-n 9)))
;;lobal-set-key [(control meta super kp-prior)] '

;;; ================================================================================
;;;; Bindings for mouse commands.

;; ==================== header-line mouse ====================
;;
;; (global-set-key [header-line (control mouse-1)]       ')
;; (global-set-key [header-line (control shift mouse-1)] ')
;; (global-set-key [header-line (control mouse-3)]       ')
;; (global-set-key [header-line (control shift mouse-3)] ')

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

(global-set-key [(control shift mouse-1)] #'(lambda (e) (interactive "e") (hs-minor-mode 1)(hs-toggle-hiding e)))
(global-set-key [(control shift mouse-3)] 'ffap-at-mouse)

(global-set-key [(meta    shift mouse-3)] #'(lambda (e) (interactive "e")(let ((ffap-file-finder 'find-file-other-frame)) (ffap-at-mouse e))))
;;
;; Mode Line Mouse
;;
;;(global-set-key [mode-line mouse-1] 'mouse-drag-mode-line)
(global-set-key [mode-line mouse-2] 'split-window-vertically)
(global-set-key [mode-line mouse-3] 'split-window-horizontally)
(global-set-key [mode-line mouse-8] 'mouse-delete-window)
(global-set-key [mode-line mouse-9] 'mouse-delete-other-windows)

(global-set-key [mode-line (control mouse-3)] 'balance-windows)
;;
;; Fringe Mouse
;;
(global-set-key [left-fringe  mouse-1] 'bm-toggle-mouse)
(global-set-key [left-fringe  mouse-4] 'bm-previous-mouse)
(global-set-key [left-fringe  mouse-5] 'bm-next-mouse)
;; (global-set-key [right-fringe mouse-1] 'mouse-delete-other-windows)
;; (global-set-key [right-fringe mouse-2] 'split-window-horizontally)

;; (global-set-key [right-fringe (control mouse-1)] 'mouse-delete-window)

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
;;lobal-set-key [mode-line C-mouse-2] 'split-window-vertically)
(global-set-key [mode-line C-mouse-3] 'mouse-delete-window)

;;lobal-set-key [mode-line C-S-mouse-1] ')
;;lobal-set-key [mode-line C-S-mouse-2] ')
(global-set-key [mode-line C-S-mouse-3] 'minions-minor-modes-menu)

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
(message "Loading tjf-keys...done")
(provide 'tjf-keys)

;;; tjf-keys.el ends here
