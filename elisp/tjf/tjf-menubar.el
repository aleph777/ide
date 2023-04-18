;;; tjf-menubar.el --- Custom menubar support -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

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

;; Revision: 23-Jun-2000 Changed menu-bar-final-items to use ‘Help’ instead of ‘help-menu’
;;                       Added usr-reset-window-min-height
;;           16-Nov-2001 Revised for Emacs 21
;;           20-Nov-2001 Added delete-whitespace-rectangle
;;           06-Dec-2001 Removed ‘Faces’ from ‘Tools’ menu
;;                       Removed items and references to ‘indented-text-mode’
;;           07-Dec-2001 Added ‘pquery-replace’
;;           22-Jun-2006 Added ‘usr-join-paragraphs’
;;           02-Feb-2010 Added entry and exit messages
;;                       Changed to ‘easy-menu-add-item’ for external easy submenus
;;           06-May-2014 Changed from ‘toggle-read-only’ to read-only for Emacs 24.3
;;           24-May-2014 Updated menu entry names for usability
;;           27-May-2014 Added line number mode entry to Tools menu
;;           17-Jun-2014 Added ‘usr-copy-buffer’ to Edit menu
;;           24-Mar-2015 Changed ‘split-window’ to ‘split-window-vertically’
;;           25-Mar-2015 Changed ‘unscroll’ to undo scroll and redo scroll (‘atim-unscroll’)
;;                       Updated from ‘moccur’ to ‘multi-occur-in-matching-buffers’
;;           27-Mar-2015 Added ‘apropos’ to Help menu
;;           05-Jan-2016 Removed ‘usr-d2u’ and ‘usr-u2d’
;;                       Added ‘usr-set-unix-file’, ‘usr-set-dos-file’, and ‘usr-set-mac-file’
;;           18-Jan-2016 Updated for new user interface
;;           04-Feb-2016 Added ‘ergoemacs-select-text-in-quote’
;;           09-Feb-2016 Moved clipboard menu to just below Cut-Copy-Paste
;;           10-Feb-2016 Added Insert Symbol menu to Tools
;;           25-Feb-2016 Added library ‘u-navigate’
;;                       Added library ‘u-search’
;;                       Replaced ‘usr-flags’ with ‘u-flags’
;;           14-Jan-2017 Added Bookmark menu
;;           22-Jul-2019 Moved Bookmark menu to Navigate
;;           03-Feb-2021 ‘tjf’ overhaul
;;           15-Jul-2021 Sorted Edit menu
;;           27-Sep-2022 Added ‘tjf:menu-bar/feature’
;;

;;; Code:

(message "Loading tjf-menubar...")
(require 'easymenu)
(require 'tjf-clipboard)
(require 'tjf-edit)
(require 'tjf-file)
(require 'tjf-flags)
(require 'tjf-navigate)
(require 'tjf-search)
(require 'tjf-sort)
(require 'tjf-tools)
(require 'tjf-view)
(require 'undo-tree)

(define-key global-map [menu-bar] (make-sparse-keymap "menu-bar"))
(define-key global-map [menu-bar buffer] (cons "Window" global-buffers-menu-map))
(setq menu-bar-final-items '(buffer help))

(defun tjf:menu-bar/feature (feature)
  "Show message indicating that FEATURE is a feature."
  (interactive "sFeature: ")
  (let ((feature-symbol (intern feature)))
    (message "FEATURE: %s" (featurep feature-symbol))))

(easy-menu-define help-menu global-map "Help"
  '("Help"
    ["Apropos..."                apropos            :active t :key-sequence nil]
    ["Where is..."               where-is           :active t :key-sequence [C-h w]]
    ["Describe Key..."           describe-key       :active t :key-sequence [C-h k]]
    ["Describe Function..."      describe-function  :active t :key-sequence [C-h f]]
    ["Describe Variable..."      describe-variable  :active t :key-sequence [C-h v]]
    ["Describe Mode..."          describe-mode      :active t :key-sequence [C-h m]]
    ["Emacs Command Apropos... " command-apropos    :active t :key-sequence nil]
    ["Info"                      info               :active t :key-sequence [C-h i]]
    ["Unix Manpage…"             manual-entry       :active t :key-sequence nil]
    "---"
    ["Feature?..." tjf:menu-bar/feature :active t]
    ))

(easy-menu-define tools-menu     global-map "Tools"     tjf:tools/menu)
(define-key-after tools-menu [diff-menu] '("Diff"  . menu-bar-ediff-menu) t)

(defalias 'set-coding 'set-buffer-file-coding-system)

(easy-menu-define encoding-menu global-map "Encoding"
  '("Encoding"
    ["UTF-8 Unix"    (set-coding 'utf-8-unix) :style toggle :selected (tjf:flags/is-utf-8-unix?) :enable (tjf:flags/enable-encoding?)]
    ["UTF-8 Windows" (set-coding 'utf-8-dos)  :style toggle :selected (tjf:flags/is-utf-8-dos?)  :enable (tjf:flags/enable-encoding?)]
    ["UTF-8 Mac"     (set-coding 'utf-8-mac)  :style toggle :selected (tjf:flags/is-utf-8-mac?)  :enable (tjf:flags/enable-encoding?)]
    "---"
    ["UTF-8 With BOM Unix"    (set-coding 'utf-8-with-signature-unix) :style toggle :selected (tjf:flags/is-utf-8-bom-unix?) :enable (tjf:flags/enable-encoding?)]
    ["UTF-8 With BOM Windows" (set-coding 'utf-8-with-signature-dos)  :style toggle :selected (tjf:flags/is-utf-8-bom-dos?)  :enable (tjf:flags/enable-encoding?)]
    ["UTF-8 With BOM Mac"     (set-coding 'utf-8-with-signature-mac)  :style toggle :selected (tjf:flags/is-utf-8-bom-mac?)  :enable (tjf:flags/enable-encoding?)]
    "---"
    ["UCS-2 BE BOM Unix"    (set-coding 'utf-16be-with-signature-unix) :style toggle :selected (tjf:flags/is-utf-16be-bom-unix?) :enable (tjf:flags/enable-encoding?)]
    ["UCS-2 BE BOM Windows" (set-coding 'utf-16be-with-signature-dos)  :style toggle :selected (tjf:flags/is-utf-16be-bom-dos?)  :enable (tjf:flags/enable-encoding?)]
    ["UCS-2 BE BOM Mac"     (set-coding 'utf-16be-with-signature-mac)  :style toggle :selected (tjf:flags/is-utf-16be-bom-mac?)  :enable (tjf:flags/enable-encoding?)]
    "---"
    ["UCS-2 LE BOM Unix"    (set-coding 'utf-16le-with-signature-unix) :style toggle :selected (tjf:flags/is-utf-16le-bom-unix?) :enable (tjf:flags/enable-encoding?)]
    ["UCS-2 LE BOM Windows" (set-coding 'utf-16le-with-signature-dos)  :style toggle :selected (tjf:flags/is-utf-16le-bom-dos?)  :enable (tjf:flags/enable-encoding?)]
    ["UCS-2 LE BOM Mac"     (set-coding 'utf-16le-with-signature-mac)  :style toggle :selected (tjf:flags/is-utf-16le-bom-mac?)  :enable (tjf:flags/enable-encoding?)]
    ))

(easy-menu-define view-menu   global-map "View"   tjf:view/menu)
(easy-menu-define search-menu global-map "Search" tjf:search/menu)

(easy-menu-define edit-menu global-map "Edit" tjf:edit/menu)
(easy-menu-add-item nil '("Edit") tjf:clipboard/menu   'marker1)
(define-key-after edit-menu [xxx1]      '(menu-item "--")                 'marker1)
(define-key-after edit-menu [yank-menu] '("Select and Paste" . yank-menu) 'marker1)
(define-key-after edit-menu [xxx2]      '(menu-item "--")                 'marker1)
(easy-menu-add-item nil '("Edit") tjf:edit/menu-align                     'marker2)
(easy-menu-add-item nil '("Edit") tjf:edit/menu-case                      'marker2)
(easy-menu-add-item nil '("Edit") tjf:edit/menu-comment                   'marker2)
(easy-menu-add-item nil '("Edit") tjf:edit/menu-delete                    'marker2)
(easy-menu-add-item nil '("Edit") tjf:edit/menu-indent                    'marker2)
(easy-menu-add-item nil '("Edit") tjf:edit/menu-justify                   'marker2)
(easy-menu-add-item nil '("Edit") tjf:edit/menu-rectangle                 'marker2)
(easy-menu-add-item nil '("Edit") tjf:sort/menu                           'marker2)
(easy-menu-add-item nil '("Edit") tjf:edit/menu-whitespace                'marker2)

(easy-menu-define file-menu global-map "File" tjf:file/menu)

;;
(message "Loading tjf-menubar...done")
(provide 'tjf-menubar)

;;; tjf-menubar.el ends here
