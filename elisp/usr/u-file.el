;;; u-file.el --- File menu and associated local functions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2018 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   28-Feb-2016

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

;; Revision: 02-Sep-2016 Changed `usr-exit-buffer-frame' to `exit-buffer-and-frame'
;;           19-Apr-2018 Removed ‘u-initial-major-mode’ from ‘new-empty-buffer’
;;           13-Jun-2018 Updated ‘require’ list
;;

;;; Code:

(message "Loading u-file...")
(require 'u-frame)
(require 'u-flags)

;;
(defun new-empty-buffer ()
  "Open a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "new")))
    (switch-to-buffer buf)
    (funcall 'text-mode)
    (setq buffer-offer-save t)))

(defvar u-file-menu
  '("File"
    ["New" new-empty-buffer :enable (menu-bar-non-minibuffer-window-p) :key-sequence [C-n]]
    "---"
    ["View..."               view-file             :enable (menu-bar-non-minibuffer-window-p)]
    ["Open..."               find-file             :enable (menu-bar-non-minibuffer-window-p)]
    ["Open in New Window..." find-file-other-frame :enable (menu-bar-non-minibuffer-window-p)]
    "---"
    ["Reload" revert-buffer :enable (enable-revert?)]
    "---"
    ["Toggle Read-Only" read-only-mode :style toggle :selected buffer-read-only]
    "---"
    ["Insert File..."        insert-file   :enable (menu-bar-non-minibuffer-window-p)]
    ["Insert from Window..." insert-buffer :enable (menu-bar-non-minibuffer-window-p)]
    "---"
    ["Save"                  save-buffer  :enable (enable-save?)  ]
    ["Save As... "           write-file   :enable (enable-saveas?)]
    ["Save Selection As... " write-region :active mark-active]
    "---"
    ["Close"        kill-this-buffer      :enable (kill-this-buffer-enabled-p)]
    ["Close Window" exit-buffer-and-frame :enable (delete-frame-enabled-p)    ]
    "---"
    "---"
    ["Exit" save-buffers-kill-terminal :active t]
    ))

;;
(message "Loading u-file...done")
(provide 'u-file)
;;; u-file.el ends here
