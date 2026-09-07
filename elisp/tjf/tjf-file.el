;;; tjf-file.el --- File menu and associated local functions -*-lexical-binding: t-*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2026 Tom Fontaine

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

;;; Code:

(message "Loading tjf-file...")
(require 'tjf-frame)
(require 'tjf-flags)

;;
(defvar tjf:file/menu)
(setq tjf:file/menu
  '("File"
    ["New" tjf:file/new-empty-buffer :enable (menu-bar-non-minibuffer-window-p) :key-sequence [C-n]]
    ["---" nil :visible t :enable nil]
    ["View..."               view-file             :enable (menu-bar-non-minibuffer-window-p)]
    ["Open..."               find-file             :enable (menu-bar-non-minibuffer-window-p)]
    ["Open in New Window..." find-file-other-frame :enable (menu-bar-non-minibuffer-window-p)]
    ["---" nil :visible t :enable nil]
    ["Reload" revert-buffer :enable (tjf:flags/enable-revert?)]
    ["---" nil :visible t :enable nil]
    ["Toggle Read-Only" read-only-mode :style toggle :selected buffer-read-only]
    ["---" nil :visible t :enable nil]
    ["Insert File..."        insert-file   :enable (menu-bar-non-minibuffer-window-p)]
    ["Insert from Window..." insert-buffer :enable (menu-bar-non-minibuffer-window-p)]
    ["---" nil :visible t :enable nil]
    ["Save"                  save-buffer  :enable (tjf:flags/enable-save?)  ]
    ["Save As... "           write-file   :enable (tjf:flags/enable-saveas?)]
    ["Save Selection As... " write-region :active mark-active               ]
    ["---" nil :visible t :enable nil]
    ["Rename..." tjf:file/rename-this-file-and-buffer :enable (tjf:flags/enable-saveas?)]
    ["---" nil :visible t :enable nil]
    ["Close"        kill-this-buffer      :enable (kill-this-buffer-enabled-p)]
    ["Close Window" exit-buffer-and-frame :enable (delete-frame-enabled-p)    ]
    ["---" nil :visible t :enable nil]
    ["---" nil :visible t :enable nil]
    ["Exit" save-buffers-kill-terminal :active t]
    ))

(defun tjf:file/new-empty-buffer ()
  "Open a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "new")))
    (switch-to-buffer buf)
    (funcall 'text-mode)
    (setq buffer-offer-save t)))

;; why??? how is this different than `saveas'?

(defun tjf:file/rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' not associated with a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;;
(message "Loading tjf-file...done")
(provide 'tjf-file)

;;; tjf-file.el ends here
