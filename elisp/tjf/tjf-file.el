;;; tjf-file.el --- File menu and associated local functions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2024 Tom Fontaine

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
;;           24-Sep-2022 Added ‘Rename’ to ‘tjf:edit/menu’
;;

;;; Code:

(message "Loading tjf-file...")
(require 'tjf-frame)
(require 'tjf-flags)

;;; overload
(defun dnd-open-local-file (uri action)
  "Open a local file.
The file is opened in the current window, or a new window if
`dnd-open-file-other-window' is set.  URI is the url for the file,
and must have the format file:file-name or file:///file-name.
The last / in file:/// is part of the file name.  If the system
natively supports unc file names, then remote urls of the form
file://server-name/file-name will also be handled by this function.
An alternative for systems that do not support unc file names is
`dnd-open-remote-url'. ACTION is ignored."

  (let* ((f (dnd-get-local-file-name uri t)))
    (if (and f (file-readable-p f))
        (progn
          (find-file f)
          ;; (if dnd-open-file-other-window
          ;;     (find-file-other-window f)
          ;;   (find-file f))
          'private)
      (error "Can not read %s" uri))))

;;; overload
(defun dnd-open-remote-url (uri action)
  "Open a remote file with `find-file' and `url-handler-mode'.
Turns `url-handler-mode' on if not on before.  The file is opened in the
current window, or a new window if `dnd-open-file-other-window' is set.
URI is the url for the file.  ACTION is ignored."
  (progn
    (require 'url-handlers)
    (or url-handler-mode (url-handler-mode))
    (find-file-other-frame uri)
    ;; (if dnd-open-file-other-window
    ;;     (find-file-other-window uri)
    ;;   (find-file uri))
    'private))

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

(defvar tjf:file/menu
  '("File"
    ["New" tjf:file/new-empty-buffer :enable (menu-bar-non-minibuffer-window-p) :key-sequence [C-n]]
    "---"
    ["View..."               view-file             :enable (menu-bar-non-minibuffer-window-p)]
    ["Open..."               find-file             :enable (menu-bar-non-minibuffer-window-p)]
    ["Open in New Window..." find-file-other-frame :enable (menu-bar-non-minibuffer-window-p)]
    "---"
    ["Reload" revert-buffer :enable (tjf:flags/enable-revert?)]
    "---"
    ["Toggle Read-Only" read-only-mode :style toggle :selected buffer-read-only]
    "---"
    ["Insert File..."        insert-file   :enable (menu-bar-non-minibuffer-window-p)]
    ["Insert from Window..." insert-buffer :enable (menu-bar-non-minibuffer-window-p)]
    "---"
    ["Save"                  save-buffer  :enable (tjf:flags/enable-save?)  ]
    ["Save As... "           write-file   :enable (tjf:flags/enable-saveas?)]
    ["Save Selection As... " write-region :active mark-active               ]
    "---"
    ["Rename..." tjf:file/rename-this-file-and-buffer :enable (tjf:flags/enable-saveas?)]
    "---"
    ["Close"        kill-this-buffer      :enable (kill-this-buffer-enabled-p)]
    ["Close Window" exit-buffer-and-frame :enable (delete-frame-enabled-p)    ]
    "---"
    "---"
    ["Exit" save-buffers-kill-terminal :active t]
    ))

;;
(message "Loading tjf-file...done")
(provide 'tjf-file)

;;; tjf-file.el ends here
