;;; tjf-edit.el --- Edit menu and associated functions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 1999-2021 Tom Fontaine

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

;; Revision: 22-Jun-2000 Fixed regexp bug in usr-delete-forward-space
;;                       Added (if (looking-at "[ \t]") to ‘usr-delete-backward-space’
;;           07-Mar-2001 Added (require 'line-position)
;;           09-Mar-2001 Added defalias ‘usr-kill-line’
;;           13-Mar-2001 Added ‘usr-copy-rectangle’
;;           20-Nov-2001 Changed ‘usr-trim-buffer’ to use ‘delete-trailing-whitespace’
;;           04-Dec-2001 Added ‘usr-join-paragraph’
;;           26-Dec-2001 Added ‘usr-join-paragraphs’
;;           12-Jan-2005 Added ‘usr-move-line-up’
;;           22-Jun-2006 Changed ‘usr-join-paragraphs’ to use ‘call-process-region’
;;           23-Jun-2006 Added ‘usr-align-columns’ & ‘usr-align-equals’
;;           14-May-2008 Changed ‘usr-align-columns’, ‘usr-align-equals’, and ‘usr-join-paragraph’ to use ‘user-bin’
;;           02-Feb-2010 Added exit message
;;           13-Jun-2014 Removed byte-compile-dynamic
;;           24-Mar-2015 Fixed ‘usr-move-line-up’ after Emacs 24.4 changes
;;           25-Mar-2015 Added ‘usr-capitalize-word’, ‘usr-downcase-word’, and ‘usr-upcase-word’
;;           04-May-2015 Added ‘usr-move-line-down’
;;           06-May-2015 Moved ‘saved-point’ to ‘usr-misc’
;;           05-Jan-2016 Removed ‘usr-d2u’ and ‘usr-u2d’
;;                       Added ‘usr-set-unix-file’, ‘usr-set-dos-file’, and ‘usr-set-mac-file’
;;                       Changed ‘usr-align-columns’ to use ‘align-columns-1’
;;           26-Feb-2016 Renamed to ‘u-edit’
;;                       Changed ‘usr-kill-bol’ to use ‘line-beginning-position’
;;           02-Mar-2016 Added ‘capitalize-word-or-region’, ‘downcase-word-or-region’, ‘upcase-word-or-region’, and
;;                       ‘upcase-initials-word-or-region’
;;                       Updated ‘edit-case-menu’
;;                       Added ‘xah-toggle-letter-case’
;;                       Changed "(if mark-active" to "(if (‘use-region-p’"
;;           03-Mar-2016 Added ‘fill-paragraph-or-region’, ‘justify-paragraph-or-region’, ‘toggle-fill-paragraph-or-region’
;;                       Updated ‘edit-justify-menu’
;;                       Removed ‘join-paragraphs’
;;           18-Mar-2016 Changed ‘*-word-or-region’ to not deactivate region
;;           12-Oct-2016 Added ‘rectangle-mark-mode’ to edit-rectangle-menu
;;                       Removed ‘comment-current-line’
;;           16-Jan-2017 Added ‘insert-chs’ and ‘insert-che’
;;           17-Jan-2017 Removed ‘*-whole-word’
;;           22-Mar-2017 Added ‘cleanse-whitespace’
;;           13-Jun-2018 Added ‘require’ for ‘u-navigate’
;;                       Fixed ‘*-word-or-region’ definitions
;;           24-Jun-2019 Added ‘toggle-char-case-at-point’
;;           10-Jul-2019 Added ‘u/transpose-lines’
;;           03-Feb-2021 ‘tjf’ overhaul
;;           11-Mar-2021 Removed ‘tjf:edit/delete-forward-whitespace’ and ‘tjf:edit/delete-backward-whitespace’
;;                       Updated ‘tjf:edit/capitalize’, ‘tjf:edit/downcase’, and ‘tjf:edit/upcase’
;;           07-Apr-2021 Added ‘tjf:edit/fill-skeleton’
;;           28-Apr-2022 Changed ‘tjf:edit/cleanse-whitespace’ to use ‘tjf:flags/using-tabs’
;;           02-May-2022 Restored ‘tjf:edit/copy-buffer’
;;           29-Aug-2022 Fixed ‘tjf:edit/menu-justify’
;;

;;; Code:

(message "Loading tjf-edit...")
(require 'tjf-flags)
(require 'tjf-navigate)
(require 'xah)

(eval-when-compile
  (require 'tjf-macro))

;;
(defun tjf:edit/align-columns (beg end)
  "Align columns in lines within the specified region (BEG to END)."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (setq beg (line-beginning-position))
    (goto-char end)
    (if (not (or (= (point) (line-beginning-position))
                 (= (point) (line-end-position))))
        (setq end (1+ (line-end-position)))))
  (call-process-region beg end (concat tjf:user/dir-bin "align-columns-1") t t t))

(defun tjf:edit/align-equals (beg end)
  "Aligns equal signs in lines within region (BEG to END)."
  (interactive "r")
  (let ((beg-line (line-number-at-pos beg))
        (end-line (line-number-at-pos end)))
    (save-excursion
      (indent-region beg end nil)
      (tjf:navigate/goto-line beg-line)
      (setq beg (point))
      (tjf:navigate/goto-line end-line)
      (setq end (1+ (line-end-position))))
    (call-process-region beg end (concat tjf:user/dir-bin "align-equals") t t t)))

(defun tjf:edit/capitalize ()
  "Convert the word at current point or the selected region to first caps."
  (interactive "*")
  (with-word-or-region (beg end)
                       (capitalize-region beg end (region-noncontiguous-p))
                       (goto-char end)))

(defun tjf:edit/cleanse-whitespace ()
  "Untabify, then trim excess whitespace and compress all blank lines."
  (interactive "*")
  (unless tjf:flags/using-tabs
      (untabify (point-min) (point-max)))
  (delete-trailing-whitespace)
  (xah-clean-whitespace))

(defun tjf:edit/clear-line ()
  "Deletes all the text on the current line."
  (interactive "*")
  (kill-region (line-beginning-position) (line-end-position)))

(defun tjf:edit/comment-kill-region (start end)
  "Kill comments within the lines in from START to END."
  (interactive "*r")
  (goto-char start)
  (comment-kill (count-lines start end)))

(defun tjf:edit/delete-backward ()
  "Delete syntax backward."
  (interactive)
  (let* ((syntax (string (char-syntax (char-before))))
         (begin  (save-excursion
                   (cond
                    ((equal syntax " ") (skip-syntax-backward " "))
                    ((equal syntax "w") (skip-syntax-backward "w"))
                    ((equal syntax "_") (skip-syntax-backward "w_"))
                    ((equal syntax "(") (skip-syntax-backward "()"))
                    ((equal syntax ")") (skip-syntax-backward ")"))
                    ((equal syntax "<") (skip-syntax-backward "<"))
                    ((equal syntax ">") (skip-syntax-backward ">"))
                    ((equal syntax ".") (skip-syntax-backward "."))
                    ((equal syntax "'") (skip-syntax-backward "'"))
                    (t (error "Unknown syntax")))
                   (point))))
    (delete-region begin (point))))

(defun tjf:edit/delete-forward ()
  "Delete syntax forward."
  (interactive)
  (let* ((syntax (string (char-syntax (char-after))))
         (end    (save-excursion
                   (cond
                    ((equal syntax " ") (skip-syntax-forward " "))
                    ((equal syntax "w") (skip-syntax-forward "w"))
                    ((equal syntax "_") (skip-syntax-forward "w_") )
                    ((equal syntax "(") (skip-syntax-forward "()"))
                    ((equal syntax ")") (skip-syntax-forward ")"))
                    ((equal syntax "<") (skip-syntax-forward "<"))
                    ((equal syntax ">") (skip-syntax-forward ">"))
                    ((equal syntax ".") (skip-syntax-forward "."))
                    ((equal syntax "'") (skip-syntax-forward "'"))
                    (t (error "Unknown syntax")))
                   (point))))
    (delete-region (point) end)))

(defun tjf:edit/delete-line ()
  "Cuts the entire contents of the current line."
  (interactive "*")
  (beginning-of-line)
  (kill-line 1))

(defun tjf:edit/fill-skeleton (match-text replace-text)
  "Use MATCH-TEXT to fill REPLACE-TEXT into skeleton."
  (save-excursion
    (while (search-forward match-text (point-max) t)
        (replace-match replace-text t))))

(defun tjf:edit/copy-buffer-file-name ()
  "Make a copy of the current buffer filename."
  (interactive)
  (let ((name (buffer-file-name)))
    (kill-new name)
    (message name)))

(defun tjf:edit/copy-buffer ()
  "Make a copy of the current buffer."
  (interactive)
  (let* ((copy-bufname (concat "copy of " (buffer-name)))
         (copy-buffer (get-buffer-create copy-bufname)))
    (copy-to-buffer copy-buffer (point-min) (point-max))))

(defun tjf:edit/copy-buffer-name ()
  "Make a copy of the current ‘buffer-name’."
  (interactive)
  (let ((name (buffer-name)))
    (kill-new name)
    (message name)))

(defun tjf:edit/delete-line-text ()
  "Deletes all the text on the current line."
  (interactive "*")
  (kill-region (line-beginning-position) (line-end-position)))

(defun tjf:edit/delete-to-beginning ()
  "Delete to the beginning of the buffer."
  (interactive "*")
  (delete-region (point-min) (point)))

(defun tjf:edit/delete-to-bol ()
  "Deletes text from the current column back to the beginning of the current line."
  (interactive "*")
  (kill-region (line-beginning-position) (point)))

(defun tjf:edit/delete-to-end ()
  "Delete to the end of the buffer."
  (interactive "*")
  (delete-region (point) (point-max)))

(defalias 'tjf:edit/delete-to-eol 'kill-line)

(defun tjf:edit/delete-word ()
  "Delete the entire word at point."
  (interactive "*")
  (if (looking-at-word-or-symbol)
      (delete-region (word-beginning-position) (word-end-position))))

;; (defun tjf:edit/delete-whitespace-backward ()
;;   "Delete whitespace from just prior to point to non-whitespace."
;;   (interactive "*")
;;   (if (looking-at "[[:space:]]")
;;       (let ((point-sav (point)))
;;         (skip-chars-backward "[:space:]")
;;         (delete-region (point) point-sav))
;;     (if (eolp)
;;         (delete-trailing-whitespace))))

;; (defun tjf:edit/delete-whitespace-forward ()
;;   "Delete whitespace from point to non-whitespace."
;;   (interactive "*")
;;   (if (looking-at "[[:space:]]")
;;       (delete-region (match-beginning 0) (match-end 0))))

(defun tjf:edit/downcase ()
  "Convert the word at current point or the selected region to lowercase."
  (interactive "*")
  (with-word-or-region (beg end)
                       (downcase-region beg end (region-noncontiguous-p))
                       (goto-char end)))

(defun tjf:edit/fill ()
  "Fill the current paragraph or selected region."
  (interactive "*")
  (with-paragraph-or-region (beg end)
    (fill-region beg end)))

(defun tjf:edit/indent ()
  "Indent the entire buffer or the region."
  (interactive "*")
  (with-buffer-or-region (beg end)
                         (indent-region beg end)))

(defun tjf:edit/insert-chs ()
  "Insert opening \"cut here start\" snippet."
  (interactive "*")
  (insert "--8<---------------cut here---------------start------------->8---\n"))

(defun tjf:edit/insert-che ()
  "Insert closing \"cut here end\" snippet."
  (interactive "*")
  (insert "--8<---------------cut here---------------end--------------->8---\n"))

(defun tjf:edit/insert-newline-after ()
  "Insert a newline at the end of the current line."
  (interactive "*")
  (end-of-line)
  (newline 1))

(defun tjf:edit/insert-newline-after-and-indent ()
  "Insert a newline at the end of the current line and indent."
  (interactive "*")
  (end-of-line)
  (newline-and-indent))

(defun tjf:edit/insert-newline-before ()
  "Insert a newline at the beginning of the current line and positions
the cursor prior to the inserted newline."
  (interactive "*")
  (beginning-of-line)
  (newline)
  (forward-line -1))

(defun tjf:edit/insert-newline-before-and-indent ()
  "Insert a newline at the beginning of the current line and positions
the cursor prior to the inserted newline."
  (interactive "*")
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1)
  (indent-relative))

(defun tjf:edit/join-line-1 ()
  "Join this line to next and fix up whitespace at join."
  (interactive "*")
  (join-line 1))

(defun tjf:edit/justify (justify)
  "JUSTIFY the current paragraph or selected region."
  (interactive "*")
  (with-paragraph-or-region (beg end)
    (fill-region beg end justify)))

(defun tjf:edit/spaceify-indetation ()
  "Change all indentation to spaces for buffer or region."
  (interactive "*")
  (with-buffer-or-region (beg end)
    (save-excursion
      (save-restriction
        (goto-char end)
        (while (re-search-backward "^[  ]+" beg t)
          (replace-match " " nil nil)
          (backward-char))))
    (indent-region beg end)))

(defun tjf:edit/tabify ()
  "Tabify the entire buffer or region."
  (interactive "*")
  (with-buffer-or-region (beg end)
                         (tabify beg end)))

(defun tjf:edit/toggle-char-case-at-point ()
  "Toggle the case of the character at point."
  (interactive)
  (let* ((case-fold-search nil)
         (char-at-point    (char-after))
         (tmp-string       (char-to-string char-at-point)))
      (cond
       ((string-match "^[[:lower:]]" tmp-string)
        (upcase-region (point) (+ 1 (point))))
       ((string-match "^[[:upper:]]" tmp-string)
        (downcase-region (point) (+ 1(point)))))))

(defun tjf:edit/toggle-fill ()
  "Toggle fill or unfill for current paragraph or region."
  (interactive "*")
  (let (currentStateIsCompact (bigFillColumnVal most-positive-fixnum) (deactivate-mark nil))
    (save-excursion
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil)))
      (if currentStateIsCompact
          (tjf:edit/fill)
        (let ((fill-column bigFillColumnVal))
          (tjf:edit/fill)))
      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)))))

(defun tjf:edit/unfill ()
  "Join the current paragraph or region into a single line."
  (interactive "*")
  (let ((fill-column (point-max)))
    (tjf:edit/fill)))

(defun tjf:edit/untabify ()
  "Untabify the ENTIRE buffer or region."
  (interactive "*")
  (with-buffer-or-region (beg end)
                         (untabify beg end)))

(defun tjf:edit/upcase ()
  "Convert the word at current point or the selected region to uppercase."
  (interactive "*")
  (with-word-or-region (beg end)
                       (upcase-region beg end (region-noncontiguous-p))
                       (goto-char end)))

(defvar tjf:edit/menu
  '("Edit"
    ;; ["Undo" undo :enable (tjf:flags/enable-undo?)] -- from undo-tree.el
    ;; ["Redo" redo :enable (tjf:flags/enable-redo?)]
    "---"
    ["Cut"   kill-region    :enable (tjf:flags/enable-modify-region?)]
    ["Copy"  kill-ring-save :enable mark-active]
    ["Paste" yank           :enable (tjf:flags/enable-paste?)]
    ["marker1" nil :visible nil]
    ["Select Quoted Text"     ergoemacs-select-text-in-quote]
    ["Select All"             mark-whole-buffer             :key-sequence [C-a]]
    ["Copy Buffer"            tjf:edit/copy-buffer          ]
    ["Copy Buffer Name"       tjf:edit/copy-buffer-name     ]
    ["Copy Buffer File Name"  tjf:edit/copy-buffer-file-name]
    "---"
    ["marker2" nil :visible nil]
    ))

(defvar tjf:edit/menu-align
  '("Align"
    ["Align Columns Rectangle" pretty-rectangle       :enable (tjf:flags/enable-modify-region?)]
    ["Align Columns Region"    tjf:edit/align-columns :enable (tjf:flags/enable-modify-region?)]
    ["Align Equals"            tjf:edit/align-equals  :enable (tjf:flags/enable-modify-region?)]
    ["Align Regexp..."         align-regexp           :enable (tjf:flags/enable-modify-region?)]))

(defvar tjf:edit/menu-case
  '("Case"
    ["Capitalize Word or Region" tjf:edit/capitalize    :enable (tjf:flags/enable-write?)]
    ["Downcase Word or Region"   tjf:edit/downcase-word :enable (tjf:flags/enable-write?)]
    ["Upcase Word or Region"     tjf:edit/upcase        :enable (tjf:flags/enable-write?)]))

(defvar tjf:edit/menu-comment
  '("Comment"
    ["Comment Region"         comment-region      :enable (tjf:flags/enable-comment?)]
    ["Delete Comments Region" comment-kill-region :enable (tjf:flags/enable-comment?)]
    ["Uncomment Region"       uncomment-region    :enable (tjf:flags/enable-comment?)]))

(defvar tjf:edit/menu-delete
  '("Delete"
    ["Flush Lines..." flush-lines :enable (tjf:flags/enable-write?)]
    ["Keep Lines..."  keep-lines  :enable (tjf:flags/enable-write?)]
    "---"
    ("Buffer"
     ["Delete Entire Buffer"          erase-buffer                 :enable (tjf:flags/enable-write?)]
     ["Delete to Beginning of Buffer" tjf:edit/delete-to-beginning :enable (tjf:flags/enable-write?)]
     ["Delete to End of Buffer"       tjf:edit/delete-to-end       :enable (tjf:flags/enable-write?)])
    ("Line"
     ["Delete All Text on Line"     tjf:edit/clear-line    :enable (tjf:flags/enable-write?)]
     ["Delete Entire Line"          tjf:edit/delete-line   :enable (tjf:flags/enable-write?)]
     ["Delete to Beginning of Line" tjf:edit/delete-to-bol :enable (tjf:flags/enable-write?)]
     ["Delete to End of Line"       tjf:edit/delete-to-eol :enable (tjf:flags/enable-write?)])
    ("Word"
     ["Delete Backward Word" backward-kill-word :enable (tjf:flags/enable-write?)])
     ["Delete Forward Word"  kill-word          :enable (tjf:flags/enable-write?)]))

(defvar tjf:edit/menu-indent
  '("Indent"
    ["Indent Buffer" tjf:edit/indent :enable (tjf:flags/enable-buffer-operations?)]
    ["Indent Region" tjf:edit/indent :enable (tjf:flags/enable-modify-region?)]))

(defvar tjf:edit/menu-justify
  '("Justify"
    ["Canonically Space Region" canonically-space-region :enable (tjf:flags/enable-space-region?)]
    "---"
    ["Center Justify Paragraph" (tjf:edit/justify 'center) :enable (tjf:flags/enable-write?)]
    ["Full Justify Paragraph"   (tjf:edit/justify 'full)   :enable (tjf:flags/enable-write?)]
    ["Left Justify Paragraph"   (tjf:edit/justify 'left)   :enable (tjf:flags/enable-write?)]
    ["Right Justify Paragraph"  (tjf:edit/justify 'right)  :enable (tjf:flags/enable-write?)]
    "---"
    ["Center Justify Region" (tjf:edit/justify 'center) :enable (tjf:flags/enable-modify-region?)]
    ["Full Justify Region"   (tjf:edit/justify 'full)   :enable (tjf:flags/enable-modify-region?)]
    ["Left Justify Region"   (tjf:edit/justify 'left)   :enable (tjf:flags/enable-modify-region?)]
    ["Right Justify Region"  (tjf:edit/justify 'right)  :enable (tjf:flags/enable-modify-region?)]
    "---"
    ["Set Fill Column..." set-fill-column]
    "---"
    ["Toggle Fill"                tjf:edit/toggle-fill :enable (tjf:flags/enable-write?)]
    ["Unfill Paragraph or Region" tjf:edit/unfill      :enable (tjf:flags/enable-write?)]))

(defvar tjf:edit/menu-rectangle
  '("Rectangle"
    ["Rectangle Mark Mode" rectangle-mark-mode]
    "---"
    ["Clear Rectangle"              clear-rectangle             :enable (tjf:flags/enable-modify-region?)]
    ["Delete Whitespace Rectangle"  delete-whitespace-rectangle :enable (tjf:flags/enable-modify-region?)]
    ["Cut Rectangle"                kill-rectangle              :enable (tjf:flags/enable-modify-region?)]
    ["Copy Rectangle"               copy-rectangle-as-kill      :enable mark-active]
    ["Open Rectangle"               open-rectangle              :enable (tjf:flags/enable-modify-region?)]
    ["Paste Rectangle"              yank-rectangle              :enable (tjf:flags/enable-write?)]))

(defvar tjf:edit/menu-whitespace
  '("Whitespace"
    ["Cleanse Whitespace"          tjf:edit/cleanse-whitespace :enable (tjf:flags/enable-write?)]
    "---"
    ["Compress Blank Lines"        delete-blank-lines                  :enable (tjf:flags/enable-write?)]
    ["Delete Backward Whitespace"  tjf:edit/delete-whitespace-backward :enable (tjf:flags/enable-write?)]
    ["Delete Forward Whitespace"   tjf:edit/delete-whitespace-forward  :enable (tjf:flags/enable-write?)]
    ["Delete Whitespace"           delete-horizontal-space             :enable (tjf:flags/enable-write?)]
    ["Trim Excess Whitespace "     delete-trailing-whitespace          :enable (tjf:flags/enable-write?)]
    "---"
    ["Fix Indentation Whitespace" tjf:edit/spaceify-indetation :enable (tjf:flags/enable-write?)]
    ["Tabify Buffer"              tjf:edit/tabify              :enable (tjf:flags/enable-buffer-operations?)]
    ["Tabify Region"              tjf:edit/tabify              :enable (tjf:flags/enable-modify-region?)]
    ["Untabify Buffer"            tjf:edit/untabify            :enable (tjf:flags/enable-buffer-operations?)]
    ["Untabify Region"            tjf:edit/untabify            :enable (tjf:flags/enable-modify-region?)]))

;;
(message "Loading tjf-edit...done")
(provide 'tjf-edit)

;;; tjf-edit.el ends here
