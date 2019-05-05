;;; u-edit.el --- Edit menu and associated functions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

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
;;

;;; Code:

(message "Loading u-edit...")
(require 'u-flags)
(require 'u-navigate)
(require 'xah)

(eval-when-compile
  (require 'u-macro))

;;
(defvar u-edit-menu
  '("Edit"
    ["Undo" undo :enable (enable-undo?)]
    ["Redo" redo :enable (enable-undo?)]
    "---"
    ["Cut"   kill-region    :enable (enable-modify-region?)]
    ["Copy"  kill-ring-save :enable mark-active]
    ["Paste" yank           :enable (enable-paste?)]
    ["marker1" nil :visible nil]
    ["Select Quoted Text"     ergoemacs-select-text-in-quote :active t]
    ["Select All"             mark-whole-buffer              :active t :key-sequence [C-a]]
    ["Copy Buffer"            copy-buffer                    :active t]
    ["Copy Buffer Name"       copy-buffer-name               :active t]
    ["Copy Buffer File Name"  copy-buffer-file-name          :active t]
    "---"
    ["marker2" nil :visible nil]
    ))

(defvar edit-align-menu
  '("Align"
    ["Align Columns Region"     align-columns    :enable (enable-modify-region?)]
    ["Align Equals"             align-equals     :enable (enable-modify-region?)]
    ["Align Regexp..."          align-regexp     :enable (enable-modify-region?)]
    ["Align Columns Rectangle"  pretty-rectangle :enable (enable-modify-region?)]))

(defvar edit-case-menu
  '("Case"
    ["Capitalize Word or Region"       capitalize-word-or-region      :enable (is-rw?)]
    ["Downcase Word or Region"         downcase-word-or-region        :enable (is-rw?)]
    ["Upcase Word or Region"           upcase-word-or-region          :enable (is-rw?)]
    ["Upcase Initials Word or Region"  upcase-initials-word-or-region :enable (is-rw?)]))

(defvar edit-comment-menu
  '("Comment"
    ["Comment Region"         comment-region      :enable (enable-comment?)]
    ["Uncomment Region"       uncomment-region    :enable (enable-comment?)]
    ["Delete Comments Region" comment-kill-region :enable (enable-comment?)]))

(defvar edit-delete-menu
  '("Delete"
    ["Flush Lines..." flush-lines :enable (is-rw?)]
    ["Keep Lines..."  keep-lines  :enable (is-rw?)]
    "---"
    ("Buffer"
     ["Delete Entire Buffer"          erase-buffer                        :enable (is-rw?)]
     ["Delete to Beginning of Buffer" (delete-region (point-min) (point)) :enable (is-rw?)]
     ["Delete to End of Buffer"       (delete-region (point) (point-max)) :enable (is-rw?)])
    ("Line"
     ["Delete Entire Line"          delete-line      :enable (is-rw?)]
     ["Delete All Text on Line"     delete-line-text :enable (is-rw?)]
     ["Delete to Beginning of Line" delete-to-bol    :enable (is-rw?)]
     ["Delete to End of Line"       delete-to-eol    :enable (is-rw?)])
    ("Word"
     ["Delete Forward Word"  (kill-word 1)          :enable (is-rw?)]
     ["Delete Backward Word" (backward-kill-word 1) :enable (is-rw?)])))

(defvar edit-indent-menu
  '("Indent"
    ["Indent Buffer" (indent-region (point-min) (point-max)) :enable (is-rw?)]
    ["Indent Region" indent-region                           :enable (enable-modify-region?)]))

(defvar edit-justify-menu
  '("Justify"
    ["Left Justify Paragraph"   (justify-paragraph-or-region 'left)   :enable (is-rw?)]
    ["Right Justify Paragraph"  (justify-paragraph-or-region 'right)  :enable (is-rw?)]
    ["Full Justify Paragraph"   (justify-paragraph-or-region 'full)   :enable (is-rw?)]
    ["Center Justify Paragraph" (justify-paragraph-or-region 'center) :enable (is-rw?)]
    "---"
    ["Left Justify Region"   (justify-paragraph-or-region 'left)   :enable (enable-modify-region?)]
    ["Right Justify Region"  (justify-paragraph-or-region 'right)  :enable (enable-modify-region?)]
    ["Full Justify Region"   (justify-paragraph-or-region 'full)   :enable (enable-modify-region?)]
    ["Center Justify Region" (justify-paragraph-or-region 'center) :enable (enable-modify-region?)]
    "---"
    ["Toggle Fill"                toggle-fill-paragraph-or-region :enable (is-rw?)]
    ["Unfill Paragraph or Region" unfill-paragraph-or-region      :enable (is-rw?)]
    "---"
    ["Canonically Space Region" canonically-space-region :enable (enable-space-region?)]
    "---"
    ["Set Fill Column..." set-fill-column :active t]))

(defvar edit-rectangle-menu
  '("Rectangle"
    ["Rectangle Mark Mode" rectangle-mark-mode]
    "---"
    ["Open Rectangle"               open-rectangle              :enable (enable-modify-region?)]
    ["Clear Rectangle"              clear-rectangle             :enable (enable-modify-region?)]
    ["Delete Whitespace Rectangle"  delete-whitespace-rectangle :enable (enable-modify-region?)]
    ["Cut Rectangle"                kill-rectangle              :enable (enable-modify-region?)]
    ["Copy Rectangle"               copy-rectangle-as-kill      :enable mark-active]
    ["Paste Rectangle"              yank-rectangle              :enable (is-rw?)]))

(defvar edit-whitespace-menu
  '("Whitespace"
    ["Cleanse Whitespace"          cleanse-whitespace         :enable (is-rw?)]
    "---"
    ["Trim Excess Whitespace "     delete-trailing-whitespace :enable (is-rw?)]
    ["Delete Forward Whitespace"   delete-forward-whitespace  :enable (is-rw?)]
    ["Delete Backward Whitespace"  delete-backward-whitespace :enable (is-rw?)]
    ["Delete Whitespace"           delete-horizontal-space    :enable (is-rw?)]
    ["Compress Blank Lines"        delete-blank-lines         :enable (is-rw?)]
    "---"
    ["Fix Indentation Whitespace" spaceify-indetation               :enable (is-rw?)]
    ["Tabify Buffer"             (tabify   (point-min) (point-max)) :enable (is-rw?)]
    ["Untabify Buffer"           (untabify (point-min) (point-max)) :enable (is-rw?)]
    ["Tabify Region"             tabify                             :enable (enable-modify-region?)]
    ["Untabify Region"           untabify                           :enable (enable-modify-region?)]))

(defun cleanse-whitespace ()
  "Untabify, then trim excess whitespace and compress all blank lines."
  (interactive "*")
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (xah-clean-whitespace))

(defun delete-to-bol ()
  "Deletes text from the current column back to the beginning of the current line."
  (interactive "*")
  (kill-region (line-beginning-position) (point)))

(defalias 'delete-to-eol 'kill-line)

(defun delete-line ()
  "Cuts the entire contents of the current line."
  (interactive "*")
  (beginning-of-line)
  (kill-line 1))

(defun delete-line-text ()
  "Deletes all the text on the current line."
  (interactive "*")
  (kill-region (line-beginning-position) (line-end-position)))

(defun insert-newline-after ()
  "Insert a newline at the end of the current line."
  (interactive "*")
  (end-of-line)
  (newline 1))

(defun insert-newline-after-and-indent ()
  "Insert a newline at the end of the current line and indent according to the major mode."
  (interactive "*")
  (end-of-line)
  (newline-and-indent))

(defun insert-newline-before ()
  "Insert a new-line at the beginning of the current line and positions the cursor prior to the inserted newline."
  (interactive "*")
  (beginning-of-line)
  (newline)
  (forward-line -1))

(defun insert-newline-before-and-indent ()
  "Insert a new-line at the beginning of the current line and positions the cursor prior to the inserted newline."
  (interactive "*")
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1)
  (indent-relative))

(defun delete-forward-whitespace ()
  "Delete whitespace from point to non-whitespace."
  (interactive "*")
  (if (looking-at "[ \t]+")
      (delete-region (match-beginning 0) (match-end 0))))

(defun delete-backward-whitespace ()
  "Delete whitespace from just prior to point to non-whitespace."
  (interactive "*")
  (if (looking-at "[ \t]")
      (let ((point-sav (point)))
        (skip-chars-backward " \t")
        (delete-region (point) point-sav))))

(defun comment-kill-region (start end)
  "Kill comments within the lines in from START to END."
  (interactive "*r")
  (goto-char start)
  (comment-kill (count-lines start end)))

(defun capitalize-word-or-region (beg end)
  "Convert the word at current point or the selected region to first caps."
  (interactive "*r")
  (with-word-or-region (beg end)
    (capitalize-region beg end)))

(defun downcase-word-or-region (beg end)
  "Convert the word at current point or the selected region to lowercase."
  (interactive "*r")
  (with-word-or-region (beg end)
    (downcase-region beg end)))

(defun upcase-word-or-region (beg end)
  "Convert the word at current point or the selected region to uppercase."
  (interactive "*r")
  (with-word-or-region (beg end)
     (upcase-region beg end)))

(defun upcase-initials-word-or-region (beg end)
  "Convert the word at current point or the selected region to uppercase initials."
  (interactive "*r")
  (with-word-or-region (beg end)
    (upcase-initials-region beg end)))

(defun join-line-1 ()
  "Join this line to next and fix up whitespace at join."
  (interactive "*")
  (join-line 1))

(defun align-columns (beg end)
  "Align columns in lines within the specified region (BEG to END)."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (setq beg (line-beginning-position))
    (goto-char end)
    (if (not (or (= (point) (line-beginning-position))
                 (= (point) (line-end-position))))
        (setq end (1+ (line-end-position)))))
  (call-process-region beg end (concat user-dir-bin "align-columns-1") t t t))

(defun align-equals (beg end)
  "Aligns equal signs in lines within region (BEG to END)."
  (interactive "r")
  (let ((beg-line (line-number-at-pos beg))
        (end-line (line-number-at-pos end)))
    (save-excursion
      (indent-region beg end nil)
      (u-goto-line beg-line)
      (setq beg (point))
      (u-goto-line end-line)
      (setq end (1+ (line-end-position))))
    (call-process-region beg end (concat user-dir-bin "align-equals") t t t)))

(defun copy-buffer ()
  "Make a copy of the current buffer."
  (interactive)
  (let* ((copy-bufname (concat "copy of " (buffer-name)))
         (copy-buffer (get-buffer-create copy-bufname)))
    (copy-to-buffer copy-buffer (point-min) (point-max))))

(defun copy-buffer-name ()
  "Make a copy of the current ‘buffer-name’."
  (interactive)
  (let ((name (buffer-name)))
    (kill-new name)
    (message name)))

(defun copy-buffer-file-name ()
  "Make a copy of the current buffer filename."
  (interactive)
  (let ((name (buffer-file-name)))
    (kill-new name)
    (message name)))

(defun fill-paragraph-or-region ()
  "Fill the current paragraph or selected region."
  (interactive "*")
  (with-paragraph-or-region (beg end)
    (fill-region beg end)))

(defun justify-paragraph-or-region (justify)
  "JUSTIFY the current paragraph or selected region."
  (interactive "*")
  (with-paragraph-or-region (beg end)
    (fill-region beg end justify)))
  
(defun spaceify-indetation ()
  "Change all indentation to spaces for buffer or region."
  (interactive "*")
  (with-buffer-or-region (beg end)
    (save-excursion
      (save-restriction
        (goto-char end)
        (while (re-search-backward "^[ 	]+" beg t)
          (replace-match " " nil nil)
          (backward-char))))
    (indent-region beg end)))

(defun toggle-fill-paragraph-or-region ()
  "Toggle fill or unfill for current paragraph or region."
  (interactive "*")
  (let (currentStateIsCompact (bigFillColumnVal most-positive-fixnum) (deactivate-mark nil))
    (save-excursion
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil)))
      (if currentStateIsCompact
          (fill-paragraph-or-region)
        (let ((fill-column bigFillColumnVal))
          (fill-paragraph-or-region)))
      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)))))

(defun unfill-paragraph-or-region ()
  "Join the current paragraph or region into a single line."
  (interactive "*")
  (let ((fill-column (point-max)))
    (fill-paragraph-or-region)))

(defun insert-chs ()
  "Insert opening \"cut here start\" snippet."
  (interactive "*")
  (insert "--8<---------------cut here---------------start------------->8---\n"))

(defun insert-che ()
  "Insert closing \"cut here end\" snippet."
  (interactive "*")
  (insert "--8<---------------cut here---------------end--------------->8---\n"))

;;
(message "Loading u-edit...done")
(provide 'u-edit)

;;; u-edit.el ends here
