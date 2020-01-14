;;; u-view.el --- View menu defintion and associated functions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-
 
;;         Copyright © 2016-2019 Tom Fontaine

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

;; Revision: 02-Mar-2016 Added ‘query-frame-font’ to ‘u-view-menu’
;;           11-Aug-2016 Changed ‘usr-view-clipboard’ to ‘u-view-clipboard’
;;           25-Aug-2016 Added ‘narrow-or-widen-dwim’
;;           13-Sep-2016 Removed original ‘Hide/Show’ and ‘Folding’ menus
;;                       Added new ‘Fold’ menu
;;           15-Sep-2016 Added ‘which-function-mode’
;;           16-Dec-2016 Added ‘linum-relative-mode’ and supporting functions
;;           12-Jan-2017 Changed ‘is-linum?’ to buffer local
;;                       Fixed toggle functions
;;           02-Apr-2018 Changed line-number functions to use ‘display-line-numbers’
;;           17-Apr-2018 Added ‘diminish-minor-modes' and added to ‘u-view-menu’
;;                       Fixed byte compile warnings
;;           31-May-2018 Added ‘get-os-version’ and ‘get-desktop’
;;                       Updated ‘display-properties’
;;           20-Jun-2018 Fixed ‘display-line-numbers-set’ with ‘bound-and-true-p’ check
;;                       of ‘display-line-numbers-mode’
;;           02-Jul-2018 Changed menu text to eliminate reference to frame
;;           06-Jul-2018 Added ‘list-faces-display’
;;                       Moved randon background colors to submenu
;;

;;; Code:

(message "Loading u-view...")

(require 'u-flags)
(require 'clipboard)
(require 'u-frame)
(require 'colors)

(eval-when-compile
  (require 'display-line-numbers)
  (require 'face-remap)
  (require 'org)
  (require 'org-src)
  (require 'u-macro))
;;
(defvar line-numbers 'off)
(make-variable-buffer-local 'line-numbers)

(defvar u-which-function-mode 'off)
(make-variable-buffer-local 'u-which-function-mode)

(defsubst is-line-numbers-absolute? ()
  "Return boolean t if ‘display-line-numbers’ absolute mode is chosen."
  (eq line-numbers 'absolute))

(defsubst is-line-numbers-relative? ()
  "Return boolean t if ‘display-line-numbers’ relative mode is chosen."
  (eq line-numbers 'relative))

(defun u-which-function-mode ()
  "Toggle function ‘which-function-mode’ and keep local setting for modeline."
  (interactive)
  (setq u-which-function-mode (if (eq u-which-function-mode 'off) 'on 'off))
  (which-function-mode))

(defun display-line-numbers-set (mode)
  "Set line numbers display according to MODE."
  (interactive)
  (setq line-numbers mode)
  (if (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode 'toggle))
  (unless (eq mode 'off)
    (progn
      (setq display-line-numbers-type (or (eq line-numbers 'absolute) 'relative)) ;; t or relative
      (display-line-numbers-mode))))

(defun display-line-numbers-absolute-toggle ()
  "Toggle ‘display-line-numbers’ absolute mode state."
  (interactive)
  (if (is-line-numbers-absolute?)
      (display-line-numbers-set 'off)
    (display-line-numbers-set 'absolute)))

(defun display-line-numbers-relative-toggle ()
  "Toggle ‘display-line-numbers’ relative mode state."
  (interactive)
  (if (is-line-numbers-relative?)
      (display-line-numbers-set 'off)
    (display-line-numbers-set 'relative)))

(defvar u-view-menu
  '("View"
    ["Absolute Line Numbers" display-line-numbers-absolute-toggle :style toggle :selected (is-line-numbers-absolute?)]
    ["Relative Line Numbers" display-line-numbers-relative-toggle :style toggle :selected (is-line-numbers-relative?)]
    ["Which Function"        u-which-function-mode                :style toggle :selected which-function-mode]
    ["Whitespace"            whitespace-mode                      :style toggle :selected whitespace-mode]
    ["Line Wrap"             visual-line-mode                     :style toggle :selected word-wrap]
    "---"
    ["Local Clipboard" u-view-clipboard :enable (get-buffer u-clipboard-buffer-name)]
    "---"
    ["Fullscreen" toggle-frame-fullscreen :style toggle :selected (is-fullscreen?)                              :keys "Alt-F11"]
    ["Maximized"  toggle-frame-maximized  :style toggle :selected (is-maxmized?)   :enable (is-not-fullscreen?) :keys "Alt-F10"]
    "---"
    ("Hide/Show"
     ["Hide/Show Enabled"  hs-minor-mode :style toggle :selected hs-minor-mode]
     "---"
     ["Hide Block"    hs-hide-block    :enable hs-minor-mode :help "Hide the code or comment block at point"]
     ["Show Block"    hs-show-block    :enable hs-minor-mode :help "Show the code or comment block at point"]
     ["Hide All"      hs-hide-all      :enable hs-minor-mode :help "Hide all the blocks in the buffer"]
     ["Show All"      hs-show-all      :enable hs-minor-mode :help "Show all the blocks in the buffer"]
     ["Hide Level"    hs-hide-level    :enable hs-minor-mode :help "Hide all block at levels below the current block"]
     ["Toggle Hiding" hs-toggle-hiding :enable hs-minor-mode :help "Toggle the hiding state of the current block"]
     "----"
     ["Hide comments when hiding all" (setq hs-hide-comments-when-hiding-all (not hs-hide-comments-when-hiding-all)) :style toggle :selected hs-hide-comments-when-hiding-all]
     ("Reveal on isearch"
      ["Code blocks"             (setq hs-isearch-open 'code)    :active t :style radio :selected (eq hs-isearch-open 'code)]
      ["Comment blocks"          (setq hs-isearch-open 'comment) :active t :style radio :selected (eq hs-isearch-open 'comment)]
      ["Code and Comment blocks" (setq hs-isearch-open t)        :active t :style radio :selected (eq hs-isearch-open t)]
      ["None"                    (setq hs-isearch-open nil)      :active t :style radio :selected (eq hs-isearch-open nil)]))
    "---"
    ["Change Local Font"  change-local-font  t]
    ["Reset default font" reset-default-font t]
    "---"
    ["Increase Text Size" text-scale-increase t]
    ["Decrease Text Size" text-scale-decrease t]
    ["Reset Text Size"     (text-scale-mode 0) t]
    "---"
    ["Properties"  display-properties :active t]
    "---"
    ["Diminish Minor Modes" diminish-minor-modes :active t]
    "---"
    ["List Faces" list-faces-display :active t]
    "---"
    ["Recenter Window"   recenter          :active t]
    ["Reposition Window" reposition-window :active t]
    "---"
    ("Window"
     ["Make New Window"       make-new-frame       :active t]
     ["Make Window Invisible" make-frame-invisible :enable (delete-frame-enabled-p)]
     ["Delete Window"         delete-frame         :enable (delete-frame-enabled-p)]
     "---"
     ["Reset Window Size"     reset-frame-size             :enable (is-not-fullscreen?)]
     ("Background Color"
      ["Set Background Color"  set-random-background-color  :active t]

      ["Set Red Background Color"     (set-background-color (color-bg/red))     :active t]
      ["Set Green Background Color"   (set-background-color (color-bg/green))   :active t]
      ["Set Blue Background Color"    (set-background-color (color-bg/blue))    :active t]
      ["Set Cyan Background Color"    (set-background-color (color-bg/cyan))    :active t]
      ["Set Magenta Background Color" (set-background-color (color-bg/magenta)) :active t]
      ["Set Yellow Background Color"  (set-background-color (color-bg/yellow))  :active t]
      ["Set Pink Background Color"    (set-background-color (color-bg/pink))    :active t]
      ["Set Indigo Background Color"  (set-background-color (color-bg/indigo))  :active t]
      ["Set Gray Background Color"    (set-background-color (color-bg/gray))    :active t])
     "---"
     ["Query Window Font "   (message (query-frame-font   'modeline)) :active t]
     ["Query Window Size "   (message (query-frame-size   'modeline)) :active t]
     ["Query Window Colors " (message (query-frame-colors 'modeline)) :active t])
    ("Pane"
     ["Split Pane Vertically"   split-window-vertically   :active t]
     ["Split Pane Horizontally" split-window-horizontally :active t]
     "---"
     ["Delete Other Panes" delete-other-windows :enable (not (one-window-p))]
     ["Delete Pane"        delete-window        :enable (not (one-window-p))]
     "---"
     ["Balance Panes" balance-windows :enable (not (one-window-p))]
     "---"
     ["Reset Minimum Height" reset-window-min-height :enable (not (= window-min-height 4))])
    ))

(defun change-local-font ()
  "Change the currently displayed font."
  (interactive)
  (let ((font (x-select-font)))
    (buffer-face-mode-invoke
     (if (fontp font 'font-spec)
         (list :font font)
       (font-face-attributes font))
     t (called-interactively-p 'interactive))))

(defun reset-text-size ()
  "Reset text size to default value."
  (interactive)
  (text-scale-mode 0))

(defun reset-default-font ()
  "Reset font to default value."
  (interactive)
  (text-scale-mode 0)
  (buffer-face-mode 0))

(defun get-os-version ()
  "Call ‘lsb_release’ to retrieve OS version."
  (replace-regexp-in-string
   "Description:\\|[\t\n\r]+" ""
   (shell-command-to-string "lsb_release -d")))

(defun get-desktop ()
  "Call ‘cinnamon’ or ‘gnome-shell’ to retrieve GNOME version."
  (let ((cin (shell-command-to-string "which cinnamon")))
    (if cin
        (replace-regexp-in-string "[\t\n\r]+" "" (shell-command-to-string "cinnamon --version"))
      (replace-regexp-in-string "[\t\n\r]+" "" (shell-command-to-string "gnome-shell --version")))))

(defun display-properties ()
  "Display the Properties of the current buffer."
  (interactive)
  (with-output-to-temp-buffer "*Properties*"
    (princ (format "File Name:   %s\n" (buffer-file-name)))
    (princ (format "Buffer Name: %s\n" (buffer-name)))
    (princ (format "Major Mode:  %s\n" mode-name))
    (princ (format "Buffer Size: %d lines, %d Bytes\n" (count-lines (point-min) (point-max)) (1- (point-max))))
    (princ (format "Point:       %d\n" (point)))
    (let ((page-list (query-page)))
      (princ (format "Page Number: %d (of %s), Line %s (of %s)\n" (car page-list) (nth 1 page-list) (nth 2 page-list) (nth 3 page-list))))
    (princ (query-frame-size   'properties))
    (princ (query-frame-font   'properties))
    (princ (query-frame-colors 'properties))
    ;; (princ (format "Printer:     %s\n" printer-name))
    (princ (format "Host ID:     %s\n" (system-name)))
    (princ (format "User ID:     %s (%s)\n\n" (user-login-name) (user-full-name)))
    (princ (format "System Type: %s\n" system-type))
    (princ (format "OS/Desktop:  %s/%s\n" (get-os-version) (get-desktop)))
    (princ (format "Version:     %s\n" (emacs-version)))))

(defun reset-window-min-height ()
  "Reset the minimum allowable window height."
  (interactive)
  (setq window-min-height 4))

(defun query-page ()
  "Return a page list: number, total, line number and line total."
  (interactive)
  (let ((count-all 1)
        (count-pgn 1)
        (pline-num nil)
        (page-all  nil)
        (page-beg  nil)
        (page-end  nil)
        (point-sav (point)))
    (save-excursion
      (forward-page)
      (beginning-of-line)
      (or (looking-at page-delimiter)
          (end-of-line))
      (setq page-end (point))
      (backward-page)
      (setq page-beg (point) page-all (count-lines page-beg page-end)))
    (save-restriction
      (widen)
      (save-excursion
        (beginning-of-line)
        (setq point-sav (point))
        (goto-char 1)
        (while (re-search-forward page-delimiter point-sav t)
          (setq count-all (1+ count-all) count-pgn count-all))
        (setq pline-num (1+ (count-lines (point) point-sav)))
        (while (re-search-forward page-delimiter nil t)
          (setq count-all (1+ count-all)))
        (list count-pgn count-all pline-num page-all)))))

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first.  Narrowing to
org-src-block actually calls ‘org-edit-src-code’.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; ‘org-edit-src-code’ is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))

(defun diminish-minor-modes ()
  "Clear minor mode lighters from modeline."
  (interactive)
  (diminish-mode-list #'""
                      'abbrev-mode
                      'anzu-mode
                      'auto-revert-mode
                      'company-mode
                      'eldoc-mode
                      'highlight-operators-mode
                      'hs-minor-mode
                      'modern-c++-font-lock-mode
                      'smartparens-mode
                      'undo-tree-mode
                      'volatile-highlights-mode
                      'whitespace-mode
                      'ycmd-mode))

;;
(message "Loading u-view...done")
(provide 'u-view)

;;; u-view.el ends here
