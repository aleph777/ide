;;; tjf-view.el --- View menu defintion and associated functions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2021 Tom Fontaine

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

;; Revision: 02-Mar-2016 Added ‘query-frame-font’ to ‘tjf-view-menu’
;;           11-Aug-2016 Changed ‘usr-view-clipboard’ to ‘tjf-view-clipboard’
;;           25-Aug-2016 Added ‘narrow-or-widen-dwim’
;;           13-Sep-2016 Removed original ‘Hide/Show’ and ‘Folding’ menus
;;                       Added new ‘Fold’ menu
;;           15-Sep-2016 Added ‘which-function-mode’
;;           16-Dec-2016 Added ‘linum-relative-mode’ and supporting functions
;;           12-Jan-2017 Changed ‘is-linum?’ to buffer local
;;                       Fixed toggle functions
;;           02-Apr-2018 Changed line-number functions to use ‘display-line-numbers’
;;           17-Apr-2018 Added ‘diminish-minor-modes' and added to ‘tjf-view-menu’
;;                       Fixed byte compile warnings
;;           31-May-2018 Added ‘get-os-version’ and ‘get-desktop’
;;                       Updated ‘display-properties’
;;           20-Jun-2018 Fixed ‘display-line-numbers-set’ with ‘bound-and-true-p’ check
;;                       of ‘display-line-numbers-mode’
;;           02-Jul-2018 Changed menu text to eliminate reference to frame
;;           06-Jul-2018 Added ‘list-faces-display’
;;                       Moved randon background colors to submenu
;;           21-Oct-2020 Fixed ‘is-line-numbers-absolute?' and ‘is-line-numbers-relative?'
;;           22-Jan-2021 Added ‘what-cursor-position' to menu
;;           03-Feb-2021 ‘tjf’ overhaul
;;           08-May-2021 Fixed reset frame size menu entry
;;

;;; Code:

(message "Loading tjf-view...")
(require 'tjf-clipboard)
(require 'tjf-color)
(require 'tjf-flags)
(require 'tjf-frame)

(eval-when-compile
  (require 'display-line-numbers)
  (require 'face-remap)
  (require 'org)
  (require 'org-src)
  (require 'tjf-macro))

;;
(defvar tjf:view/which-function-mode)
(setq   tjf:view/which-function-mode 'off)
(make-variable-buffer-local 'tjf:view/which-function-mode)

(defun tjf:view/change-local-font ()
  "Change the currently displayed font."
  (interactive)
  (let ((font (x-select-font)))
    (buffer-face-mode-invoke
     (if (fontp font 'font-spec)
         (list :font font)
       (font-face-attributes font))
     t (called-interactively-p 'interactive))))

(defun tjf:view/features ()
  "Display ‘features’ in a Help buffer."
  (interactive)
  (with-output-to-temp-buffer "Features"
    (mapc #'print (sort features #'string<))))

(defun tjf:view/get-line-numbers ()
  "Get line number setting from `display-line-numbers'."
  (cond ((not (boundp display-line-numbers)) 'off)
        ((eq display-line-numbers nil)       'off)
        ((eq display-line-numbers t)         'absolute)
        ((eq display-line-numbers 'absolute) 'absolute)
        ((eq display-line-numbers 'relative) 'relative)
        ((eq display-line-numbers 'visual)   'visual)
        (t                                   'fucked)))

(defun tjf:view/get-desktop ()
  "Call ‘cinnamon’ or ‘gnome-shell’ to retrieve GNOME version."
  (let ((cin (shell-command-to-string "which cinnamon")))
    (unless (eq cin "")
        (replace-regexp-in-string "[\t\n\r]+" "" (shell-command-to-string "cinnamon --version"))
      (replace-regexp-in-string "[\t\n\r]+" "" (shell-command-to-string "gnome-shell --version")))))

(defun tjf:view/diminish ()
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

(defun tjf:view/get-os-version ()
  "Call ‘lsb_release’ to retrieve OS version."
  (replace-regexp-in-string
   "Description:\\|[\t\n\r]+" ""
   (shell-command-to-string "lsb_release -d")))

(defun tjf:view/is-line-numbers-absolute? ()
  "Return boolean t if ‘display-line-numbers’ absolute mode is chosen."
  (eq (tjf:view/get-line-numbers) 'absolute))

(defun tjf:view/is-line-numbers-relative? ()
  "Return boolean t if ‘display-line-numbers’ relative mode is chosen."
  (eq (tjf:view/get-line-numbers) 'relative))

(defun tjf:view/is-which-function-mode? ()
  "Return boolean t if ‘u-which-function-mode’ is on."
  (eq tjf:view/which-function-mode 'on))

(defun tjf:view/line-numbers-absolute-toggle ()
  "Toggle ‘display-line-numbers’ absolute mode state."
  (interactive)
  (if (tjf:view/is-line-numbers-absolute?)
      (tjf:view/set-line-numbers 'off)
    (tjf:view/set-line-numbers 'absolute)))

(defun tjf:view/line-numbers-relative-toggle ()
  "Toggle ‘display-line-numbers’ relative mode state."
  (interactive)
  (if (tjf:view/is-line-numbers-relative?)
      (tjf:view/set-line-numbers 'off)
    (tjf:view/set-line-numbers 'relative)))

(defun tjf:view/narrow-or-widen (p)
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

(defun tjf:view/page ()
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

(defun tjf:view/properties ()
  "Display the Properties of the current buffer."
  (interactive)
  (with-output-to-temp-buffer "*Properties*"
    (princ (format "File Name:   %s\n" (buffer-file-name)))
    (princ (format "Buffer Name: %s\n" (buffer-name)))
    (princ (format "Major Mode:  %s\n" mode-name))
    (princ (format "Buffer Size: %d lines, %d Bytes\n" (count-lines (point-min) (point-max)) (1- (point-max))))
    (princ (format "Point:       %d\n" (point)))
    (let ((page-list (tjf:view/page)))
      (princ (format "Page Number: %d (of %s), Line %s (of %s)\n" (car page-list) (nth 1 page-list) (nth 2 page-list) (nth 3 page-list))))
    (princ (tjf:frame/size   'properties))
    (princ (tjf:frame/font   'properties))
    (princ (tjf:frame/colors 'properties))
    (princ (format "Host ID:     %s\n"        (system-name)))
    (princ (format "User ID:     %s (%s)\n\n" (user-login-name) (user-full-name)))
    (princ (format "System Type: %s\n"        system-type))
    (princ (format "OS/Desktop:  %s/%s\n"     (tjf:view/get-os-version) (tjf:view/get-desktop)))
    (princ (format "Version:     %s\n"        (emacs-version)))))

(defun tjf:view/reset-default-font ()
  "Reset font to default value."
  (interactive)
  (text-scale-mode 0)
  (buffer-face-mode 0))

(defun tjf:view/reset-window-min-height ()
  "Reset the minimum allowable window height."
  (interactive)
  (setq window-min-height 4))

(defun tjf:view/reset-text-size ()
  "Reset text size to default value."
  (interactive)
  (text-scale-mode 0))

;; If called from Lisp, toggle the mode if ARG is toggle.
;;
;; Enable the mode if ARG is nil, omitted, or is a positive number.
;;
;; Disable the mode if ARG is a negative number.
;;
;; To change the type while the mode is on, set `display-line-numbers'
;; directly.

(defun tjf:view/set-line-numbers (mode)
  "Set line numbers display according to MODE."
  (interactive)
  (cond ((eq mode 'off)
         (display-line-numbers-mode -1))
        ((and (eq mode 'absolute) (eq (tjf:view/get-line-numbers) 'relative))
         (setq display-line-numbers t))
        ((and (eq mode 'relative) (eq (tjf:view/get-line-numbers) 'absolute))
         (setq display-line-numbers 'relative))
        ((eq mode 'absolute)
         (setq display-line-numbers t)
         (display-line-numbers-mode 'toggle))
        ((eq mode 'relative)
         (setq display-line-numbers 'relative)
         (display-line-numbers-mode 'toggle))
        (t (display-line-numbers-mode 'toggle))))

(defun tjf:view/which-function-mode ()
  "Toggle function ‘which-function-mode’ and keep local setting for modeline."
  (interactive)
  (setq tjf:view/which-function-mode (if (eq tjf:view/which-function-mode 'off) 'on 'off))
  (which-function-mode))

(defvar tjf:view/menu
  '("View"
    ["Absolute Line Numbers" tjf:view/line-numbers-absolute-toggle :style toggle :selected (tjf:view/is-line-numbers-absolute?)]
    ["Relative Line Numbers" tjf:view/line-numbers-relative-toggle :style toggle :selected (tjf:view/is-line-numbers-relative?)]
    ["Which Function"        tjf:view/which-function-mode          :style toggle :selected (tjf:view/is-which-function-mode?)]
    ["Whitespace"            whitespace-mode                       :style toggle :selected whitespace-mode]
    ["Line Wrap"             visual-line-mode                      :style toggle :selected word-wrap]
    "---"
    ["Local Clipboard" tjf:clipboard/view :enable (get-buffer tjf:clipboard/name)]
    "---"
    ["Fullscreen" toggle-frame-fullscreen :style toggle :selected (tjf:flags/is-fullscreen?)                                        :keys "Alt-F11"]
    ["Maximized"  toggle-frame-maximized  :style toggle :selected (tjf:flags/is-maxmized?)   :enable (tjf:flags/is-not-fullscreen?) :keys "Alt-F10"]
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
      ["Code blocks"             (setq hs-isearch-open 'code)    :style radio :selected (eq hs-isearch-open 'code)]
      ["Comment blocks"          (setq hs-isearch-open 'comment) :style radio :selected (eq hs-isearch-open 'comment)]
      ["Code and Comment blocks" (setq hs-isearch-open t)        :style radio :selected (eq hs-isearch-open t)]
      ["None"                    (setq hs-isearch-open nil)      :style radio :selected (eq hs-isearch-open nil)]))
    "---"
    ["Change Local Font"  change-local-font  t]
    ["Reset default font" reset-default-font t]
    "---"
    ["Increase Text Size" text-scale-increase t]
    ["Decrease Text Size" text-scale-decrease t]
    ["Reset Text Size"    (text-scale-mode 0) t]
    "---"
    ["Properties"  tjf:view/properties]
    "---"
    ["Diminish Minor Modes" diminish-minor-modes]
    "---"
    ["List Faces"            list-faces-display]
    ["Show Face Under Point" (describe-char (point))]
    "---"
    ["Recenter Window"   recenter]
    ["Reposition Window" reposition-window]
    "---"
    ("Window"
     ["Make New Window"       tjf:frame/make-new]
     ["Make Window Invisible" make-frame-invisible :enable (delete-frame-enabled-p)]
     ["Delete Window"         delete-frame         :enable (delete-frame-enabled-p)]
     "---"
     ["Reset Window Size"     tjf:frame/reset-size :enable (tjf:flags/is-not-fullscreen?)]
     ("Background Color"
      ["Set Background Color"  tjf:color/set-background-random]

      ["Set Blue Background Color"    tjf:color/set-background-blue]
      ["Set Cyan Background Color"    tjf:color/set-background-cyan]
      ["Set Gray Background Color"    tjf:color/set-background-gray]
      ["Set Green Background Color"   tjf:color/set-background-green]
      ["Set Magenta Background Color" tjf:color/set-background-magenta]
      ["Set Red Background Color"     tjf:color/set-background-red]
      ["Set Yellow Background Color"  tjf:color/set-background-yellow]
      "---"
      ["Decrease Hue"                 tjf:color/decrease-hue-background]
      ["Increase Hue"                 tjf:color/increase-hue-background]
      ["Decrease Saturation"          tjf:color/desaturate-background]
      ["Increase Saturation"          tjf:color/saturate-background]
      ["Decrease Luminance"           tjf:color/darken-background]
      ["Increase Luminance"           tjf:color/brighten-background])
     "---"
     ["Query Window Font "   (message (tjf:frame/font   'modeline))]
     ["Query Window Size "   (message (tjf:frame/size   'modeline))]
     ["Query Window Colors " (message (tjf:frame/colors 'modeline))])
    ("Pane"
     ["Split Pane Vertically"   split-window-vertically]
     ["Split Pane Horizontally" split-window-horizontally]
     "---"
     ["Delete Other Panes" delete-other-windows :enable (not (one-window-p))]
     ["Delete Pane"        delete-window        :enable (not (one-window-p))]
     "---"
     ["Balance Panes" balance-windows :enable (not (one-window-p))]
     "---"
     ["Reset Minimum Height" reset-window-min-height :enable (not (= window-min-height 4))])
    ))
;;
(message "Loading tjf-view...done")
(provide 'tjf-view)

;;; tjf-view.el ends here
