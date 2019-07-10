;;; u-overload.el --- [insert description here] -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

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

;; Revision: 16-Nov-2001 New for Emacs 21
;;           03-Dec-2001 Added `menu-bar-select-frame'
;;           17-Feb-2005 Added `eval-after-load' for `Man-notify-when-ready'
;;           13-May-2008 Removed `menu-bar-select-frame'
;;           02-Feb-2010 Added exit message
;;           13-Feb-2010 Added `dnd-open-local-file'
;;           23-May-2014 Updated `switch-to-buffer-other-frame' for Emacs 24
;;           24-May-2014 Changed title of Frames sub-menu
;;           04-Feb-2016 Added `neo-buffer--get-nodes'
;;           08-Feb-2016 Changed `dnd-open-local-file to find-file'
;;           24-Feb-2016 Added `cperl-init-faces'
;;           29-Feb-2016 Changed from `usr-' to `u-'
;;           01-Mar-2016 Fixed `switch-to-buffer-other-frame' major-mode issue
;;           05-Jan-2017 Added `tool-bar--image-expression' for png icons
;;           13-Jan-2017 Added `bm-show-extract-bookmarks'
;;           14-Jan-2017 Added `bm-show-display-lines'
;;           13-Jun-2018 Changed to use ‘with-eval-after-load’
;;           08-May-2019 Stopped using ‘handle-delete-frame’
;;           10-Jul-2019 Added ‘undo-tree-update-menu-bar’
;;

;;; Code:

(message "Loading u-overload...")

;;
;; This function is completely re-written
;;
(defun tool-bar--image-expression (icon)
  "Return an expression to evaluate an image spec for ICON."
  (let ((xpm-spec (list :type 'xpm :file (concat icon ".xpm")))
        (png-spec (list :type 'png :file (concat icon ".png"))))
    `(find-image ',(list xpm-spec png-spec))))

(defun switch-to-buffer-other-frame (buffer-or-name &optional norecord)
  "Switch to buffer BUFFER-OR-NAME in another frame.
BUFFER-OR-NAME may be a buffer, a string (a buffer name), or
nil.  Return the buffer switched to.

If called interactively, prompt for the buffer name using the
minibuffer.  The variable `confirm-nonexistent-file-or-buffer'
determines whether to request confirmation before creating a new
buffer.

If BUFFER-OR-NAME is a string and does not identify an existing
buffer, create a new buffer with that name.  If BUFFER-OR-NAME is
nil, switch to the buffer returned by `other-buffer'.

Optional second arg NORECORD non-nil means do not put this
buffer at the front of the list of recently selected ones.

This uses the function `display-buffer' as a subroutine; see its
documentation for additional customization information."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other frame: ")))
  (let* ((major-mode (with-current-buffer buffer-or-name major-mode))
         (pop-up-frames t)
         (pop-up-frame-alist (append (cdr (assq major-mode frame-mode-size-alist))                 ;;; tjf
                                     (list (cons 'background-color (random-background-color))))))  ;;; tjf
    (pop-to-buffer buffer-or-name display-buffer--other-frame-action norecord)))

(defun mouse-tear-off-window (click)
  "Delete the window clicked on, and create a new frame displaying its buffer."
  (interactive "e")
  (mouse-minibuffer-check click)
  (let* ((window (posn-window (event-start click)))
         (buf (window-buffer window))
         (mode-size (assq major-mode frame-mode-size-alist))                      ;;; tjf
         (width     (or (cdr (assq 'width  mode-size)) 80))                       ;;; tjf
         (height    (or (cdr (assq 'height mode-size)) 40))                       ;;; tjf
         (frame (make-frame (list (cons 'background-color (random-background-color))  ;;; tjf
                                  (cons 'width width)                             ;;; tjf
                                  (cons 'height height)))))                       ;;; tjf
    (select-frame frame)
    (switch-to-buffer buf)
    (delete-window window)))

;; (defun msb-item-handler (buffer &optional maxbuf)
;;   "Create one string item, concerning BUFFER, for the buffer menu.
;; The item looks like:
;; *% <buffer-name>
;; The `*' appears only if the buffer is marked as modified.
;; The `%' appears only if the buffer is read-only.
;; Optional second argument MAXBUF is completely ignored."
;;   (let ((name (or (buffer-file-name) (buffer-name)))  ;;; tjf
;;         (modified (if (buffer-modified-p) "*" " "))
;;         (read-only (if buffer-read-only "%" " ")))
;;     (format "%s%s %s" modified read-only name)))

;; (defun handle-delete-frame (event)
;;   (interactive "e")
;;   (let ((frame (posn-window (event-start event)))
;;         (i 0)
;;         (tail (frame-list)))
;;     (while tail
;;       (and (frame-visible-p (car tail))
;;            (not (eq (car tail) frame))
;;            (setq i (1+ i)))
;;       (setq tail (cdr tail)))
;;     (if (> i 0)
;;         (exit-buffer-and-frame)     ;;; tjf
;;       (save-buffers-kill-emacs))))

(eval-after-load "man"
  '(defun Man-notify-when-ready (man-buffer)
     "Notify the user when MAN-BUFFER is ready.
See the variable `Man-notify-method' for the different notification behaviors."
     (let ((saved-frame (with-current-buffer Man-original-frame)))
       (cond
        ((eq Man-notify-method 'newframe)
         ;; Since we run asynchronously, perhaps while Emacs is waiting
         ;; for input, we must not leave a different buffer current.  We
         ;; can't rely on the editor command loop to reselect the
         ;; selected window's buffer.
         (save-excursion
           (let ((frame (make-frame Man-frame-parameters)))
             (set-window-buffer (frame-selected-window frame) man-buffer)
             ;; tjf          (set-window-dedicated-p (frame-selected-window frame) t)
             (or (display-multi-frame-p frame)
                 (select-frame frame)))))
        ((eq Man-notify-method 'pushy)
         (switch-to-buffer man-buffer))
        ((eq Man-notify-method 'bully)
         (and (frame-live-p saved-frame)
              (select-frame saved-frame))
         (pop-to-buffer man-buffer)
         (delete-other-windows))
        ((eq Man-notify-method 'aggressive)
         (and (frame-live-p saved-frame)
              (select-frame saved-frame))
         (pop-to-buffer man-buffer))
        ((eq Man-notify-method 'friendly)
         (and (frame-live-p saved-frame)
              (select-frame saved-frame))
         (display-buffer man-buffer 'not-this-window))
        ((eq Man-notify-method 'polite)
         (beep)
         (message "Manual buffer %s is ready" (buffer-name man-buffer)))
        ((eq Man-notify-method 'quiet)
         (message "Manual buffer %s is ready" (buffer-name man-buffer)))
        ((or (eq Man-notify-method 'meek)
             t)
         (message ""))
        )))
  )

(require 'dnd)

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

(with-eval-after-load 'msb
  '(defun msb-menu-bar-update-buffers (&optional arg)
     "A re-written version of `menu-bar-update-buffers'."
     ;; If user discards the Buffers item, play along.
     (when (and (lookup-key (current-global-map) [menu-bar buffer])
                (or (not (fboundp 'frame-or-buffer-changed-p))
                    (frame-or-buffer-changed-p)
                    arg))
       (let ((frames (frame-list))
             buffers-menu frames-menu)
         ;; Make the menu of buffers proper.
         (setq msb--last-buffer-menu (msb--create-buffer-menu))
         ;; Skip the `keymap' symbol.
         (setq buffers-menu (cdr msb--last-buffer-menu))
         ;; Make a Frames menu if we have more than one frame.
         (when (cdr frames)
           (let* ((frame-length (length frames))
                  (f-title  (format "Windows (%d)" frame-length)))  ;; tjf
             ;; List only the N most recently selected frames
             (when (and (integerp msb-max-menu-items)
                        (> msb-max-menu-items 1)
                        (> frame-length msb-max-menu-items))
               (setcdr (nthcdr msb-max-menu-items frames) nil))
             (setq frames-menu
                   (nconc
                    (list 'frame f-title '(nil) 'keymap f-title)
                    (mapcar
                     (lambda (frame)
                       (nconc
                        (list (frame-parameter frame 'name)
                              (frame-parameter frame 'name)
                              (cons nil nil))
                        `(lambda ()
                           (interactive) (menu-bar-select-frame ,frame))))
                     frames)))))
         (setcdr global-buffers-menu-map
                 (if (and buffers-menu frames-menu)
                     ;; Combine Frame and Buffers menus with separator between
                     (nconc (list "Buffers and Frames" frames-menu
                                  (and msb-separator-diff '(separator "--")))
                            (cdr buffers-menu))
                   buffers-menu))))))

(with-eval-after-load 'bm
  (defun bm-show-display-lines (header lines)
    "Show bookmarked LINES to the `bm-show-buffer-name' buffer."
    (if (= (length lines) 0)
        (message "No bookmarks defined.")
      (with-output-to-temp-buffer bm-show-buffer-name
        (set-buffer standard-output)
        (insert lines)
        (bm-show-mode)
        ;; Can only set header-line-format after bm-show-mode has called
        ;; kill-all-local-variables.  This use of propertize allows the
        ;; header line to line up with the left fringe, thanks
        ;; http://www.emacswiki.org/emacs/HeaderLine!
        (setq header-line-format
              (concat (propertize " " 'display '((space :align-to 0))) header))
        (setq buffer-read-only t)
        (when bm-electric-show
          (pop-to-buffer (current-buffer))
          (fit-window-to-buffer) ;; tjf
          ))))

  (defun bm-show-extract-bookmarks (&optional lifo-order all)
    "Return (HEADER BOOKMARK-LIST) for displaying a list of bookmarks.
Both are strings to be used in the bookmark lists provided to
users by the likes of `bm-show' and `bm-show-all'."
    ;; format-non-nil is just like format except it ignores any nil
    ;; arguments.  For example, (format-non-nil "%s %s" "foo" nil "bar")
    ;; yields "foo bar".  This is useful below where we conditionally
    ;; omit annotations.
    ;;
    ;; lstrip strips trailing white space from STR.  lstrip was stolen
    ;; from s.el and
    ;; http://ergoemacs.org/emacs/modernization_elisp_lib_problem.html.
    (cl-flet ((format-non-nil (format-string &rest args)
                              (apply #'format format-string (delete nil args)))
              (lstrip (str)
                      (if (string-match "\\`[ \t\n\r]+" str)
                          (replace-match "" t t str)
                        str)))
      (let* ((bookmarks (if lifo-order
                            (bm-overlays-lifo-order all)
                          (if all (bm-overlay-all)
                            (bm-overlay-in-buffer))))
             (file-line-width (bm-find-file-line-max-width bookmarks all))
             (format-string (concat (format "%%-%ds" file-line-width)
                                    (when bm-show-annotations
                                      (format " %%-%ds" bm-annotation-width))
                                    " %s")))
        (list
         ;; The header
         (format-non-nil format-string
                         (if all
                             (format "%s:%s" bm-header-buffer-name
                                     bm-header-line)
                           bm-header-line)
                         (when bm-show-annotations
                           bm-header-annotation)
                         bm-header-contents)
         ;; The bookmark list
         (mapconcat
          #'(lambda (bm)
              (with-current-buffer (overlay-buffer bm)
                (let* ((line (lstrip (buffer-substring (overlay-start bm)
                                                       (overlay-end bm))))
                       ;; line numbers start on 1
                       (line-num (+ 1 (count-lines (point-min) (overlay-start bm)))) 
                       (string
                        (format-non-nil format-string
                                        (if all
                                            (format "%s:%d" (buffer-name)
                                                    line-num)
                                          line-num)
                                        (when bm-show-annotations
                                          (or (overlay-get bm 'annotation) ""))
                                        (if (string-match "\n$" line)
                                            line
                                          (concat line "\n")))))
                  (put-text-property 0 (length string) 'bm-buffer (buffer-name) string)
                  (put-text-property 0 (length string) 'bm-bookmark bm string)
                  (add-text-properties 0 (length (format "%s:%d" (buffer-name) line-num))
                                       '(font-lock-face bookmark-menu-bookmark
                                                        mouse-face highlight
                                                        follow-link t
                                                        help-echo "mouse-1: go to this bookmark in other window") string) ;; tjf
                  string)))
          bookmarks
          "")))))
  )

(defun undo-tree-update-menu-bar ()
  "Update `undo-tree-mode' Edit menu items."
  (if undo-tree-mode
      (progn
	;; save old undo menu item, and install undo/redo menu items
	(setq undo-tree-old-undo-menu-item
	      (cdr (assq 'undo (lookup-key global-map [menu-bar Edit]))))
	(define-key (lookup-key global-map [menu-bar Edit])
	  [Undo] '(menu-item "Undo" undo-tree-undo
			     :enable (and undo-tree-mode
					  (not buffer-read-only)
					  (not (eq t buffer-undo-list))
					  (undo-tree-node-previous
					   (undo-tree-current buffer-undo-tree)))
			     :help "Undo last operation"))
	(define-key-after (lookup-key global-map [menu-bar Edit])
	  [Redo] '(menu-item "Redo" undo-tree-redo
			     :enable (and undo-tree-mode
					  (not buffer-read-only)
					  (not (eq t buffer-undo-list))
					  (undo-tree-node-next
					   (undo-tree-current buffer-undo-tree)))
			     :help "Redo last operation")
	  'Undo))
    ;; uninstall undo/redo menu items
    (define-key (lookup-key global-map [menu-bar Edit]) [Undo] undo-tree-old-undo-menu-item)
    (define-key (lookup-key global-map [menu-bar Edit]) [Redo] nil)))

;;
(message "Loading u-overload...done")
(provide 'u-overload)

;;; u-overload.el ends here
