;;; tjf-tabline.el --- Display a tab bar in the tab-line -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*- ;; -*-no-byte-compile: t; -*-

;;         Copyright © 2021-2023 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   03-Feb-2021

;; Original Author:     David Ponce <david@dponce.com>
;; Original Maintainer: David Ponce <david@dponce.com>
;; Created:  25 February 2003
;; Keywords: convenience
;; This file is not part of GNU Emacs.

;; This is a heavily modified version of the original tabbar.el

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

;; This library provides the Tabbar global minor mode to display a tab
;; bar in the header line of Emacs 21 and later versions.  You can use
;; the mouse to click on a tab and select it.  Also, three buttons are
;; displayed on the left side of the tab bar in this order: the
;; "home", "scroll left", and "scroll right" buttons.  The "home"
;; button is a general purpose button used to change something on the
;; tab bar.  The scroll left and scroll right buttons are used to
;; scroll tabs horizontally.  Tabs can be divided up into groups to
;; maintain several sets of tabs at the same time (see also the
;; chapter "Core" below for more details on tab grouping).  Only one
;; group is displayed on the tab bar, and the "home" button, for
;;
;; These commands, and default keyboard shortcuts, are provided:
;;
;; `tabbar-mode'
;;     Toggle the Tabbar global minor mode.  When enabled a tab bar is
;;     displayed in the tab-line.
;;
;; `tabbar-local-mode'         (C-c <C-f10>)
;;     Toggle the Tabbar-Local minor mode.  Provided the global minor
;;     mode is turned on, the tab bar becomes local in the current
;;     buffer when the local minor mode is enabled.  This permits to
;;     see the tab bar in a buffer where the tab-line is already
;;     used by another mode (like `Info-mode' for example).
;;
;; `tabbar-mwheel-mode'
;;     Toggle the Tabbar-Mwheel global minor mode.  When enabled you
;;     can use the mouse wheel to navigate through tabs of groups.
;;
;; `tabbar-press-home'         (C-c <C-home>)
;; `tabbar-press-scroll-left'  (C-c <C-prior>)
;; `tabbar-press-scroll-right' (C-c <C-next>)
;;     Simulate a mouse-1 click on respectively the "home", "scroll
;;     left", and "scroll right" buttons.  A numeric prefix argument
;;     value of 2, or 3, respectively simulates a mouse-2, or mouse-3
;;     click.
;;
;; `tabbar-backward'           (C-c <C-left>)
;; `tabbar-forward'            (C-c <C-right>)
;;     Are the basic commands to navigate cyclically through tabs or
;;     groups of tabs.  The cycle is controlled by the
;;     `tabbar-cycle-scope' option.  The default is to navigate
;;     through all tabs across all existing groups of tabs.  You can
;;     change the default behavior to navigate only through the tabs
;;     visible on the tab bar, or through groups of tabs only.  Or use
;;     the more specialized commands below.
;;
;; `tabbar-backward-tab'
;; `tabbar-forward-tab'
;;     Navigate through the tabs visible on the tab bar.
;;
;; `tabbar-backward-group'     (C-c <C-up>)
;; `tabbar-forward-group'      (C-c <C-down>)
;;     Navigate through existing groups of tabs.
;;
;;
;; Core
;; ----
;;
;; The content of the tab bar is represented by an internal data
;; structure: a tab set.  A tab set is a collection (group) of tabs,
;; identified by an unique name.  In a tab set, at any time, one and
;; only one tab is designated as selected within the tab set.
;;
;; A tab is a simple data structure giving the value of the tab, and a
;; reference to its tab set container.  A tab value can be any Lisp
;; object.  Each tab object is guaranteed to be unique.
;;
;; A tab set is displayed on the tab bar through a "view" defined by
;; the index of the leftmost tab shown.  Thus, it is possible to
;; scroll the tab bar horizontally by changing the start index of the
;; tab set view.
;;
;; The visual representation of a tab bar is a list of valid
;; `tab-line-format' template elements, one for each special
;; button, and for each tab found into a tab set "view".  When the
;; visual representation of a tab is required, the function specified
;; in the variable `tabbar-tab-label-function' is called to obtain it.
;; The visual representation of a special button is obtained by
;; calling the function specified in `tabbar-button-label-function',
;; which is passed a button name among `home', `scroll-left', or
;; `scroll-right'.  There are also options and faces to customize the
;; appearance of buttons and tabs (see the code for more details).
;;
;; When the mouse is over a tab, the function specified in
;; `tabbar-help-on-tab-function' is called, which is passed the tab
;; and should return a help string to display.  When a tab is
;; selected, the function specified in `tabbar-select-tab-function' is
;; called, which is passed the tab and the event received.
;;
;; Similarly, to control the behavior of the special buttons, the
;; following variables are available, for respectively the `home',
;; `scroll-left' and `scroll-right' value of `<button>':
;;
;; `tabbar-<button>-function'
;;    Function called when <button> is selected.  The function is
;;    passed the mouse event received.
;;
;; `tabbar-<button>-help-function'
;;    Function called with no arguments to obtain a help string
;;    displayed when the mouse is over <button>.
;;
;; To increase performance, each tab set automatically maintains its
;; visual representation in a cache.  As far as possible, the cache is
;; used to display the tab set, and refreshed only when necessary.
;;
;; Several tab sets can be maintained at the same time.  Only one is
;; displayed on the tab bar, it is obtained by calling the function
;; specified in the variable `tabbar-current-tabset-function'.
;;
;; A special tab set is maintained, that contains the list of the
;; currently selected tabs in the existing tab sets.  This tab set is
;; useful to show the existing tab sets in a tab bar, and switch
;; between them easily.  The function `tabbar-get-tabsets-tabset'
;; returns this special tab set.
;;
;;
;; Buffer tabs
;; -----------
;;
;; The default tab bar implementation provided displays buffers in
;; dedicated tabs.  Selecting a tab, switch (mouse-1), or pop
;; (mouse-2), to the buffer it contains.
;;
;; The list of buffers put in tabs is provided by the function
;; specified in the variable `tabbar-buffer-list-function'.  The
;; default function: `tabbar-buffer-list', excludes buffers whose name
;; starts with a space, when they are not visiting a file.
;;
;; Buffers are organized in groups, each one represented by a tab set.
;; A buffer can have no group, or belong to more than one group.  The
;; function specified by the variable `tabbar-buffer-groups-function'
;; is called for each buffer to obtain the groups it belongs to.  The
;; default function provided: `tabbar-buffer-groups' organizes buffers
;; depending on their major mode (see that function for details).
;;
;; The "home" button toggles display of buffer groups on the tab bar,
;; allowing to easily show another buffer group by clicking on the
;; associated tab.

;; Revision:

;;; Code:

(message "Configuring from tjf-tabline...")
(require 'powerline)

;;
;;; Options
;;
(defgroup tjf-tabline nil
  "Display a tab bar in the tab-line."
  :group 'convenience)

(defcustom tjf:tabline/cycle-scope nil
  "*Specify the scope of cyclic navigation through tabs.
The following scopes are possible:

- `tabs'
    Navigate through visible tabs only.
- `groups'
    Navigate through tab groups only.
- default
    Navigate through visible tabs, then through tab groups."
  :group 'tjf-tabline
  :type '(choice :tag "Cycle through..."
                 (const :tag "Visible Tabs Only" tabs)
                 (const :tag "Tab Groups Only" groups)
                 (const :tag "Visible Tabs then Tab Groups" nil)))

(defcustom tjf:tabline/auto-scroll-flag t
  "*Non-nil means to automatically scroll the tab bar.
That is, when a tab is selected outside of the tab bar visible area,
the tab bar is scrolled horizontally so the selected tab becomes
visible."
  :group 'tjf-tabline
  :type 'boolean)

(defvar tjf:tabline/inhibit-functions '(tjf:tabline/default-inhibit-function)
  "List of functions to be called before displaying the tab bar.
Those functions are called one by one, with no arguments, until one of
them returns a non-nil value, and thus, prevents to display the tab
bar.")

(defvar tjf:tabline/current-tabset-function nil
  "Function called with no argument to obtain the current tab set.
This is the tab set displayed on the tab bar.")

(defvar tjf:tabline/select-tab-function nil
  "Function that select a tab.
The function is passed a mouse event and a tab, and should make it the
selected tab.")

(defvar tjf:tabline/help-on-tab-function nil
  "Function to obtain a help string for a tab.
The help string is displayed when the mouse is onto the button.  The
function is passed the tab and should return a help string or nil for
none.")

(defvar tjf:tabline/button-label-function nil
  "Function that obtains a button label displayed on the tab bar.
The function is passed a button name should return a propertized
string to display.")

(defvar tjf:tabline/home-function nil
  "Function called when clicking on the tab bar home button.
The function is passed the mouse event received.")

(defvar tjf:tabline/home-help-function nil
  "Function to obtain a help string for the tab bar home button.
The help string is displayed when the mouse is onto the button.
The function is called with no arguments.")

(defvar tjf:tabline/scroll-left-function 'tjf:tabline/scroll-left
  "Function that scrolls tabs on left.
The function is passed the mouse event received when clicking on the
scroll left button.  It should scroll the current tab set.")

(defvar tjf:tabline/scroll-left-help-function 'tjf:tabline/scroll-left-help
  "Function to obtain a help string for the scroll left button.
The help string is displayed when the mouse is onto the button.
The function is called with no arguments.")

(defvar tjf:tabline/scroll-right-function 'tjf:tabline/scroll-right
  "Function that scrolls tabs on right.
The function is passed the mouse event received when clicking on the
scroll right button.  It should scroll the current tab set.")

(defvar tjf:tabline/scroll-right-help-function 'tjf:tabline/scroll-right-help
  "Function to obtain a help string for the scroll right button.
The help string is displayed when the mouse is onto the button.
The function is called with no arguments.")

;;; Misc.
;;
(eval-and-compile
  (defalias 'tjf:tabline/display-update
    (if (fboundp 'force-window-update)
        #'(lambda () (force-window-update (selected-window)))
      'force-mode-line-update)))

(defsubst tjf:tabline/click-p (event)
  "Return non-nil if EVENT is a mouse click event."
  (memq 'click (event-modifiers event)))

(defun tjf:tabline/shorten (str width)
  "Return a shortened string from STR that fits in the given display WIDTH.
WIDTH is specified in terms of character display width in the current
buffer; see also `char-width'.  If STR display width is greater than
WIDTH, STR is truncated and an ellipsis string \"...\" is inserted at
end or in the middle of the returned string, depending on available
room."
  (let* ((n  (length str))
         (sw (string-width str))
         (el "...")
         (ew (string-width el))
         (w  0)
         (i  0))
    (cond
     ;; STR fit in WIDTH, return it.
     ((<= sw width)
      str)
     ;; There isn't enough room for the ellipsis, STR is just
     ;; truncated to fit in WIDTH.
     ((<= width ew)
      (while (< w width)
        (setq w (+ w (char-width (aref str i)))
              i (1+ i)))
      (substring str 0 i))
     ;; There isn't enough room to insert the ellipsis in the middle
     ;; of the truncated string, so put the ellipsis at end.
     ((zerop (setq sw (/ (- width ew) 2)))
      (setq width (- width ew))
      (while (< w width)
        (setq w (+ w (char-width (aref str i)))
              i (1+ i)))
      (concat (substring str 0 i) el))
     ;; Put the ellipsis in the middle of the truncated string.
     (t
      (while (< w sw)
        (setq w (+ w (char-width (aref str i)))
              i (1+ i)))
      (setq w (+ w ew))
      (while (< w width)
        (setq n (1- n)
              w (+ w (char-width (aref str n)))))
      (concat (substring str 0 i) el (substring str n)))
     )))

;;; Tab and tab set
;;
(defsubst tjf:tabline/make-tab (object tabset)
  "Return a new tab with value OBJECT.
TABSET is the tab set the tab belongs to."
  (cons object tabset))

(defsubst tjf:tabline/tab-value (tab)
  "Return the value of tab TAB."
  (car tab))

(defsubst tjf:tabline/tab-tabset (tab)
  "Return the tab set TAB belongs to."
  (cdr tab))

(defvar tjf:tabline/tabsets nil
  "The tab sets store.")

(defvar tjf:tabline/tabsets-tabset nil
  "The special tab set of existing tab sets.")

(defvar tjf:tabline/current-tabset nil
  "The tab set currently displayed on the tab bar.")
(make-variable-buffer-local 'tjf:tabline/current-tabset)

(defvar tjf:tabline/init-hook nil
  "Hook run after tab bar data has been initialized.
You should use this hook to initialize dependent data.")

(defsubst tjf:tabline/init-tabsets-store ()
  "Initialize the tab set store."
  (setq tjf:tabline/tabsets (make-vector 31 0)
        tjf:tabline/tabsets-tabset (make-symbol "tjf:tabline/tabsets-tabset"))
  (put tjf:tabline/tabsets-tabset 'start 0)
  (run-hooks 'tjf:tabline/init-hook))

(defvar tjf:tabline/quit-hook nil
  "Hook run after tab bar data has been freed.
You should use this hook to reset dependent data.")

(defsubst tjf:tabline/free-tabsets-store ()
  "Free the tab set store."
  (setq tjf:tabline/tabsets nil
        tjf:tabline/tabsets-tabset nil)
  (run-hooks 'tjf:tabline/quit-hook))

;; Define an "hygienic" function free of side effect between its local
;; variables and those of the callee.
(eval-and-compile
  (defalias 'tjf:tabline/map-tabsets
    (let ((function (make-symbol "function"))
          (result   (make-symbol "result"))
          (tabset   (make-symbol "tabset")))
      `(lambda (,function)
         "Apply FUNCTION to each tab set, and make a list of the results.
The result is a list just as long as the number of existing tab sets."
         (let (,result)
           (if tjf:tabline/tabsets
	       (mapatoms
		#'(lambda (,tabset)
		    (push (funcall ,function ,tabset) ,result))
		tjf:tabline/tabsets))
           ,result)))))

(defun tjf:tabline/make-tabset (name &rest objects)
  "Make a new tab set whose name is the string NAME.
It is initialized with tabs build from the list of OBJECTS."
  (let* ((tabset (intern name tjf:tabline/tabsets))
         (tabs (mapcar #'(lambda (object)
                           (tjf:tabline/make-tab object tabset))
                       objects)))
    (set tabset tabs)
    (put tabset 'select (car tabs))
    (put tabset 'start 0)
    tabset))

(defsubst tjf:tabline/get-tabset (name)
  "Return the tab set whose name is the string NAME.
Return nil if not found."
  (intern-soft name tjf:tabline/tabsets))

(defsubst tjf:tabline/delete-tabset (tabset)
  "Delete the tab set TABSET.
That is, remove it from the tab sets store."
  (unintern tabset tjf:tabline/tabsets))

(defsubst tjf:tabline/tabs (tabset)
  "Return the list of tabs in TABSET."
  (symbol-value tabset))

(defsubst tjf:tabline/tab-values (tabset)
  "Return the list of tab values in TABSET."
  (mapcar 'tjf:tabline/tab-value (tjf:tabline/tabs tabset)))

(defsubst tjf:tabline/get-tab (object tabset)
  "Search for a tab with value OBJECT in TABSET.
Return the tab found, or nil if not found."
  (assoc object (tjf:tabline/tabs tabset)))

(defsubst tjf:tabline/member (tab tabset)
  "Return non-nil if TAB is in TABSET."
  (or (eq (tjf:tabline/tab-tabset tab) tabset)
      (memq tab (tjf:tabline/tabs tabset))))

(defsubst tjf:tabline/template (tabset)
  "Return the cached visual representation of TABSET.
That is, a `tab-line-format' template, or nil if the cache is
empty."
  (get tabset 'template))

(defsubst tjf:tabline/set-template (tabset template)
  "Set the cached visual representation of TABSET to TEMPLATE.
TEMPLATE must be a valid `tab-line-format' template, or nil to
cleanup the cache."
  (put tabset 'template template))

(defsubst tjf:tabline/selected-tab (tabset)
  "Return the tab selected in TABSET."
  (get tabset 'select))

(defsubst tjf:tabline/selected-value (tabset)
  "Return the value of the tab selected in TABSET."
  (tjf:tabline/tab-value (tjf:tabline/selected-tab tabset)))

(defsubst tjf:tabline/selected-p (tab tabset)
  "Return non-nil if TAB is the selected tab in TABSET."
  (eq tab (tjf:tabline/selected-tab tabset)))

(defsubst tjf:tabline/modified-p (tab tabset)
  "Return non-nil if TAB is a modified tab in TABSET."
  (and (buffer-modified-p (tjf:tabline/tab-value tab))
       (buffer-file-name (tjf:tabline/tab-value tab))))

(defvar tjf:tabline/-track-selected nil)

(defsubst tjf:tabline/select-tab (tab tabset)
  "Make TAB the selected tab in TABSET.
Does nothing if TAB is not found in TABSET.
Return TAB if selected, nil if not."
  (when (tjf:tabline/member tab tabset)
    (unless (tjf:tabline/selected-p tab tabset)
      (tjf:tabline/set-template tabset nil)
      (setq tjf:tabline/-track-selected tjf:tabline/auto-scroll-flag))
    (put tabset 'select tab)))

(defsubst tjf:tabline/select-tab-value (object tabset)
  "Make the tab with value OBJECT, the selected tab in TABSET.
Does nothing if a tab with value OBJECT is not found in TABSET.
Return the tab selected, or nil if nothing was selected."
  (tjf:tabline/select-tab (tjf:tabline/get-tab object tabset) tabset))

(defsubst tjf:tabline/start (tabset)
  "Return the index of the first visible tab in TABSET."
  (get tabset 'start))

(defsubst tjf:tabline/view (tabset)
  "Return the list of visible tabs in TABSET.
That is, the sub-list of tabs starting at the first visible one."
  (nthcdr (tjf:tabline/start tabset) (tjf:tabline/tabs tabset)))

(defun tjf:tabline/add-tab (tabset object &optional _append_ignored)
  "Add to TABSET a tab with value OBJECT if there isn't one there yet.
 If the tab is added, it is added at the beginning of the tab list,
 unless the optional argument APPEND is non-nil, in which case it is
 added at the end."
  (let ((tabs (tjf:tabline/tabs tabset)))
    (if (tjf:tabline/get-tab object tabset)
        tabs
      (let ((tab (tjf:tabline/make-tab object tabset)))
        (tjf:tabline/set-template tabset nil)
        (set tabset (sort (cons tab tabs)
                          (lambda (a b) (string< (buffer-name (car a))
                                                 (buffer-name (car b))))))))))

;; (defun tjf:tabline/add-tab (tabset object &optional append)
;;   "Add to TABSET a tab with value OBJECT if there isn't one there yet.
;; If the tab is added, it is added at the beginning of the tab list,
;; unless the optional argument APPEND is non-nil, in which case it is
;; added at the end."
;;   (let ((tabs (tjf:tabline/tabs tabset)))
;;     (if (tjf:tabline/get-tab object tabset)
;;         tabs
;;       (let ((tab (tjf:tabline/make-tab object tabset)))
;;         (tjf:tabline/set-template tabset nil)
;;         (set tabset (if append
;;                         (append tabs (list tab))
;;                       (cons tab tabs)))))))

(defun tjf:tabline/delete-tab (tab)
  "Remove TAB from its tab set."
  (let* ((tabset (tjf:tabline/tab-tabset tab))
         (tabs   (tjf:tabline/tabs tabset))
         (sel    (eq tab (tjf:tabline/selected-tab tabset)))
         (next   (and sel (cdr (memq tab tabs)))))
    (tjf:tabline/set-template tabset nil)
    (setq tabs (delq tab tabs))
    ;; When the selected tab is deleted, select the next one, if
    ;; available, or the last one otherwise.
    (and sel (tjf:tabline/select-tab (car (or next (last tabs))) tabset))
    (set tabset tabs)))

(defun tjf:tabline/scroll (tabset count)
  "Scroll the visible tabs in TABSET of COUNT units.
If COUNT is positive move the view on right.  If COUNT is negative,
move the view on left."
  (let ((start (min (max 0 (+ (tjf:tabline/start tabset) count))
                    (1- (length (tjf:tabline/tabs tabset))))))
    (when (/= start (tjf:tabline/start tabset))
      (tjf:tabline/set-template tabset nil)
      (put tabset 'start start))))

(defun tjf:tabline/tab-next (tabset tab &optional before)
  "Search in TABSET for the tab after TAB.
If optional argument BEFORE is non-nil, search for the tab before
TAB.  Return the tab found, or nil otherwise."
  (let* (last (tabs (tjf:tabline/tabs tabset)))
    (while (and tabs (not (eq tab (car tabs))))
      (setq last (car tabs)
            tabs (cdr tabs)))
    (and tabs (if before last (nth 1 tabs)))))

(defun tjf:tabline/current-tabset (&optional update)
  "Return the tab set currently displayed on the tab bar.
If optional argument UPDATE is non-nil, call the user defined function
`tjf:tabline/current-tabset-function' to obtain it.  Otherwise return the
current cached copy."
  (and update tjf:tabline/current-tabset-function
       (setq tjf:tabline/current-tabset
             (funcall tjf:tabline/current-tabset-function)))
  tjf:tabline/current-tabset)

(defun tjf:tabline/get-tabsets-tabset ()
  "Return the tab set of selected tabs in existing tab sets."
  (set tjf:tabline/tabsets-tabset (tjf:tabline/map-tabsets 'tjf:tabline/selected-tab))
  (tjf:tabline/scroll tjf:tabline/tabsets-tabset 0)
  (tjf:tabline/set-template tjf:tabline/tabsets-tabset nil)
  tjf:tabline/tabsets-tabset)

;;; Faces
;;
(defface tjf:tabline/default
  '((t :inherit variable-pitch :height 0.8 :background "#2f2f2f" :foreground "#2f2f2f" :box nil))
  "Default face used in the tab bar."
  :group 'tjf-tabline)

(defface tjf:tabline/unselected
  '((t :inherit tjf:tabline/default :background "#424747" :foreground "#cccccc"))
  "Face used for unselected tabs."
  :group 'tjf-tabline)

(defface tjf:tabline/selected
  '((t :inherit tjf:tabline/default :background "xxx"     :foreground "black"))
  "Face used for the selected tab."
  :group 'tjf-tabline)

(defface tjf:tabline/modified
  '((t :inherit tjf:tabline/default :background "red"     :foreground "white"))
  "Face used for unsaved tabs."
  :group 'tjf-tabline)

(defface tjf:tabline/sel-mod
  '((t     :inherit tjf:tabline/selected                  :foreground "red"))
  "Face used for unsaved and selected tabs."
  :group 'tjf-tabline)

(defface tjf:tabline/highlight
  '((t :underline t))
  "Face used to highlight a tab during mouse-overs."
  :group 'tjf-tabline)

(defface tjf:tabline/separator
  '((t :inherit tjf:tabline/default))
  "Face used for separators between tabs."
  :group 'tjf-tabline)

(defface tjf:tabline/button
  '((t :inherit tjf:tabline/unselected))
  "Face used for tab bar buttons."
  :group 'tjf-tabline)

(defface tjf:tabline/button-highlight
  '((t :inherit tjf:tabline/default))
  "Face used to highlight a button during mouse-overs."
  :group 'tjf-tabline)

(defcustom tjf:tabline/background-color nil
  "*Background color of the tab bar.
By default, use the background color specified for the
`tjf:tabline/default' face (or inherited from another face), or the
background color of the `default' face otherwise."
  :group 'tjf-tabline
  :type '(choice (const :tag "Default" nil)
                 (color)))

(defsubst tjf:tabline/background-color ()
  "Return the background color of the tab bar."
  (or tjf:tabline/background-color
      (let* ((face 'tjf:tabline/default)
             (color (face-background face)))
        (while (null color)
          (or (facep (setq face (face-attribute face :inherit)))
              (setq face 'default))
          (setq color (face-background face)))
        color)))

;;; Buttons and separator look and feel
;;
(defconst tjf:tabline/button-widget
  '(cons
    (cons :tag "Enabled"
          (string)
          (repeat :tag "Image"
                  :extra-offset 2
                  (restricted-sexp :tag "Spec"
                                   :match-alternatives (listp))))
    (cons :tag "Disabled"
          (string)
          (repeat :tag "Image"
                  :extra-offset 2
                  (restricted-sexp :tag "Spec"
                                   :match-alternatives (listp))))
    )
  "Widget for editing a tab bar button.
A button is specified as a pair (ENABLED-BUTTON . DISABLED-BUTTON),
where ENABLED-BUTTON and DISABLED-BUTTON specify the value used when
the button is respectively enabled and disabled.  Each button value is
a pair (STRING . IMAGE) where STRING is a string value, and IMAGE a
list of image specifications.
If IMAGE is non-nil, try to use that image, else use STRING.
If only the ENABLED-BUTTON image is provided, a DISABLED-BUTTON image
is derived from it.")

(defvar tjf:tabline/height 20)
(defvar tjf:tabline/left  (powerline-wave-right 'tjf:tabline/default nil tjf:tabline/height))
(defvar tjf:tabline/right (powerline-wave-left  nil 'tjf:tabline/default tjf:tabline/height))

(defun tjf:tabline/label-function (tab)
  (powerline-render (list tjf:tabline/left (format " %s " (car tab)) tjf:tabline/right)))

(defvar tjf:tabline/tab-label-function #'tjf:tabline/label-function
  "Function that obtains a tab label displayed on the tab bar.
The function is passed a tab and should return a string.")

;;; Home button
;;
(defvar tjf:tabline/home-button-value nil
  "Value of the home button.")

(defconst tjf:tabline/home-button-enabled-image
  '((:type pbm :data "\
P2 13 13 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0
6 0 255 255 255 255 255 255 255 255 255 255 9 130 9 255 255 255 255
255 255 255 255 255 255 26 130 26 255 255 255 255 255 255 255 0 9 26
41 130 41 26 9 0 255 255 255 255 5 145 140 135 130 125 120 115 5 255
255 255 255 0 9 26 41 130 41 26 9 0 255 255 255 255 255 255 255 26 130
26 255 255 255 255 255 255 255 255 255 255 9 130 9 255 255 255 255 255
255 255 255 255 255 0 6 0 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255
"))
  "Default image for the enabled home button.")

(defconst tjf:tabline/home-button-disabled-image
  '((:type pbm :data "\
P2 13 13 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 0 0 1 2 3 2 1 0 0 255 255 255 255 0 132 128 123 119 114 110
106 0 255 255 255 255 0 0 1 2 3 2 1 0 0 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255
"))
  "Default image for the disabled home button.")

(defcustom tjf:tabline/home-button
  (cons (cons "[o]" tjf:tabline/home-button-enabled-image)
        (cons "[x]" tjf:tabline/home-button-disabled-image))
  "The home button.
The variable `tjf:tabline/button-widget' gives details on this widget."
  :group 'tjf-tabline
  :type tjf:tabline/button-widget
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of button value.
          (setq tjf:tabline/home-button-value nil)))

;;; Scroll left button
;;
(defvar tjf:tabline/scroll-left-button-value nil
  "Value of the scroll left button.")

(defconst tjf:tabline/scroll-left-button-enabled-image
  '((:type pbm :data "\
P2 13 13 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 128 16 48 255 255 255 255 255 255 255
255 144 28 86 128 0 255 255 255 255 255 255 160 44 92 159 135 113 0
255 255 255 255 160 44 97 165 144 129 120 117 0 255 255 176 44 98 175
174 146 127 126 127 128 0 255 255 0 160 184 156 143 136 134 135 137
138 0 255 255 176 32 67 144 146 144 145 146 148 149 0 255 255 255 255
160 42 75 140 154 158 159 160 0 255 255 255 255 255 255 160 40 74 154
170 171 0 255 255 255 255 255 255 255 255 160 41 82 163 0 255 255 255
255 255 255 255 255 255 255 160 32 48 255 255 255 255 255 255 255 255
255 255 255 255 255 255
"))
  "Default image for the enabled scroll left button.
A disabled button image will be automatically build from it.")

(defcustom tjf:tabline/scroll-left-button
  (cons (cons " <" tjf:tabline/scroll-left-button-enabled-image)
        (cons " =" nil))
  "The scroll left button.
The variable `tjf:tabline/button-widget' gives details on this widget."
  :group 'tjf-tabline
  :type tjf:tabline/button-widget
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of button value.
          (setq tjf:tabline/scroll-left-button-value nil)))

;;; Scroll right button
;;
(defvar tjf:tabline/scroll-right-button-value nil
  "Value of the scroll right button.")

(defconst tjf:tabline/scroll-right-button-enabled-image
  '((:type pbm :data "\
P2 13 13 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
48 32 160 255 255 255 255 255 255 255 255 255 255 44 161 71 32 160 255
255 255 255 255 255 255 255 36 157 163 145 62 32 160 255 255 255 255
255 255 30 128 133 137 142 124 50 32 160 255 255 255 255 29 120 121
124 126 126 124 105 42 32 176 255 255 31 126 127 128 128 128 128 126
124 89 32 255 255 33 134 135 136 137 137 138 119 49 32 176 255 255 34
143 144 145 146 128 54 32 160 255 255 255 255 36 152 153 134 57 32 160
255 255 255 255 255 255 38 141 60 32 160 255 255 255 255 255 255 255
255 48 32 160 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255
"))
  "Default image for the enabled scroll right button.
A disabled button image will be automatically build from it.")

(defcustom tjf:tabline/scroll-right-button
  (cons (cons " >" tjf:tabline/scroll-right-button-enabled-image)
        (cons " =" nil))
  "The scroll right button.
The variable `tjf:tabline/button-widget' gives details on this widget."
  :group 'tjf-tabline
  :type tjf:tabline/button-widget
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of button value.
          (setq tjf:tabline/scroll-right-button-value nil)))

;;; Separator
;;
(defconst tjf:tabline/separator-widget
  '(cons (choice (string)
                 (number :tag "Space width" 0.2))
         (repeat :tag "Image"
                 :extra-offset 2
                 (restricted-sexp :tag "Spec"
                                  :match-alternatives (listp))))
  "Widget for editing a tab bar separator.
A separator is specified as a pair (STRING-OR-WIDTH . IMAGE) where
STRING-OR-WIDTH is a string value or a space width, and IMAGE a list
of image specifications.
If IMAGE is non-nil, try to use that image, else use STRING-OR-WIDTH.
The value (\"\"), or (0) hide separators.")

(defvar tjf:tabline/separator-value nil
  "Value of the separator used between tabs.")

(defcustom tjf:tabline/separator (list 0.2)
  "Separator used between tabs.
The variable `tjf:tabline/separator-widget' gives details on this widget."
  :group 'tjf-tabline
  :type tjf:tabline/separator-widget
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of separator value.
          (setq tjf:tabline/separator-value nil)))

;;; Images
;;
(defcustom tjf:tabline/use-images t
  "*Non-nil means to try to use images in tab bar.
That is for buttons and separators."
  :group 'tjf-tabline
  :type 'boolean
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of all buttons and separator values.
          (setq tjf:tabline/separator-value nil
                tjf:tabline/home-button-value nil
                tjf:tabline/scroll-left-button-value nil
                tjf:tabline/scroll-right-button-value nil)))

;; the following cache only provides minor speed benefits
;; but it may be a workaround for the close-tab/undo.png display issue
(defvar tjf:tabline/cached-image nil)
(defvar tjf:tabline/cached-spec nil)
(defsubst tjf:tabline/find-image (specs)
  "Find an image, choosing one of a list of image specifications.
SPECS is a list of image specifications.  See also `find-image'."
  (if (eq tjf:tabline/cached-spec specs)
      tjf:tabline/cached-image
    (when (and tjf:tabline/use-images (display-images-p))
      (condition-case nil
	  (prog1
	      (setq tjf:tabline/cached-image (find-image specs))
	    (setq tjf:tabline/cached-spec specs))
	(error nil)))))

(defsubst tjf:tabline/disable-image (image)
  "From IMAGE, return a new image which looks disabled."
  (setq image (copy-sequence image))
  (setcdr image (plist-put (cdr image) :conversion 'disabled))
  image)

(defsubst tjf:tabline/normalize-image (image &optional margin)
  "Make IMAGE centered and transparent.
If optional MARGIN is non-nil, it must be a number of pixels to add as
an extra margin around the image."
  (let ((plist (cdr image)))
    (or (plist-get plist :ascent)
        (setq plist (plist-put plist :ascent 'center)))
    (or (plist-get plist :mask)
        (setq plist (plist-put plist :mask '(heuristic t))))
    (or (not (natnump margin))
        (plist-get plist :margin)
        (plist-put plist :margin margin))
    (setcdr image plist))
  image)

;;; Button keymaps and callbacks
;;
(defun tjf:tabline/make-mouse-keymap (callback)
  "Return a keymap that call CALLBACK on mouse events.
CALLBACK is passed the received mouse event."
  (let ((keymap (make-sparse-keymap)))
    ;; Pass mouse-1, mouse-2 and mouse-3 events to CALLBACK.
    (define-key keymap [tab-line down-mouse-1] 'ignore)
    (define-key keymap [tab-line mouse-1] callback)
    (define-key keymap [tab-line down-mouse-2] 'ignore)
    (define-key keymap [tab-line mouse-2] callback)
    (define-key keymap [tab-line down-mouse-3] 'ignore)
    (define-key keymap [tab-line mouse-3] callback)
    keymap))

(defsubst tjf:tabline/make-mouse-event (&optional type)
  "Return a mouse click event.
Optional argument TYPE is a mouse-click event or one of the
symbols `mouse-1', `mouse-2' or `mouse-3'.
The default is `mouse-1'."
  (if (tjf:tabline/click-p type)
      type
    (list (or (memq type '(mouse-2 mouse-3)) 'mouse-1)
          (or (event-start nil) ;; Emacs 21.4
              (list (selected-window) (point) '(0 . 0) 0)))))

;;; Buttons
;;
(defconst tjf:tabline/default-button-keymap
  (tjf:tabline/make-mouse-keymap 'tjf:tabline/select-button-callback)
  "Default keymap of a button.")

(defun tjf:tabline/help-on-button (window object position)
  "Return a help string or nil for none, for the button under the mouse.
WINDOW is the window in which the help was found (unused).
OBJECT is the button label under the mouse.
POSITION is the position in that label.
Call `tjf:tabline/NAME-help-function' where NAME is the button name
associated to OBJECT."
  (let* ((name (get-text-property position 'tjf:tabline/button object))
         (funvar (and name
                      (intern-soft (format "tjf:tabline/%s-help-function"
                                           name)))))
    (and (symbol-value funvar)
         (funcall (symbol-value funvar)))))

(defsubst tjf:tabline/click-on-button (name &optional type)
  "Handle a mouse click event on button NAME.
Call `tjf:tabline/select-NAME-function' with the received, or simulated
mouse click event.
Optional argument TYPE is a mouse click event type (see the function
`tjf:tabline/make-mouse-event' for details)."
  (let ((funvar (intern-soft (format "tjf:tabline/%s-function" name))))
    (when (symbol-value funvar)
      (funcall (symbol-value funvar) (tjf:tabline/make-mouse-event type))
      (tjf:tabline/display-update))))

(defun tjf:tabline/select-button-callback (event)
  "Handle a mouse EVENT on a button.
Pass mouse click events on a button to `tjf:tabline/click-on-button'."
  (interactive "@e")
  (when (tjf:tabline/click-p event)
    (let ((target (posn-string (event-start event))))
      (tjf:tabline/click-on-button
       (get-text-property (cdr target) 'tjf:tabline/button (car target))
       event))))

(defun tjf:tabline/make-button-keymap (name)
  "Return a keymap to handle mouse click events on button NAME."
  (if (fboundp 'posn-string)
      tjf:tabline/default-button-keymap
    (let ((event (make-symbol "event")))
      (tjf:tabline/make-mouse-keymap
       `(lambda (,event)
          (interactive "@e")
          (and (tjf:tabline/click-p ,event)
               (tjf:tabline/click-on-button ',name ,event)))))))

;;; Button callbacks
;;
(defun tjf:tabline/scroll-left (event)
  "On mouse EVENT, scroll current tab set on left."
  (when (eq (event-basic-type event) 'mouse-1)
    (tjf:tabline/scroll (tjf:tabline/current-tabset) -1)))

(defun tjf:tabline/scroll-left-help ()
  "Help string shown when mouse is over the scroll left button."
  "mouse-1: scroll tabs left.")

(defun tjf:tabline/scroll-right (event)
  "On mouse EVENT, scroll current tab set on right."
  (when (eq (event-basic-type event) 'mouse-1)
    (tjf:tabline/scroll (tjf:tabline/current-tabset) 1)))

(defun tjf:tabline/scroll-right-help ()
  "Help string shown when mouse is over the scroll right button."
  "mouse-1: scroll tabs right.")

;;; Tabs
;;
(defconst tjf:tabline/default-tab-keymap
  (tjf:tabline/make-mouse-keymap 'tjf:tabline/select-tab-callback)
  "Default keymap of a tab.")

(defun tjf:tabline/help-on-tab (window object position)
  "Return a help string or nil for none, for the tab under the mouse.
WINDOW is the window in which the help was found (unused).
OBJECT is the tab label under the mouse.
POSITION is the position in that label.
Call `tjf:tabline/help-on-tab-function' with the associated tab."
  (when tjf:tabline/help-on-tab-function
    (let ((tab (get-text-property position 'tjf:tabline/tab object)))
      (funcall tjf:tabline/help-on-tab-function tab))))

(defsubst tjf:tabline/click-on-tab (tab &optional type)
  "Handle a mouse click event on tab TAB.
Call `tjf:tabline/select-tab-function' with the received, or simulated
mouse click event, and TAB.
Optional argument TYPE is a mouse click event type (see the function
`tjf:tabline/make-mouse-event' for details)."
  (when tjf:tabline/select-tab-function
    (funcall tjf:tabline/select-tab-function
             (tjf:tabline/make-mouse-event type) tab)
    (tjf:tabline/display-update)))

(defun tjf:tabline/select-tab-callback (event)
  "Handle a mouse EVENT on a tab.
Pass mouse click events on a tab to `tjf:tabline/click-on-tab'."
  (interactive "@e")
  (when (tjf:tabline/click-p event)
    (let ((target (posn-string (event-start event))))
      (tjf:tabline/click-on-tab
       (get-text-property (cdr target) 'tjf:tabline/tab (car target))
       event))))

(defun tjf:tabline/make-tab-keymap (tab)
  "Return a keymap to handle mouse click events on TAB."
  (if (fboundp 'posn-string)
      tjf:tabline/default-tab-keymap
    (let ((event (make-symbol "event")))
      (tjf:tabline/make-mouse-keymap
       `(lambda (,event)
          (interactive "@e")
          (and (tjf:tabline/click-p ,event)
               (tjf:tabline/click-on-tab ',tab ,event)))))))

;;; Tab bar construction
;;
(defun tjf:tabline/button-label (name)
  "Return a label for button NAME.
That is a pair (ENABLED . DISABLED), where ENABLED and DISABLED are
respectively the appearance of the button when enabled and disabled.
They are propertized strings which could display images, as specified
by the variable `tjf:tabline/NAME-button'."
  (let* ((btn (symbol-value
               (intern-soft (format "tjf:tabline/%s-button" name))))
         (on  (tjf:tabline/find-image (cdar btn)))
         (off (and on (tjf:tabline/find-image (cddr btn)))))
    (when on
      (tjf:tabline/normalize-image on 1)
      (if off
          (tjf:tabline/normalize-image off 1)
        ;; If there is no disabled button image, derive one from the
        ;; button enabled image.
        (setq off (tjf:tabline/disable-image on))))
    (cons
     (propertize (or (caar btn) " ") 'display on)
     (propertize (or (cadr btn) " ") 'display off))))

(defun tjf:tabline/line-button (name)
  "Return the display representation of button NAME.
That is, a propertized string used as an `tab-line-format' template
element."
  (let ((label (if tjf:tabline/button-label-function
                   (funcall tjf:tabline/button-label-function name)
                 (cons name name))))
    ;; Cache the display value of the enabled/disabled buttons in
    ;; variables `tjf:tabline/NAME-button-value'.
    (set (intern (format "tjf:tabline/%s-button-value"  name))
         (cons
          (propertize (car label)
                      'tjf:tabline/button name
                      'face 'tjf:tabline/button
                      'mouse-face 'tjf:tabline/button-highlight
                      'pointer 'hand
                      'local-map (tjf:tabline/make-button-keymap name)
                      'help-echo 'tjf:tabline/help-on-button)
          (propertize (cdr label)
                      'face 'tjf:tabline/button
                      'pointer 'arrow)))))

(defun tjf:tabline/line-separator ()
  "Return the display representation of a tab bar separator.
That is, a propertized string used as an `tab-line-format' template
element."
  (let ((image (tjf:tabline/find-image (cdr tjf:tabline/separator))))
    ;; Cache the separator display value in variable
    ;; `tjf:tabline/separator-value'.
    (setq tjf:tabline/separator-value
          (cond
           (image
            (propertize " "
                        'face 'tjf:tabline/separator
                        'pointer 'arrow
                        'display (tjf:tabline/normalize-image image)))
           ((numberp (car tjf:tabline/separator))
            (propertize " "
                        'face 'tjf:tabline/separator
                        'pointer 'arrow
                        'display (list 'space
                                       :width (car tjf:tabline/separator))))
           ((propertize (or (car tjf:tabline/separator) " ")
                        'face 'tjf:tabline/separator
                        'pointer 'arrow))))
    ))

(defsubst tjf:tabline/line-buttons (tabset)
  "Return a list of propertized strings for tab bar buttons.
TABSET is the tab set used to choose the appropriate buttons."
  (list
   (if tjf:tabline/home-function
       (car tjf:tabline/home-button-value)
     (cdr tjf:tabline/home-button-value))
   (if (> (tjf:tabline/start tabset) 0)
       (car tjf:tabline/scroll-left-button-value)
     (cdr tjf:tabline/scroll-left-button-value))
   (if (< (tjf:tabline/start tabset)
          (1- (length (tjf:tabline/tabs tabset))))
       (car tjf:tabline/scroll-right-button-value)
     (cdr tjf:tabline/scroll-right-button-value))
   tjf:tabline/separator-value))

(defsubst tjf:tabline/line-tab (tab)
  "Return the display representation of tab TAB.
That is, a propertized string used as an `tab-line-format' template
element.
Call `tjf:tabline/tab-label-function' to obtain a label for TAB."
  (concat (propertize
           (if tjf:tabline/tab-label-function
               (funcall tjf:tabline/tab-label-function tab)
             tab)
           'tjf:tabline/tab tab
           'local-map (tjf:tabline/make-tab-keymap tab)
           'help-echo 'tjf:tabline/help-on-tab
           'mouse-face 'tjf:tabline/highlight
           'face (cond ((and (tjf:tabline/selected-p tab (tjf:tabline/current-tabset))
                             (tjf:tabline/modified-p tab (tjf:tabline/current-tabset)))
                        'tjf:tabline/sel-mod)
                       ((tjf:tabline/selected-p tab (tjf:tabline/current-tabset))
                        'tjf:tabline/selected)
                       ((tjf:tabline/modified-p tab (tjf:tabline/current-tabset))
                        'tjf:tabline/modified)
                       (t 'tjf:tabline/unselected))
           'pointer 'hand)
          tjf:tabline/separator-value))

(defun tjf:tabline/line-format (tabset)
  "Return the `tab-line-format' value to display TABSET."
  (let* ((sel (tjf:tabline/selected-tab tabset))
         (tabs (tjf:tabline/view tabset))
         (padcolor (tjf:tabline/background-color))
         atsel elts)
    ;; Initialize buttons and separator values.
    (or tjf:tabline/separator-value
        (tjf:tabline/line-separator))
    (or tjf:tabline/home-button-value
        (tjf:tabline/line-button 'home))
    (or tjf:tabline/scroll-left-button-value
        (tjf:tabline/line-button 'scroll-left))
    (or tjf:tabline/scroll-right-button-value
        (tjf:tabline/line-button 'scroll-right))
    ;; Track the selected tab to ensure it is always visible.
    (when tjf:tabline/-track-selected
      (while (not (memq sel tabs))
        (tjf:tabline/scroll tabset -1)
        (setq tabs (tjf:tabline/view tabset)))
      (while (and tabs (not atsel))
        (setq elts  (cons (tjf:tabline/line-tab (car tabs)) elts)
              atsel (eq (car tabs) sel)
              tabs  (cdr tabs)))
      (setq elts (nreverse elts))
      ;; At this point the selected tab is the last elt in ELTS.
      ;; Scroll TABSET and ELTS until the selected tab becomes
      ;; visible.
      (with-temp-buffer
        (let ((truncate-partial-width-windows nil)
              (inhibit-modification-hooks t)
              deactivate-mark ;; Prevent deactivation of the mark!
              start)
          (setq truncate-lines nil
                buffer-undo-list t)
          (apply 'insert (tjf:tabline/line-buttons tabset))
          (setq start (point))
          (while (and (cdr elts) ;; Always show the selected tab!
                      (progn
                        (delete-region start (point-max))
                        (goto-char (point-max))
                        (apply 'insert elts)
                        (goto-char (point-min))
                        (> (vertical-motion 1) 0)))
            (tjf:tabline/scroll tabset 1)
            (setq elts (cdr elts)))))
      (setq elts (nreverse elts))
      (setq tjf:tabline/-track-selected nil))
    ;; Format remaining tabs.
    (while tabs
      (setq elts (cons (tjf:tabline/line-tab (car tabs)) elts)
            tabs (cdr tabs)))
    ;; Cache and return the new tab bar.
    (tjf:tabline/set-template
     tabset
     (list (tjf:tabline/line-buttons tabset)
           (nreverse elts)
           (propertize "%-"
                       'face (list :background padcolor
                                   :foreground padcolor)
                       'pointer 'arrow)))
    ))

(defun tjf:tabline/line ()
  "Return the tab-line templates that represent the tab bar.
Inhibit display of the tab bar in current window if any of the
`tjf:tabline/inhibit-functions' return non-nil."
  (cond
   ((run-hook-with-args-until-success 'tjf:tabline/inhibit-functions)
    ;; Don't show the tab bar.
    (setq tab-line-format nil))
   ((tjf:tabline/current-tabset t)
    ;; When available, use a cached tab bar value, else recompute it.
    (or (tjf:tabline/template tjf:tabline/current-tabset)
        (tjf:tabline/line-format tjf:tabline/current-tabset)))))

(defconst tjf:tabline/tab-line-format '(:eval (tjf:tabline/line))
  "The tab bar tab-line format.")

(defun tjf:tabline/default-inhibit-function ()
  "Inhibit display of the tab bar in specified windows.
That is dedicated windows, and `checkdoc' status windows."
  (or (window-dedicated-p (selected-window))
      (member (buffer-name)
              (list " *Checkdoc Status*"
                    (if (boundp 'ispell-choices-buffer)
                        ispell-choices-buffer
                      "*Choices*")))))

;;; Cyclic navigation through tabs
;;
(defun tjf:tabline/cycle (&optional backward type)
  "Cycle to the next available tab.
The scope of the cyclic navigation through tabs is specified by the
option `tjf:tabline/cycle-scope'.
If optional argument BACKWARD is non-nil, cycle to the previous tab
instead.
Optional argument TYPE is a mouse event type (see the function
`tjf:tabline/make-mouse-event' for details)."
  (let* ((tabset (tjf:tabline/current-tabset t))
         (ttabset (tjf:tabline/get-tabsets-tabset))
         ;; If navigation through groups is requested, and there is
         ;; only one group, navigate through visible tabs.
         (cycle (if (and (eq tjf:tabline/cycle-scope 'groups)
                         (not (cdr (tjf:tabline/tabs ttabset))))
                    'tabs
                  tjf:tabline/cycle-scope))
         selected tab)
    (when tabset
      (setq selected (tjf:tabline/selected-tab tabset))
      (cond
       ;; Cycle through visible tabs only.
       ((eq cycle 'tabs)
        (setq tab (tjf:tabline/tab-next tabset selected backward))
        ;; When there is no tab after/before the selected one, cycle
        ;; to the first/last visible tab.
        (unless tab
          (setq tabset (tjf:tabline/tabs tabset)
                tab (car (if backward (last tabset) tabset))))
        )
       ;; Cycle through tab groups only.
       ((eq cycle 'groups)
        (setq tab (tjf:tabline/tab-next ttabset selected backward))
        ;; When there is no group after/before the selected one, cycle
        ;; to the first/last available group.
        (unless tab
          (setq tabset (tjf:tabline/tabs ttabset)
                tab (car (if backward (last tabset) tabset))))
        )
       (t
        ;; Cycle through visible tabs then tab groups.
        (setq tab (tjf:tabline/tab-next tabset selected backward))
        ;; When there is no visible tab after/before the selected one,
        ;; cycle to the next/previous available group.
        (unless tab
          (setq tab (tjf:tabline/tab-next ttabset selected backward))
          ;; When there is no next/previous group, cycle to the
          ;; first/last available group.
          (unless tab
            (setq tabset (tjf:tabline/tabs ttabset)
                  tab (car (if backward (last tabset) tabset))))
          ;; Select the first/last visible tab of the new group.
          (setq tabset (tjf:tabline/tabs (tjf:tabline/tab-tabset tab))
                tab (car (if backward (last tabset) tabset))))
        ))
      (tjf:tabline/click-on-tab tab type))))

;;;###autoload
(defun tjf:tabline/backward ()
  "Select the previous available tab.
Depend on the setting of the option `tjf:tabline/cycle-scope'."
  (interactive)
  (tjf:tabline/cycle t))

;;;###autoload
(defun tjf:tabline/forward ()
  "Select the next available tab.
Depend on the setting of the option `tjf:tabline/cycle-scope'."
  (interactive)
  (tjf:tabline/cycle))

;;;###autoload
(defun tjf:tabline/backward-group ()
  "Go to selected tab in the previous available group."
  (interactive)
  (let ((tjf:tabline/cycle-scope 'groups))
    (tjf:tabline/cycle t)))

;;;###autoload
(defun tjf:tabline/forward-group ()
  "Go to selected tab in the next available group."
  (interactive)
  (let ((tjf:tabline/cycle-scope 'groups))
    (tjf:tabline/cycle)))

;;;###autoload
(defun tjf:tabline/backward-tab ()
  "Select the previous visible tab."
  (interactive)
  (let ((tjf:tabline/cycle-scope 'tabs))
    (tjf:tabline/cycle t)))

;;;###autoload
(defun tjf:tabline/forward-tab ()
  "Select the next visible tab."
  (interactive)
  (let ((tjf:tabline/cycle-scope 'tabs))
    (tjf:tabline/cycle)))

;;; Button press commands
;;
(defsubst tjf:tabline/-mouse (number)
  "Return a mouse button symbol from NUMBER.
That is mouse-2, or mouse-3 when NUMBER is respectively 2, or 3.
Return mouse-1 otherwise."
  (cond ((eq number 2) 'mouse-2)
        ((eq number 3) 'mouse-3)
        ('mouse-1)))

;;;###autoload
(defun tjf:tabline/press-home (&optional arg)
  "Press the tab bar home button.
That is, simulate a mouse click on that button.
A numeric prefix ARG value of 2, or 3, respectively simulates a
mouse-2, or mouse-3 click.  The default is a mouse-1 click."
  (interactive "p")
  (tjf:tabline/click-on-button 'home (tjf:tabline/-mouse arg)))

;;;###autoload
(defun tjf:tabline/press-scroll-left (&optional arg)
  "Press the tab bar scroll-left button.
That is, simulate a mouse click on that button.
A numeric prefix ARG value of 2, or 3, respectively simulates a
mouse-2, or mouse-3 click.  The default is a mouse-1 click."
  (interactive "p")
  (tjf:tabline/click-on-button 'scroll-left (tjf:tabline/-mouse arg)))

;;;###autoload
(defun tjf:tabline/press-scroll-right (&optional arg)
  "Press the tab bar scroll-right button.
That is, simulate a mouse click on that button.
A numeric prefix ARG value of 2, or 3, respectively simulates a
mouse-2, or mouse-3 click.  The default is a mouse-1 click."
  (interactive "p")
  (tjf:tabline/click-on-button 'scroll-right (tjf:tabline/-mouse arg)))

;;; Mouse-wheel support
;;
(require 'mwheel)

;;; Compatibility
;;
(defconst tjf:tabline/-mwheel-up-event
  (symbol-value (if (boundp 'mouse-wheel-up-event)
                    'mouse-wheel-up-event
                  'mouse-wheel-up-button)))

(defconst tjf:tabline/-mwheel-down-event
  (symbol-value (if (boundp 'mouse-wheel-down-event)
                    'mouse-wheel-down-event
                  'mouse-wheel-down-button)))

(defsubst tjf:tabline/-mwheel-key (event-type)
  "Return a mouse wheel key symbol from EVENT-TYPE.
When EVENT-TYPE is a symbol return it.
When it is a button number, return symbol `mouse-<EVENT-TYPE>'."
  (if (symbolp event-type)
      event-type
    (intern (format "mouse-%s" event-type))))

(defsubst tjf:tabline/-mwheel-up-p (event)
  "Return non-nil if EVENT is a mouse-wheel up event."
  (let ((x (event-basic-type event)))
    (if (eq 'mouse-wheel x)
        (< (car (cdr (cdr event))) 0)   ;; Emacs 21.3
      ;; Emacs > 21.3
      (eq x tjf:tabline/-mwheel-up-event))))

;;; Basic commands
;;
;;;###autoload
(defun tjf:tabline/mwheel-backward (event)
  "Select the previous available tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tjf:tabline/backward'."
  (interactive "@e")
  (tjf:tabline/cycle t event))

;;;###autoload
(defun tjf:tabline/mwheel-forward (event)
  "Select the next available tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tjf:tabline/forward'."
  (interactive "@e")
  (tjf:tabline/cycle nil event))

;;;###autoload
(defun tjf:tabline/mwheel-backward-group (event)
  "Go to selected tab in the previous available group.
If there is only one group, select the previous visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tjf:tabline/backward-group'."
  (interactive "@e")
  (let ((tjf:tabline/cycle-scope 'groups))
    (tjf:tabline/cycle t event)))

;;;###autoload
(defun tjf:tabline/mwheel-forward-group (event)
  "Go to selected tab in the next available group.
If there is only one group, select the next visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tjf:tabline/forward-group'."
  (interactive "@e")
  (let ((tjf:tabline/cycle-scope 'groups))
    (tjf:tabline/cycle nil event)))

;;;###autoload
(defun tjf:tabline/mwheel-backward-tab (event)
  "Select the previous visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tjf:tabline/backward-tab'."
  (interactive "@e")
  (let ((tjf:tabline/cycle-scope 'tabs))
    (tjf:tabline/cycle t event)))

;;;###autoload
(defun tjf:tabline/mwheel-forward-tab (event)
  "Select the next visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tjf:tabline/forward-tab'."
  (interactive "@e")
  (let ((tjf:tabline/cycle-scope 'tabs))
    (tjf:tabline/cycle nil event)))

;;; Wrappers when there is only one generic mouse-wheel event
;;
;;;###autoload
(defun tjf:tabline/mwheel-switch-tab (event)
  "Select the next or previous tab according to EVENT."
  (interactive "@e")
  (if (tjf:tabline/-mwheel-up-p event)
      (tjf:tabline/mwheel-forward-tab event)
    (tjf:tabline/mwheel-backward-tab event)))

;;;###autoload
(defun tjf:tabline/mwheel-switch-group (event)
  "Select the next or previous group of tabs according to EVENT."
  (interactive "@e")
  (if (tjf:tabline/-mwheel-up-p event)
      (tjf:tabline/mwheel-forward-group event)
    (tjf:tabline/mwheel-backward-group event)))

;;; Minor modes
;;
(defsubst tjf:tabline/mode-on-p ()
  "Return non-nil if Tabbar mode is on."
  (eq (default-value 'tab-line-format)
      tjf:tabline/tab-line-format))

;;; Tjf:Tabline/Local mode
;;
(defvar tjf:tabline/-local-hlf nil)

;;;###autoload
(define-minor-mode tjf:tabline/local-mode
  "Toggle local display of the tab bar.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.
When turned on, if a local tab-line is shown, it is hidden to show
the tab bar.  The tab bar is locally hidden otherwise.  When turned
off, if a local tab-line is hidden or the tab bar is locally
hidden, it is shown again.  Signal an error if Tabbar mode is off."
  :group 'tjf-tabline
  :global nil
  (unless (tjf:tabline/mode-on-p)
    (error "Tabbar mode must be enabled"))
;;; ON
  (if tjf:tabline/local-mode
      (if (and (local-variable-p 'tab-line-format)
               tab-line-format)
          ;; A local tab-line exists, hide it to show the tab bar.
          (progn
            ;; Fail in case of an inconsistency because another local
            ;; tab-line is already hidden.
            (when (local-variable-p 'tjf:tabline/-local-hlf)
              (error "Another local tab-line is already hidden"))
            (set (make-local-variable 'tjf:tabline/-local-hlf)
                 tab-line-format)
            (kill-local-variable 'tab-line-format))
        ;; Otherwise hide the tab bar in this buffer.
        (setq tab-line-format nil))
;;; OFF
    (if (local-variable-p 'tjf:tabline/-local-hlf)
        ;; A local tab-line is hidden, show it again.
        (progn
          (setq tab-line-format tjf:tabline/-local-hlf)
          (kill-local-variable 'tjf:tabline/-local-hlf))
      ;; The tab bar is locally hidden, show it again.
      (kill-local-variable 'tab-line-format))))

;;; Tabbar mode
;;
(defvar tjf:tabline/prefix-key [(control ?c)]
  "The common prefix key used in Tabbar mode.")

(defvar tjf:tabline/prefix-map
  (let ((km (make-sparse-keymap)))
    (define-key km [(control home)]  'tjf:tabline/press-home)
    (define-key km [(control left)]  'tjf:tabline/backward)
    (define-key km [(control right)] 'tjf:tabline/forward)
    (define-key km [(control up)]    'tjf:tabline/backward-group)
    (define-key km [(control down)]  'tjf:tabline/forward-group)
    (define-key km [(control prior)] 'tjf:tabline/press-scroll-left)
    (define-key km [(control next)]  'tjf:tabline/press-scroll-right)
    (define-key km [(control f10)]   'tjf:tabline/local-mode)
    km)
  "The key bindings provided in Tabbar mode.")

(defvar tjf:tabline/mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km tjf:tabline/prefix-key tjf:tabline/prefix-map)
    km)
  "Keymap to use in  Tabbar mode.")

(defvar tjf:tabline/-global-hlf nil)

;;;###autoload
(define-minor-mode tjf:tabline/mode
  "Toggle display of a tab bar in the tab-line.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\\{tjf:tabline/mode-map}"
  :group 'tjf-tabline
  :require 'tabbar
  :global t
  :keymap tjf:tabline/mode-map
  (if tjf:tabline/mode
;;; ON
      (unless (tjf:tabline/mode-on-p)
        ;; Save current default value of `tab-line-format'.
        (setq tjf:tabline/-global-hlf (default-value 'tab-line-format))
        (tjf:tabline/init-tabsets-store)
        (setq-default tab-line-format tjf:tabline/tab-line-format)
	(if (fboundp 'tjf:tabline/define-access-keys) (tjf:tabline/define-access-keys)))
;;; OFF
    (when (tjf:tabline/mode-on-p)
      ;; Turn off Tjf:Tabline/Local mode globally.
      (mapc #'(lambda (b)
                (condition-case nil
                    (with-current-buffer b
                      (and tjf:tabline/local-mode
                           (tjf:tabline/local-mode -1)))
                  (error nil)))
            (buffer-list))
      ;; Restore previous `tab-line-format'.
      (setq-default tab-line-format tjf:tabline/-global-hlf)
      (tjf:tabline/free-tabsets-store))
    ))

;;; Tjf:Tabline/Mwheel mode
;;
(defvar tjf:tabline/mwheel-mode-map
  (let ((km (make-sparse-keymap)))
    (if (get 'mouse-wheel 'event-symbol-elements)
        ;; Use one generic mouse wheel event
        (define-key km [A-mouse-wheel]
          'tjf:tabline/mwheel-switch-group)
      ;; Use separate up/down mouse wheel events
      (let ((up   (tjf:tabline/-mwheel-key tjf:tabline/-mwheel-up-event))
            (down (tjf:tabline/-mwheel-key tjf:tabline/-mwheel-down-event)))
        (define-key km `[tab-line ,down]
          'tjf:tabline/mwheel-backward-group)
        (define-key km `[tab-line ,up]
          'tjf:tabline/mwheel-forward-group)
        (define-key km `[tab-line (control ,down)]
          'tjf:tabline/mwheel-backward-tab)
        (define-key km `[tab-line (control ,up)]
          'tjf:tabline/mwheel-forward-tab)
        (define-key km `[tab-line (shift ,down)]
          'tjf:tabline/mwheel-backward)
        (define-key km `[tab-line (shift ,up)]
          'tjf:tabline/mwheel-forward)
        ))
    km)
  "Keymap to use in Tjf:Tabline/Mwheel mode.")

;;;###autoload
(define-minor-mode tjf:tabline/mwheel-mode
  "Toggle use of the mouse wheel to navigate through tabs or groups.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\\{tjf:tabline/mwheel-mode-map}"
  :group 'tjf-tabline
  :require 'tabbar
  :global t
  :keymap tjf:tabline/mwheel-mode-map
  (when tjf:tabline/mwheel-mode
    (unless (and (boundp 'mouse-wheel-mode)
                 mouse-wheel-mode
                 tjf:tabline/mode)
      (tjf:tabline/mwheel-mode -1))))

(defun tjf:tabline/mwheel-follow ()
  "Toggle Tjf:Tabline/Mwheel following Tabbar and Mouse-Wheel modes."
  (if (boundp 'mouse-wheel-mode)
      (tjf:tabline/mwheel-mode (if (and mouse-wheel-mode tjf:tabline/mode) 1 -1))))

(add-hook 'tjf:tabline/mode-hook      'tjf:tabline/mwheel-follow)
(add-hook 'mouse-wheel-mode-hook 'tjf:tabline/mwheel-follow)

;;; Buffer tabs
;;
(defgroup tjf:tabline/buffer nil
  "Display buffers in the tab bar."
  :group 'tjf-tabline)

(defcustom tjf:tabline/buffer-home-button
  (cons (cons "[+]" tjf:tabline/home-button-enabled-image)
        (cons "[-]" tjf:tabline/home-button-disabled-image))
  "The home button displayed when showing buffer tabs.
The enabled button value is displayed when showing tabs for groups of
buffers, and the disabled button value is displayed when showing
buffer tabs.
The variable `tjf:tabline/button-widget' gives details on this widget."
  :group 'tjf-tabline
  :type tjf:tabline/button-widget
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of button value.
          (setq tjf:tabline/home-button-value nil)))

(defvar tjf:tabline/buffer-list-function 'tjf:tabline/buffer-list
  "Function that returns the list of buffers to show in tabs.
That function is called with no arguments and must return a list of
buffers.")

(defvar tjf:tabline/buffer-groups-function 'tjf:tabline/buffer-groups
  "Function that gives the group names the current buffer belongs to.
It must return a list of group names, or nil if the buffer has no
group.  Notice that it is better that a buffer belongs to one group.")

(defun tjf:tabline/buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space, when they are not
visiting a file.  The current buffer is always included."
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) b) b)
                     ((buffer-file-name b) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                     ((buffer-live-p b) b)))
                (buffer-list))))

(defun tjf:tabline/buffer-mode-derived-p (mode parents)
  "Return non-nil if MODE derives from a mode in PARENTS."
  (let (derived)
    (while (and (not derived) mode)
      (if (memq mode parents)
          (setq derived t)
        (setq mode (get mode 'derived-mode-parent))))
    derived))

(defun tjf:tabline/buffer-groups ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (list
   (cond
    ((or (get-buffer-process (current-buffer))
         ;; Check if the major mode derives from `comint-mode' or
         ;; `compilation-mode'.
         (tjf:tabline/buffer-mode-derived-p
          major-mode '(comint-mode compilation-mode)))
     "Process"
     )
    ((member (buffer-name)
             '("*scratch*" "*Messages*"))
     "Common"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    ((memq major-mode
           '(help-mode apropos-mode Info-mode Man-mode))
     "Help"
     )
    ((memq major-mode
           '(rmail-mode
             rmail-edit-mode vm-summary-mode vm-mode mail-mode
             mh-letter-mode mh-show-mode mh-folder-mode
             gnus-summary-mode message-mode gnus-group-mode
             gnus-article-mode score-mode gnus-browse-killed-mode))
     "Mail"
     )
    (t
     ;; Return `mode-name' if not blank, `major-mode' otherwise.
     (if (and (stringp mode-name)
              ;; Take care of preserving the match-data because this
              ;; function is called when updating the tab-line.
              (save-match-data (string-match "[^ ]" mode-name)))
         mode-name
       (symbol-name major-mode))
     ))))

;;; Group buffers in tab sets.
;;
(defvar tjf:tabline/-buffers nil)

(defun tjf:tabline/buffer-update-groups ()
  "Update tab sets from groups of existing buffers.
Return the the first group where the current buffer is."
  (let ((bl (sort
             (mapcar
	      ;; for each buffer, create list: buffer, buffer name, groups-list
	      ;; sort on buffer name; store to bl (buffer list)
              #'(lambda (b)
                  (with-current-buffer b
                    (list (current-buffer)
                          (buffer-name)
                          (if tjf:tabline/buffer-groups-function
                              (funcall tjf:tabline/buffer-groups-function)
                            '("Common")))))
              (and tjf:tabline/buffer-list-function
                   (funcall tjf:tabline/buffer-list-function)))
             #'(lambda (e1 e2)
                 (string-lessp (nth 1 e1) (nth 1 e2))))))
    ;; If the cache has changed, update the tab sets.
    (unless (equal bl tjf:tabline/-buffers)
      ;; Add new buffers, or update changed ones.
      (dolist (e bl) ;; loop through buffer list
        (dolist (g (nth 2 e)) ;; for each member of groups-list for current buffer
          (let ((tabset (tjf:tabline/get-tabset g))) ;; get group from group name
            (if tabset ;; if group exists
		;; check if current buffer is same as any cached buffer
		;; (search buffer list for matching buffer)
                (unless (equal e (assq (car e) tjf:tabline/-buffers)) ;; if not,...
                  ;; This is a new buffer, or a previously existing
                  ;; buffer that has been renamed, or moved to another
                  ;; group.  Update the tab set, and the display.
                  (tjf:tabline/add-tab tabset (car e) t) ;; add to end of tabset
                  (tjf:tabline/set-template tabset nil))
	      ;;if tabset doesn't exist, make a new tabset with this buffer
              (tjf:tabline/make-tabset g (car e))))))
      ;; Remove tabs for buffers not found in cache or moved to other
      ;; groups, and remove empty tabsets.
      (mapc 'tjf:tabline/delete-tabset ;; delete each tabset named in following list:
            (tjf:tabline/map-tabsets ;; apply following function to each tabset:
             #'(lambda (tabset)
                 (dolist (tab (tjf:tabline/tabs tabset)) ;; for each tab in tabset
                   (let ((e (assq (tjf:tabline/tab-value tab) bl))) ;; get buffer
                     (or (and e (memq tabset ;; skip if buffer exists and tabset is a member of groups-list for this buffer
                                      (mapcar 'tjf:tabline/get-tabset
                                              (nth 2 e))))
                         (tjf:tabline/delete-tab tab)))) ;; else remove tab from this set
                 ;; Return empty tab sets
                 (unless (tjf:tabline/tabs tabset)
                   tabset)))) ;; return list of tabsets, replacing non-empties with nil
      ;; The new cache becomes the current one.
      (setq tjf:tabline/-buffers bl)))
  ;; Return the first group the current buffer belongs to.
  (car (nth 2 (assq (current-buffer) tjf:tabline/-buffers))))

;;; Tab bar callbacks
;;
(defvar tjf:tabline/buffer-show-groups nil)

(defsubst tjf:tabline/buffer-show-groups (flag)
  "Set display of tabs for groups of buffers to FLAG."
  (setq tjf:tabline/buffer-show-groups flag
        ;; Redisplay the home button.
        tjf:tabline/home-button-value nil))

(defun tjf:tabline/buffer-tabs ()
  "Return the buffers to display on the tab bar, in a tab set."
  (let ((tabset (tjf:tabline/get-tabset (tjf:tabline/buffer-update-groups))))
    (tjf:tabline/select-tab-value (current-buffer) tabset)
    (when tjf:tabline/buffer-show-groups
      (setq tabset (tjf:tabline/get-tabsets-tabset))
      (tjf:tabline/select-tab-value (current-buffer) tabset))
    tabset))

(defun tjf:tabline/buffer-button-label (name)
  "Return a label for button NAME.
That is a pair (ENABLED . DISABLED), where ENABLED and DISABLED are
respectively the appearance of the button when enabled and disabled.
They are propertized strings which could display images, as specified
by the variable `tjf:tabline/button-label'.
When NAME is 'home, return a different ENABLED button if showing tabs
or groups.  Call the function `tjf:tabline/button-label' otherwise."
  (let ((lab (tjf:tabline/button-label name)))
    (when (eq name 'home)
      (let* ((btn tjf:tabline/buffer-home-button)
             (on  (tjf:tabline/find-image (cdar btn)))
             (off (tjf:tabline/find-image (cddr btn))))
        ;; When `tjf:tabline/buffer-home-button' does not provide a value,
        ;; default to the enabled value of `tjf:tabline/home-button'.
        (if on
            (tjf:tabline/normalize-image on 1)
          (setq on (get-text-property 0 'display (car lab))))
        (if off
            (tjf:tabline/normalize-image off 1)
          (setq off (get-text-property 0 'display (car lab))))
        (setcar lab
                (if tjf:tabline/buffer-show-groups
                    (propertize (or (caar btn) (car lab)) 'display on)
                  (propertize (or (cadr btn) (car lab)) 'display off)))
        ))
    lab))

(defun tjf:tabline/buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tjf:tabline/buffer-show-groups
                    (format "[%s]" (tjf:tabline/tab-tabset tab))
                  (format "%s" (tjf:tabline/tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tjf:tabline/auto-scroll-flag
        label
      (tjf:tabline/shorten
       label (max 1 (/ (window-width)
                       (length (tjf:tabline/view
                                (tjf:tabline/current-tabset)))))))))

(defun tjf:tabline/buffer-help-on-tab (tab)
  "Return the help string shown when mouse is onto TAB."
  (if tjf:tabline/buffer-show-groups
      (let* ((tabset (tjf:tabline/tab-tabset tab))
             (tab (tjf:tabline/selected-tab tabset)))
        (format "mouse-1: switch to buffer %S in group [%s]"
                (buffer-name (tjf:tabline/tab-value tab)) tabset))
    (format "mouse-1: switch to buffer %S\n\
mouse-2: pop to buffer, mouse-3: delete other windows"
            (buffer-name (tjf:tabline/tab-value tab)))
    ))

(defun tjf:tabline/buffer-select-tab (event tab)
  "On mouse EVENT, select TAB."
  (let ((mouse-button (event-basic-type event))
        (buffer (tjf:tabline/tab-value tab)))
    (cond
     ((eq mouse-button 'mouse-2)
      (pop-to-buffer buffer t))
     ((eq mouse-button 'mouse-3)
      (delete-other-windows))
     (t
      (switch-to-buffer buffer)))
    ;; Don't show groups.
    (tjf:tabline/buffer-show-groups nil)
    ))

(defun tjf:tabline/buffer-click-on-home (event)
  "Handle a mouse click EVENT on the tab bar home button.
mouse-1, toggle the display of tabs for groups of buffers.
mouse-3, close the current buffer."
  (let ((mouse-button (event-basic-type event)))
    (cond
     ((eq mouse-button 'mouse-1)
      (tjf:tabline/buffer-show-groups (not tjf:tabline/buffer-show-groups)))
     ((eq mouse-button 'mouse-3)
      (kill-buffer nil))
     )))

(defun tjf:tabline/buffer-help-on-home ()
  "Return the help string shown when mouse is onto the toggle button."
  (concat
   (if tjf:tabline/buffer-show-groups
       "mouse-1: show buffers in selected group"
     "mouse-1: show groups of buffers")
   ", mouse-3: close current buffer"))

(defun tjf:tabline/buffer-track-killed ()
  "Hook run just before actually killing a buffer.
In Tabbar mode, try to switch to a buffer in the current tab bar,
after the current buffer has been killed.  Try first the buffer in tab
after the current one, then the buffer in tab before.  On success, put
the sibling buffer in front of the buffer list, so it will be selected
first."
  (and (eq tab-line-format tjf:tabline/tab-line-format)
       (eq tjf:tabline/current-tabset-function 'tjf:tabline/buffer-tabs)
       (eq (current-buffer) (window-buffer (selected-window)))
       (let ((bl (tjf:tabline/tab-values (tjf:tabline/current-tabset)))
             (b  (current-buffer))
             found sibling)
         (while (and bl (not found))
           (if (eq b (car bl))
               (setq found t)
             (setq sibling (car bl)))
           (setq bl (cdr bl)))
         (when (and (setq sibling (or (car bl) sibling))
                    (buffer-live-p sibling))
           ;; Move sibling buffer in front of the buffer list.
           (save-current-buffer
             (switch-to-buffer sibling))))))

;;; Tab bar buffer setup
;;
(defun tjf:tabline/buffer-init ()
  "Initialize tab bar buffer data.
Run as `tjf:tabline/init-hook'."
  (setq tjf:tabline/buffers nil
        tjf:tabline/buffer-show-groups nil
        tjf:tabline/current-tabset-function 'tjf:tabline/buffer-tabs
        tjf:tabline/tab-label-function 'tjf:tabline/buffer-tab-label
        tjf:tabline/select-tab-function 'tjf:tabline/buffer-select-tab
        tjf:tabline/help-on-tab-function 'tjf:tabline/buffer-help-on-tab
        tjf:tabline/button-label-function 'tjf:tabline/buffer-button-label
        tjf:tabline/home-function 'tjf:tabline/buffer-click-on-home
        tjf:tabline/home-help-function 'tjf:tabline/buffer-help-on-home
        )
  (add-hook 'kill-buffer-hook 'tjf:tabline/buffer-track-killed))

(defun tjf:tabline/buffer-quit ()
  "Quit tab bar buffer.
Run as `tjf:tabline/quit-hook'."
  (setq tjf:tabline/buffers nil
        tjf:tabline/buffer-show-groups nil
        tjf:tabline/current-tabset-function nil
        tjf:tabline/tab-label-function nil
        tjf:tabline/select-tab-function nil
        tjf:tabline/help-on-tab-function nil
        tjf:tabline/button-label-function nil
        tjf:tabline/home-function nil
        tjf:tabline/home-help-function nil
        )
  (remove-hook 'kill-buffer-hook 'tjf:tabline/buffer-track-killed))

(add-hook 'tjf:tabline/init-hook 'tjf:tabline/buffer-init)
(add-hook 'tjf:tabline/quit-hook 'tjf:tabline/buffer-quit)

(provide 'tjf-tabline)

(run-hooks 'tjf:tabline/load-hook)

;;
(message "Loading tjf-tabline...done")
(provide 'tjf-tabline)

;;; tjf-tabline.el ends here
