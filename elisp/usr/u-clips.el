;;; u-clips.el --- CLIPS major mode support for GNU Emacs -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2021 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   11-Apr-2016

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

;; Revision: 14-Aug-2016 Added `u-clips-menu'
;;           15-Aug-2016 Added `u-clips-font-lock-keywords-1' and `u-clips-font-lock-keywords-2'
;;           17-Aug-2016 Set `comment-start' in `clips-setup'
;;           18-Sep-2016 Expanded syntax highlighting
;;

;;; Code:

(message "Loading u-clips...")
(require 'clips-mode)
;;
(defface clips-constant-face '((t (:foreground "orange")))
  "CLIPS constant face."
  :group 'font-lock-faces)
(defvar clips-constant-face (make-face 'clips-constant-face))

(defface clips-control-face '((t (:foreground "cyan4" :weight bold)))
  "CLIPS control face."
  :group 'font-lock-faces)
(defvar clips-control-face (make-face 'clips-control-face))

(defface clips-declaration-face '((t (:foreground "gray40" :weight bold)))
  "CLIPS declaration face."
  :group 'font-lock-faces)
(defvar clips-declaration-face (make-face 'clips-declaration-face))

(defface clips-function-face '((t (:foreground "pink" :weight bold)))
  "CLIPS function face."
  :group 'font-lock-faces)
(defvar clips-function-face (make-face 'clips-function-face))

(defface clips-global-variable-face '((t (:foreground "navy")))
  "CLIPS global variable face."
  :group 'font-lock-faces)
(defvar clips-global-variable-face (make-face 'clips-global-variable-face))

(defface clips-logical-face '((t (:foreground "brown4" :weight bold)))
  "CLIPS logical face."
  :group 'font-lock-faces)
(defvar clips-logical-face (make-face 'clips-logical-face))

(defface clips-object-match-face '((t (:foreground "purple" :weight bold)))
  "CLIPS global variable face."
  :group 'font-lock-faces)
(defvar clips-object-match-face (make-face 'clips-object-match-face))

(defface clips-variable-face '((t (:foreground "royalblue")))
  "CLIPS variable face."
  :group 'font-lock-faces)
(defvar clips-variable-face (make-face 'clips-variable-face))

(defface clips-verb-face '((t(:foreground "green4" :weight bold) ))
  "CLIPS verb face."
  :group 'font-lock-faces)
(defvar clips-verb-face (make-face 'clips-verb-face))

;;
(defconst clips-font-lock-constants
  (eval-when-compile
    (let ((clips-constants (regexp-opt '("crlf"
                                         "FALSE"
                                         "nil"
                                         "TRUE"))))
      (list
       (cons (concat "\\(" clips-constants "\\)") 'clips-constant-face))))
  "Constant definitions to highlight in CLIPS mode.")

(defconst clips-font-lock-control
  (eval-when-compile
    (let ((clips-control (regexp-opt '("case"
                                       "do-for-all-facts" "do-for-all-instances" "do-for-instance"
                                       "else"
                                       "find-all-facts" "find-all-instances" "find-fact" "find-instance"
                                       "if"
                                       "loop-for-count"
                                       "progn"
                                       "switch"
                                       "then"
                                       "while"))))
      (list
       (cons (concat "\\<\\(" clips-control "\\)\\>") 'clips-control-face))))
  "Control structure definitions to highlight in CLIPS mode.")

(defconst clips-font-lock-declaration
  (eval-when-compile
    (let ((clips-declaration (regexp-opt '("allowed-classes" "allowed-symbols"
                                           "default"
                                           "message-handler" "multislot"
                                           "pattern-match"
                                           "role"
                                           "salience" "slot"
                                           "type"))))
      (list
       (cons (concat "\\<\\(" clips-declaration "\\)\\>") 'clips-declaration-face))))
  "Declaration definitions to highlight in CLIPS mode.")

(defconst clips-font-lock-functions
  (eval-when-compile
    (let ((clips-functions (regexp-opt '("abs"
                                         "div"
                                         "fact-slot-value"  "first"
                                         "length"
                                         "max" "min" "mod"
                                         "nth"
                                         "rest"
                                         "str-cat" "sym-cat"))))
      (list
       (cons (concat "\\<\\(" clips-functions "\\)\\>") 'clips-function-face))))
  "Function definitions to highlight in CLIPS mode.")

(defconst clips-font-lock-logical
  (eval-when-compile
    (let ((clips-logical (regexp-opt '("and" "any-instancep"
                                       "evenp" "exists" "eq"
                                       "instance-address" "instance-addressp" "instance-existp" "instance-name" "instance-namep" "instancep"
                                       "member"
                                       "neq" "not" "numberp"
                                       "oddp" "or"
                                       "subclassp"
                                       "test"))))
      (list
       (cons (concat "\\<\\(" clips-logical "\\)\\>") 'clips-logical-face))))
  "Logic function definitions to highlight in CLIPS mode.")


(defconst clips-font-lock-verbs
  (eval-when-compile
    (let ((clips-verbs (regexp-opt '("agenda" "assert"
                                     "batch" "bind" "break"
                                     "clear-focus-stack" "close" "create"
                                     "declare" "delete" "duplicate-instance" "dynamic-put"
                                     "eval" "exit" "export"
                                     "focus" "format" "funcall"
                                     "get"
                                     "halt"
                                     "import" "insert"
                                     "load"
                                     "make-instance" "modify"
                                     "open"
                                     "printout" "put"
                                     "reset" "retract" "return" "run"
                                     "send" "set-current-module" "slot-direct-insert" "slot-insert"
                                     "throw"
                                     "unmake-instance" "unwatch"
                                     "watch"))))
      (list
       (cons (concat "\\<\\(" clips-verbs "\\)\\>") 'clips-verb-face))))
  "Verb definitions to highlight in CLIPS mode.")

(defconst clips-font-lock-constructs
  (eval-when-compile
    (let ((clips-constructs
           (regexp-opt
            '("defclass" "defconstant" "deffunction" "defglobal" "definstances"
              "defmessage-handler" "defmodule" "defrule" "deftemplate")))
          (clips-identifier
           (let ((letter "a-zA-Z_$\-\300-\326\330-\366\370-\377")
                 (digit "0-9"))
             (concat "\\<\\([" letter "][" letter digit ":]*\\)\\>"))))
      (list
       (cons (concat "\\<" clips-constructs "\\>\\s *" clips-identifier)
             `(,(+ 1 (regexp-opt-depth clips-constructs)) font-lock-function-name-face))
       (cons (concat "\\<\\(" clips-constructs "\\)\\>") 'font-lock-keyword-face))))
  "Construct definitions to highlight in CLIPS mode.")

;; (defconst clips-font-lock-builtins
;;   (eval-when-compile
;;     (let ((clips-builtins
;;            (regexp-opt
;;             '(

;;               )))
;;             (clips-connective-constraints (regexp-opt '("|" "&"))))
;;            (list
;;             (cons (concat "\\<\\(" clips-builtins "\\)\\>")               'font-lock-builtin-face)
;;             (cons (concat "\\<\\(" clips-connective-constraints "\\)\\>") 'font-lock-builtin-face))))
;;   "Built-ins to highlight in CLIPS modes.")

(defconst clips-font-lock-variables
  (eval-when-compile
    (list
     (cons "\\([?][^&* :]+\\)" 'clips-variable-face)))
  "CLIPS mode font-lock definitions for variables.")

(defconst clips-font-lock-global-variables
  (eval-when-compile
    (list
     (cons "\\([?][*][^* ]+[*]\\)" 'clips-global-variable-face)))
  "CLIPS mode font-lock definitions for global variables.")

(defconst u-clips-font-lock-object-match
  (eval-when-compile
    (let ((clips-identifier
           (let ((letter "a-zA-Z_$\-\300-\326\330-\366\370-\377")
                 (digit "0-9"))
             (concat "\\<\\([" letter "][" letter digit ":]*\\)\\>"))))
      (list
       (cons (concat "\\<" (regexp-opt '("object" "is-a")) "\\>\\s *" clips-identifier)
             `(,(+ 1 (regexp-opt-depth (regexp-opt '("is-a")))) 'clips-object-match-face))
       (cons (concat "\\<\\(" (regexp-opt '("object" "is-a")) "\\)\\>") 'clips-object-match-face)))))

(defconst u-clips-font-lock-keywords
  (append clips-font-lock-constants
          clips-font-lock-variables
          clips-font-lock-global-variables
          clips-font-lock-control
          clips-font-lock-declaration
          clips-font-lock-constructs
          clips-font-lock-functions
          clips-font-lock-logical
          clips-font-lock-verbs
          u-clips-font-lock-object-match)
  "CLIPS mode font-lock definitions.")

(defconst u-clips-imenu-generic-expression
  (list
   (list nil
         (purecopy
          (concat "^\\s-*("
                  (eval-when-compile
                    (regexp-opt '("defgeneric" "defadvice" "defmethod") t))
                  "\\s-+'?\\(\\(\\sw\\|\\s_\\)+\\)"))
         2)
   (list (purecopy "Variables/Instances")
         (purecopy
          (concat "^\\s-*("
                  (eval-when-compile
                    (regexp-opt '("defglobal" "definstance") t))
                  "\\s-+'?\\(\\(\\sw\\|\\s_\\)+\\)"))
         2)
   (list (purecopy "Types/Objects")
         (purecopy
          (concat "^\\s-*("
                  (eval-when-compile
                    (regexp-opt '("deftemplate" "defclass") t))
                  "\\s-+'?\\(\\(\\sw\\|\\s_\\)+\\)"))
         2)
   (list (purecopy "Functions")
         (purecopy
          (concat "^\\s-*("
                  (eval-when-compile
                    (regexp-opt '("deffunction") t))
                  "\\s-+'?\\(\\(\\sw\\|\\s_\\)+\\)"))
         2)
   (list (purecopy "Rules")
         (purecopy
          (concat "^\\s-*("
                  (eval-when-compile
                    (regexp-opt '("defrule") t))
                  "\\s-+'?\\(\\(\\sw\\|\\s_\\)+\\)"))
         2))
  "Imenu generic expression for Clips mode.  See `imenu-generic-expression'.")

(setq clips-font-lock-keywords u-clips-font-lock-keywords)
(setq clips-imenu-generic-expression u-clips-imenu-generic-expression)

(define-key clips-mode-map [menu-bar] nil)

(easy-menu-define u-clips-menu clips-mode-map "U-CLIPS"
  '("CLIPS"
    ["Beginning Of Function" beginning-of-defun :active t]
    ["End Of Function"       end-of-defun       :active t]
    ["Mark Function"         mark-defun         :active t]))

(defun clips-delete-forward-space ()
  "Delete whitespace from point to non-whitespace."
  (if (looking-at "[ \t]+")
      (delete-region (match-beginning 0) (match-end 0))))

(defun u-clips-tab ()
  "Indent line function for Clips mode."
  (interactive "*")
  (let ((ind-current-line (current-indentation))
        (ind-back-up-line nil)
        (ind-goal         nil)
        (closing-paren-p  nil)
        (opening-def-p    nil))
    (save-excursion
      (beginning-of-line)
      (forward-to-indentation 0)
      (setq closing-paren-p (looking-at ")")
            opening-def-p   (and (looking-at "(def") (not (looking-at "(default")))))
                                        ;(message "OPENING: %s CLOSING: %s" opening-def-p closing-paren-p)
    (save-excursion
      (beginning-of-line)
      (if (not opening-def-p)
          (backward-up-list))
      (setq ind-back-up-line (current-column)
            ind-goal (if opening-def-p
                         0
                       (if closing-paren-p
                           ind-back-up-line
                         (+ 4 ind-back-up-line)))))
                                        ;(message "CURRENT: %s, BACKUP: %s, GOAL: %s, CLOSING: %s" ind-current-line ind-back-up-line ind-goal closing-paren-p)
    (if (or (/= ind-current-line ind-back-up-line) (= 0 ind-back-up-line))
        (progn
          (beginning-of-line)
          (clips-delete-forward-space)
          (insert (make-string ind-goal 32))))
    ))

(make-variable-buffer-local 'highlight-operators-regexp)

(defun clips-setup ()
  "Set up CLIPS major mode."
  ;; (setq-local indent-line-function 'u-clips-tab)
  (setq-local indent-region-function nil)
  (setq-local comment-start ";; ")
  (setq highlight-operators-regexp (regexp-opt '("=>" "<-" "&:")))
  (semantic-mode t)
  (imenu-add-to-menubar "Navigate"))

(add-hook 'clips-mode-hook 'clips-setup)
;;
(message "Loading u-clips...done")
(provide 'u-clips)

;;; u-clips.el ends here
