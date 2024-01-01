;;; tjf-clips.el --- CLIPS major mode support for GNU Emacs -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2024 Tom Fontaine

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
;;           03-Feb-2021 ‘tjf’ overhaul
;;

;;; Code:

(message "Loading tjf-clips...")
(require 'clips-mode)

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

(defconst tjf:clips/font-lock-object-match
  (eval-when-compile
    (let ((clips-identifier
           (let ((letter "a-zA-Z_$\-\300-\326\330-\366\370-\377")
                 (digit "0-9"))
             (concat "\\<\\([" letter "][" letter digit ":]*\\)\\>"))))
      (list
       (cons (concat "\\<" (regexp-opt '("object" "is-a")) "\\>\\s *" clips-identifier)
             `(,(+ 1 (regexp-opt-depth (regexp-opt '("is-a")))) 'clips-object-match-face))
       (cons (concat "\\<\\(" (regexp-opt '("object" "is-a")) "\\)\\>") 'clips-object-match-face)))))

(defconst tjf:clips/font-lock-keywords
  (append clips-font-lock-constants
          clips-font-lock-variables
          clips-font-lock-global-variables
          clips-font-lock-control
          clips-font-lock-declaration
          clips-font-lock-constructs
          clips-font-lock-functions
          clips-font-lock-logical
          clips-font-lock-verbs
          tjf:clips/font-lock-object-match)
  "CLIPS mode font-lock definitions.")

(defconst tjf:clips/imenu-generic-expression
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

(setq clips-font-lock-keywords tjf:clips/font-lock-keywords)
(setq clips-imenu-generic-expression tjf:clips/imenu-generic-expression)

(define-key clips-mode-map [menu-bar] nil)

(easy-menu-define tjf:clips/menu clips-mode-map "TJF-CLIPS"
  '("CLIPS"
    ["Beginning Of Function" beginning-of-defun :active t]
    ["End Of Function"       end-of-defun       :active t]
    ["Mark Function"         mark-defun         :active t]))

(defun tjf:clips/delete-forward-space ()
  "Delete whitespace from point to non-whitespace."
  (if (looking-at "[ \t]+")
      (delete-region (match-beginning 0) (match-end 0))))

(defun tjf:clips/tab ()
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
          (tjf:clips/delete-forward-space)
          (insert (make-string ind-goal 32))))
    ))

(make-variable-buffer-local 'highlight-operators-regexp)

(defun tjf:clips/setup ()
  "Set up CLIPS major mode."
  ;; (setq-local indent-line-function 'tjf:clips/tab)
  (setq-local indent-region-function nil)
  (setq-local comment-start ";; ")
  (setq highlight-operators-regexp (regexp-opt '("=>" "<-" "&:")))
  (semantic-mode t)
  (imenu-add-to-menubar "Navigate"))

;;
(message "Loading tjf-clips...done")
(provide 'tjf-clips)

;;; tjf-clips.el ends here
