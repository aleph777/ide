;;; tjf-cc.el --- Common C/C++ major mode support -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2021-2022 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   09-Feb-2021

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

;; Revision:

;;; Code:

(message "Loading tjf-cc...")
(require 'cc-mode)
(require 'f)
(require 's)
(require 'tjf-flags)

;;
(defvar tjf:cc/nproc (shell-command-to-string "nproc"))

(defvar tjf:c/dialect)
(setq   tjf:c/dialect "c18")

(defvar tjf:c/std)
(setq   tjf:c/std (concat "-std=" tjf:c/dialect))

(defvar tjf:cpp/dialect)
(setq   tjf:cpp/dialect "c++2a")

(defvar tjf:cpp/std)
(setq   tjf:cpp/std (concat "-std=" tjf:cpp/dialect))

(defun tjf:cc/docstring ()
  "Convert C++-style comments '^ *//' to a docstring."
  (interactive "*")
  (let* ((exp "^ *// *")
         (beg (progn
                (beginning-of-line)
                (and (looking-at exp) (progn (while (looking-at exp) (forward-line -1)) (forward-line 1) (point)))))
         (end (and beg (progn (while (looking-at exp) (forward-line 1)) (point)))))
    (if beg
        (let ((opn " /**\n")
              (com "  * ")
              (cls "  */\n"))
          (goto-char end)
          (insert cls)
          (save-excursion
            (goto-char end)
            (while (re-search-backward exp beg t)
              (replace-match com nil nil))
            (goto-char beg)
            (insert opn))
          (indent-region beg (point)))
      (error "Not in an eligible comment"))))

(defun tjf:cc/guard-symbol ()
  "Return the guard symbold for the current buffer."
  (let ((filename (upcase (s-replace "-" "_" (basename-no-ext))))
        (ext      (upcase (file-extension))))
    (concat "NAILPRO" "_" filename "_" ext "_")))

(defun tjf:cc/insert-boilerplate ()
  "Insert a C/C++ module boilerplate for ‘(basename)’."
  (interactive "*")
  (goto-char (point-min))
  (save-excursion
    (insert-file-contents (concat tjf:user/dir-elisp "templates/cc-skeleton.cpp")))
  (let ((year (format-time-string "%Y - %Y")))
    (save-excursion
      ;; (search-forward "<<<FILENAME>>>" (point-max) t)
      ;; (replace-match (basename) t)
      ;; (search-forward "<<<CHOLDER>>>")
      ;; (replace-match tjf:user/copyright-holder t)
      (search-forward "<<<YEAR>>>")
      (replace-match year t))))

(defun tjf:cc/insert-header-guard ()
  "Insert a header guard."
  (interactive "*")
  (let* ((guard (tjf:cc/guard-symbol)))
    (goto-char (point-min))
    (insert "\n")
    (insert "#ifndef " guard "\n")
    (insert "#define " guard "\n")
    (goto-char (point-max))
    (insert "\n#endif\n")))

(defun tjf:cc/insert-header-skeleton ()
  "Insert a header skeleton."
  (interactive "*")
  (goto-char (point-min))
  (tjf:cc/insert-header-guard)
  (tjf:cc/insert-boilerplate))

(defun tjf:cc/insert-source-skeleton ()
  "Insert a source file skeleton."
  (interactive "*")
  (goto-char (point-min))
  (let* ((ccext    (file-extension))
         (ext      (if (string= ccext ".c") ".h" ".hpp"))
         (inc-file (concat (basename-no-ext) ext)))
    (tjf:cc/insert-boilerplate)
    (insert (concat "#include \"" inc-file "\"\n\n"))))

(defun tjf:cc/set-dialect (dialect)
  "Set the C/C++ dialect to DIALECT."
  (if (string= (substring dialect 0 2) "c++")
      (progn
        (setq tjf:cpp/dialect dialect)
        (setq tjf:cpp/dialect (concat "-std=" tjf:cpp/dialect)))
    (setq tjf:c/dialect dialect)
    (setq tjf:c/std (concat "-std=" tjf:c/dialect))))

(defvar tjf:cc/menu-text
  '(
    ["Insert Header File Skeleton" tjf:cc/insert-header-skeleton :active (tjf:flags/is-rw?)]
    ["Insert Source File Skeleton" tjf:cc/insert-source-skeleton :active (tjf:flags/is-rw?)]
    ["Insert Boilerplate"          tjf:cc/insert-boilerplate     :active (tjf:flags/is-rw?)]
    ["Insert Header Guard"         tjf:cc/insert-header-guard    :active (tjf:flags/is-rw?)]
    ["Format File"                 eglot-format                  :active (tjf:flags/is-rw?)]
    "---"
    ["Go to Definition" xref-find-definitions]
    "---"
    ["Beginning Of Function" beginning-of-defun]
    ["End Of Function"       end-of-defun      ]
    ["Mark Function"         c-mark-function   ]
    "---"
    ["Fill Comment Paragraph"c-fill-paragraph :active (tjf:flags/is-rw?)]
    ;;    ["Convert comment to docstring" u-docstring    :enable (or c++-mode java-mode)]
    "---"
    ["Backward Statement" c-beginning-of-statement]
    ["Forward  Statement" c-end-of-statement      ]
    "---"
    ["Up Conditional"       c-up-conditional      ]
    ["Backward Conditional" c-backward-conditional]
    ["Forward  Conditional" c-forward-conditional ]
    ))

;;
(message "Loading tjf-cc...done")
(provide 'tjf-cc)

;;; tjf-cc.el ends here
