;;; tjf-cc.el --- Common C/C++ major mode support -*-lexical-binding: t-*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2021-2026 Tom Fontaine

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

;;; Code:

(message "Loading tjf-cc...")
(require 'cc-mode)
(require 'f)
(require 's)
(require 'tjf-flags)
(require 'tjf-macro)

;;
(defvar tjf:cc/nproc)
(setq   tjf:cc/nproc (get-shell-output "nproc"))

(defvar tjf:cc/gcc)
(setq   tjf:cc/gcc (first-executable '("gcc-16"
                                       "gcc-15"
                                       "gcc-14"
                                       "gcc-13"
                                       "gcc-12"
                                       "gcc-11"
                                       "gcc")))

(defvar tjf:cc/gcc-version)
(setq   tjf:cc/gcc-version (car (cdr (s-split "-" tjf:cc/gcc))))

(defvar tjf:cc/clang)
(setq   tjf:cc/clang (first-executable '("clang-22"
                                         "clang-21"
                                         "clang-20"
                                         "clang-19"
                                         "clang-18"
                                         "clang-17"
                                         "clang")))

(defvar tjf:cc/clang-version)
(setq   tjf:cc/clang-version (car (cdr (s-split "-" tjf:cc/clang))))

(defvar tjf:cc/clangd)
(setq   tjf:cc/clangd (join "-" (list "clangd" tjf:cc/clang-version)))

(defvar tjf:cc/clang-format)
(setq   tjf:cc/clang-format (join "-" (list "clang-format" tjf:cc/clang-version)))

(defvar tjf:cc/clang-tidy)
(setq   tjf:cc/clang-tidy (join "-" (list "clang-tidy" tjf:cc/clang-version)))

(defvar tjf:cc/file-format)
(setq   tjf:cc/file-format (concat tjf:cc/clang-format ".yml"))

(defvar tjf:cc/path-format)
(setq   tjf:cc/path-format (first-file-exists (list (concat tjf:user/dir-home    "code/Core/" tjf:cc/file-format)
                                                    (concat tjf:user/dir-home    "Core/"      tjf:cc/file-format)
                                                    (concat tjf:user/dir-config  "clang/"     tjf:cc/file-format)
                                                    ".clang-format")))

(defvar tjf:cc/menu)
(setq tjf:cc/menu
  '(
    ["Insert Header File Skeleton" tjf:cc/insert-header-skeleton :active (tjf:flags/is-rw?)]
    ["Insert Source File Skeleton" tjf:cc/insert-source-skeleton :active (tjf:flags/is-rw?)]
    ["Insert Boilerplate"          tjf:cc/insert-boilerplate     :active (tjf:flags/is-rw?)]
    ["Insert Header Guard"         tjf:cc/insert-header-guard    :active (tjf:flags/is-rw?)]
    ["Insert Docstring Template"   tjf:cc/insert-docstring       :active (tjf:flags/is-rw?)]
    ;; ["Format File"                 tjf:cc/format                 :active (tjf:flags/is-rw?)]
    "---"
    ("Format"
     ["Format Buffer or Region" tjf:cc/format :active (tjf:flags/is-rw?)]
     "---"
     ["Format Buffer or Region (Chromium)"  (tjf:cc/format "chromium")  :active (tjf:flags/is-rw?)]
     ["Format Buffer or Region (Gnu)"       (tjf:cc/format "gnu")       :active (tjf:flags/is-rw?)]
     ["Format Buffer or Region (Google)"    (tjf:cc/format "google")    :active (tjf:flags/is-rw?)]
     ["Format Buffer or Region (LLVM)"      (tjf:cc/format "llvm")      :active (tjf:flags/is-rw?)]
     ["Format Buffer or Region (Microsoft)" (tjf:cc/format "microsoft") :active (tjf:flags/is-rw?)]
     ["Format Buffer or Region (Mozilla)"   (tjf:cc/format "mozilla")   :active (tjf:flags/is-rw?)]
     ["Format Buffer or Region (WebKit)"    (tjf:cc/format "webkit")    :active (tjf:flags/is-rw?)]
     )
    "---"
    ["Beginning Of Function" beginning-of-defun]
    ["End Of Function"       end-of-defun      ]
    ["Mark Function"         c-mark-function   ]
    ["---" nil :visible t :enable nil]
    ["Fill Comment Paragraph" c-fill-paragraph :active (tjf:flags/is-rw?)]
    [tjf:menu-separator-1! nil :visible t :enable nil]
    ["Backward Statement" c-beginning-of-statement]
    ["Forward  Statement" c-end-of-statement      ]
    ["---" nil :visible t :enable nil]
    ["Up Conditional"       c-up-conditional      ]
    ["Backward Conditional" c-backward-conditional]
    ["Forward  Conditional" c-forward-conditional ]
    ["---" nil :visible t :enable nil]
    ["Check Mode Readiness" tjf:cc/ready]
    ))

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
    (concat filename "_" ext "_")))

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
      (search-forward "<<<YEAR>>>")
      (replace-match year t)
      (search-forward "<<<CHOLDER>>>")
      (replace-match tjf:user/copyright-holder t)
      )))

(defun tjf:cc/insert-docstring ()
  "Insert a docstring template at the beginning of the function at point."
  (interactive "*")
  (beginning-of-defun)
  (insert-file-contents (concat tjf:user/dir-elisp "templates/cc-docstring.h")))

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

(defun tjf:cc/insert-default-include ()
  "Insert an #include  statement."
  (let* ((ccext    (file-extension))
         (ext      (if (string= ccext ".c") ".h" ".hpp"))
         (inc-file (concat (basename-no-ext) ext)))
      (save-excursion
        (goto-char (point-min))
        (insert (concat "\n#include \"" inc-file "\"\n\n")))))

(defun tjf:cc/insert-source-skeleton ()
  "Insert a source file skeleton."
  (interactive "*")
  (tjf:cc/insert-default-include)
  (tjf:cc/insert-boilerplate))

(defun tjf:cc/ready ()
  "Display readiness of treesitter."
  (interactive)
  (message (if (or (and (eq major-mode 'c-ts-mode)   (treesit-ready-p 'c))
                   (and (eq major-mode 'c++-ts-mode) (treesit-ready-p 'cpp)))
               "Ready"
             "NOT ready")))

(defun tjf:cc/format (&optional style)
  "Format the entire buffer or the region."
  (interactive "*")
  (save-excursion
    (let ((args (if style
                    (concat "--style=" style)
                  (concat "--style=file:" tjf:cc/path-format))))
      (with-buffer-or-region (beg end)
                             (call-process-region beg end tjf:cc/clang-format t t t args)))))

;;
(message "Loading tjf-cc...done")
(provide 'tjf-cc)

;;; tjf-cc.el ends here
