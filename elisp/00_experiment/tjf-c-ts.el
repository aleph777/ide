;;; tjf-c-ts.el --- C major mode support -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2021-2025 Tom Fontaine

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

;; Revision: 13-Sep-2022 Added ‘clang-capf’
;;           16-Sep-2022 Added ‘tjf:c/check’
;;           27-Sep-2022 Added ‘eglot’
;;           20-Oct-2022 Added ‘tjf:c/includes’ to ‘tjf:c/flags’
;;           21-Oct-2022 Added key definition for ‘tjf:cc/insert-docstring’
;;                       Added ‘tjf:c/set-includes’
;;           04-Jan-2023 Fixed ‘tjf:c/setup’
;;           06-Jun-2023 changed from ‘tjf:c/setup’ to ‘tjf:c/hook’ and ‘tjf:c/config’
;;           23-Mar-2025 ‘tjf:c-ts’

;;; Code:

(message "Loading from tjf-c-ts...")
;; (require 'eglot)
;; (require 'flycheck)
;; (require 'tjf-cc)

;;
(defvar tjf:c-ts/compiler)
(setq   tjf:c-ts/compiler "gcc")

(defvar tjf:c-ts/debug)
(setq   tjf:c-ts/debug "-g")

(defvar tjf:c-ts/dialect)
(setq   tjf:c-ts/dialect "c18")

(defvar tjf:c-ts/includes)
(setq   tjf:c-ts/includes "-I.")

(defvar tjf:c-ts/ldflags)
(setq   tjf:c-ts/ldflags "-lm -pthread")

(defvar tjf:c-ts/makeflags)
(setq   tjf:c-ts/makeflags "")

(defvar tjf:c-ts/optimization)
(setq   tjf:c-ts/optimization "-O")

(defvar tjf:c-ts/std)
(setq   tjf:c-ts/std (concat "-std=" tjf:c-ts/dialect))

(defvar tjf:c-ts/warnings)
(defvar tjf:c-ts/warnings "-Wall -Wextra -Wconversion")

(defun tjf:c-ts/check ()
  "Run ‘cppcheck’ on buffer."
  (interactive)
  (let ((tmp (join "/" `("/tmp" ,(basename))))
        (buf (current-buffer))
        (std (concat "-" tjf:c-ts/std)))
    (with-temp-buffer
      (insert-buffer-substring buf)
      (write-file tmp)
      (compile (join " " `("cppcheck" "--language=c" ,std ,tmp))))))

(defun tjf:c-ts/flags ()
  "Return the compiler flags."
  (join " " `(,tjf:c-ts/std ,tjf:c-tspp/includes ,tjf:c-ts/debug ,tjf:c-ts/optimization ,tjf:c-ts/warnings)))

(defun tjf:c-ts/compile-file ()
  "Compile the current file."
  (interactive)
  (compile (join " " `(,tjf:c-ts/compiler ,(tjf:c-ts/flags) ,(basename) "-o" ,(concat (basename-no-ext) ".o")))))

(defun tjf:c-ts/compile-program ()
  "Compile and link the current file."
  (interactive)
  (compile (join " " `(,tjf:c-ts/compiler ,(tjf:c-ts/flags) ,(basename) "-o" ,(basename-no-ext)))))

(defun tjf:c-ts/make ()
  "Make the current program."
  (interactive)
  (compile (concat "make " tjf:c-ts/makeflags)))

(defun tjf:c-ts/set-compiler ()
  "Allow the user to set ‘COMPILER’."
  (interactive)
  (let ((compiler (read-shell-command "Compiler: " tjf:c-ts/compiler)))
    (unless (string= compiler tjf:c-ts/compiler)
      (setq tjf:c-ts/compiler compiler))))

(defun tjf:c-ts/set-debug ()
  "Allow the user to set ‘DEBUG’ level."
  (interactive)
  (let ((debug (read-shell-command "Debug: " tjf:c-ts/debug)))
    (unless (string= debug tjf:c-ts/debug)
      (setq tjf:c-ts/debug debug))))

(defun tjf:c-ts/set-dialect ()
  "Allow the user to set ‘DIALECT’."
  (interactive)
  (let ((dialect (read-shell-command "Dialect: " tjf:c-ts/dialect)))
    (setq tjf:c-ts/dialect dialect)
    (setq tjf:c-ts/std (concat "-std=" dialect))))

(defun tjf:c-ts/set-includes ()
  "Allow the user to set -I flags."
  (interactive)
  (let ((flags (read-shell-command "Include flags: " tjf:c-ts/includes)))
    (unless (string= flags tjf:c-ts/includes)
      (setq tjf:c-ts/includes flags))))

(defun tjf:c-ts/set-ldflags ()
  "Allow the user to set ‘LDFLAGS’."
  (interactive)
  (let ((flags (read-shell-command "Linker Flags: " tjf:c-ts/ldflags)))
    (unless (string= flags tjf:c-ts/ldflags)
      (setq tjf:c-ts/ldflags flags))))

(defun tjf:c-ts/set-makeflags ()
  "Allow the user to set ‘MAKE’ flags."
  (interactive)
  (let ((makeflags (read-shell-command "Makeflags: " tjf:c-ts/makeflags)))
    (unless (string= makeflags tjf:c-ts/makeflags)
      (setq tjf:c-ts/makeflags makeflags))))

(defun tjf:c-ts/set-optimization ()
  "Allow the user to set ‘OPTIMIZATION’ level."
  (interactive)
  (let ((optimization (read-shell-command "Optimization: " tjf:c-ts/optimization)))
    (unless (string= optimization tjf:c-ts/optimization)
      (setq tjf:c-ts/optimization optimization))))

(defun tjf:c-ts/set-warnings ()
  "Allow the user to set ‘WARNINGS’."
  (interactive)
  (let ((warnings (read-shell-command "Warnings: " tjf:c-ts/warnings)))
    (unless (string= warnings tjf:c-ts/warnings)
      (tjf:c-tsc/set-warnings warnings))))

(defun tjf:c-ts/config ()
  "C mode config function."
  (define-key c-ts-mode-map [menu-bar]    nil)
  (define-key c-ts-mode-map [(control d)] nil)
  (define-key c-ts-mode-map [(control super \;)] 'tjf:c-tsc/insert-docstring)

  (easy-menu-define tjf-cpp-menu   c-ts-mode-map "C" (append '("C") tjf:c-tsc/menu-text))
  (easy-menu-define cpp-build-menu c-ts-mode-map "C Build" tjf:c-ts/build-menu))

(defun tjf:c-ts/hook ()
  "C mode hook function."
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local completion-at-point-functions (cons #'eglot-completion-at-point completion-at-point-functions))
  (setq-local completion-at-point-functions (cons #'clang-capf                completion-at-point-functions))

  (abbrev-mode   -1)
  (flycheck-mode -1)

  (flymake-mode)

  ;; (setq flycheck-gcc-language-standard   tjf:c-ts/dialect)
  ;; (setq flycheck-clang-language-standard tjf:c-ts/dialect)

  (eglot-ensure)

  (imenu-add-to-menubar "Navigate"))

(defun tjf:c-ts/syntax-check ()
  "Compile the current buffer (syntax check only)."
  (interactive)
  (compile (join " " `(,tjf:c-ts/compiler ,(tjf:c-ts/flags) "-fsyntax-only" ,(basename)))))

(defvar tjf:c-ts/build-menu
  '("Build"
    ["Syntax  Check"   tjf:c-ts/syntax-check    t]
    ["Static Analysis" tjf:c-ts/check           t]
    ["Compile File"    tjf:c-ts/compile-file    t]
    ["Compile Program" tjf:c-ts/compile-program t]
    "---"
    ["Make"    tjf:c-ts/make t]
    ["Make..." compile    t]
    "---"
    ["Set Compiler..."           tjf:c-ts/set-compiler     t]
    ["Set Debug Level..."        tjf:c-ts/set-debug        t]
    ["Set Dialect..."            tjf:c-ts/set-dialect      t]
    ["Set Linker Flags..."       tjf:c-ts/set-ldflags      t]
    ["Set Optimization Level..." tjf:c-ts/set-optimization t]
    ["Set Warning Flags..."      tjf:c-ts/set-warnings     t]
    "---"
    ["Set Make Flags..." tjf:c-ts/set-makeflags t]
    ))

;;
(message "Loading tjf-c-ts...done")
(provide 'tjf-c-ts)

;;; tjf-c-ts.el ends here
