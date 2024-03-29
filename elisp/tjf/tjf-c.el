;;; tjf-c.el --- C major mode support -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2021-2024 Tom Fontaine

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

;;; Code:

(message "Loading from tjf-c...")
;; (require 'eglot)
;; (require 'flycheck)
;; (require 'tjf-cc)

;;
(defvar tjf:c/compiler)
(setq   tjf:c/compiler "gcc")

(defvar tjf:c/debug)
(setq   tjf:c/debug "-g")

(defvar tjf:c/dialect)
(setq   tjf:c/dialect "c18")

(defvar tjf:c/includes)
(setq   tjf:c/includes "-I.")

(defvar tjf:c/ldflags)
(setq   tjf:c/ldflags "-lm -pthread")

(defvar tjf:c/makeflags)
(setq   tjf:c/makeflags "")

(defvar tjf:c/optimization)
(setq   tjf:c/optimization "-O")

(defvar tjf:c/std)
(setq   tjf:c/std (concat "-std=" tjf:c/dialect))

(defvar tjf:c/warnings)
(defvar tjf:c/warnings "-Wall -Wextra -Wconversion")

(defun tjf:c/check ()
  "Run ‘cppcheck’ on buffer."
  (interactive)
  (let ((tmp (join "/" `("/tmp" ,(basename))))
        (buf (current-buffer))
        (std (concat "-" tjf:c/std)))
    (with-temp-buffer
      (insert-buffer-substring buf)
      (write-file tmp)
      (compile (join " " `("cppcheck" "--language=c" ,std ,tmp))))))

(defun tjf:c/flags ()
  "Return the compiler flags."
  (join " " `(,tjf:c/std ,tjf:cpp/includes ,tjf:c/debug ,tjf:c/optimization ,tjf:c/warnings)))

(defun tjf:c/compile-file ()
  "Compile the current file."
  (interactive)
  (compile (join " " `(,tjf:c/compiler ,(tjf:c/flags) ,(basename) "-o" ,(concat (basename-no-ext) ".o")))))

(defun tjf:c/compile-program ()
  "Compile and link the current file."
  (interactive)
  (compile (join " " `(,tjf:c/compiler ,(tjf:c/flags) ,(basename) "-o" ,(basename-no-ext)))))

(defun tjf:c/make ()
  "Make the current program."
  (interactive)
  (compile (concat "make " tjf:c/makeflags)))

(defun tjf:c/set-compiler ()
  "Allow the user to set ‘COMPILER’."
  (interactive)
  (let ((compiler (read-shell-command "Compiler: " tjf:c/compiler)))
    (unless (string= compiler tjf:c/compiler)
      (setq tjf:c/compiler compiler))))

(defun tjf:c/set-debug ()
  "Allow the user to set ‘DEBUG’ level."
  (interactive)
  (let ((debug (read-shell-command "Debug: " tjf:c/debug)))
    (unless (string= debug tjf:c/debug)
      (setq tjf:c/debug debug))))

(defun tjf:c/set-dialect ()
  "Allow the user to set ‘DIALECT’."
  (interactive)
  (let ((dialect (read-shell-command "Dialect: " tjf:c/dialect)))
    (setq tjf:c/dialect dialect)
    (setq tjf:c/std (concat "-std=" dialect))))

(defun tjf:c/set-includes ()
  "Allow the user to set -I flags."
  (interactive)
  (let ((flags (read-shell-command "Include flags: " tjf:c/includes)))
    (unless (string= flags tjf:c/includes)
      (setq tjf:c/includes flags))))

(defun tjf:c/set-ldflags ()
  "Allow the user to set ‘LDFLAGS’."
  (interactive)
  (let ((flags (read-shell-command "Linker Flags: " tjf:c/ldflags)))
    (unless (string= flags tjf:c/ldflags)
      (setq tjf:c/ldflags flags))))

(defun tjf:c/set-makeflags ()
  "Allow the user to set ‘MAKE’ flags."
  (interactive)
  (let ((makeflags (read-shell-command "Makeflags: " tjf:c/makeflags)))
    (unless (string= makeflags tjf:c/makeflags)
      (setq tjf:c/makeflags makeflags))))

(defun tjf:c/set-optimization ()
  "Allow the user to set ‘OPTIMIZATION’ level."
  (interactive)
  (let ((optimization (read-shell-command "Optimization: " tjf:c/optimization)))
    (unless (string= optimization tjf:c/optimization)
      (setq tjf:c/optimization optimization))))

(defun tjf:c/set-warnings ()
  "Allow the user to set ‘WARNINGS’."
  (interactive)
  (let ((warnings (read-shell-command "Warnings: " tjf:c/warnings)))
    (unless (string= warnings tjf:c/warnings)
      (tjf:cc/set-warnings warnings))))

(defun tjf:c/config ()
  "C mode config function."
  (define-key c-ts-mode-map [menu-bar]    nil)
  (define-key c-ts-mode-map [(control d)] nil)
  (define-key c-ts-mode-map [(control super \;)] 'tjf:cc/insert-docstring)

  (easy-menu-define tjf-cpp-menu   c-ts-mode-map "C" (append '("C") tjf:cc/menu-text))
  (easy-menu-define cpp-build-menu c-ts-mode-map "C Build" tjf:c/build-menu))

(defun tjf:c/hook ()
  "C mode hook function."
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local completion-at-point-functions (cons #'eglot-completion-at-point completion-at-point-functions))
  (setq-local completion-at-point-functions (cons #'clang-capf                completion-at-point-functions))

  (abbrev-mode   -1)
  (flycheck-mode -1)

  (flymake-mode)

  ;; (setq flycheck-gcc-language-standard   tjf:c/dialect)
  ;; (setq flycheck-clang-language-standard tjf:c/dialect)

  (eglot-ensure)

  (imenu-add-to-menubar "Navigate"))

(defun tjf:c/syntax-check ()
  "Compile the current buffer (syntax check only)."
  (interactive)
  (compile (join " " `(,tjf:c/compiler ,(tjf:c/flags) "-fsyntax-only" ,(basename)))))

(defvar tjf:c/build-menu
  '("Build"
    ["Syntax  Check"   tjf:c/syntax-check    t]
    ["Static Analysis" tjf:c/check           t]
    ["Compile File"    tjf:c/compile-file    t]
    ["Compile Program" tjf:c/compile-program t]
    "---"
    ["Make"    tjf:c/make t]
    ["Make..." compile    t]
    "---"
    ["Set Compiler..."           tjf:c/set-compiler     t]
    ["Set Debug Level..."        tjf:c/set-debug        t]
    ["Set Dialect..."            tjf:c/set-dialect      t]
    ["Set Linker Flags..."       tjf:c/set-ldflags      t]
    ["Set Optimization Level..." tjf:c/set-optimization t]
    ["Set Warning Flags..."      tjf:c/set-warnings     t]
    "---"
    ["Set Make Flags..." tjf:c/set-makeflags t]
    ))

;;
(message "Loading tjf-c...done")
(provide 'tjf-c)

;;; tjf-c.el ends here
