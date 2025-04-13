;;; tjf-cpp-ts.el --- C++ major mode support -*-lexical-binding: t-*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2021-2025 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   10-Feb-2021

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

;; Revision: 13-Sep-2022 added ‘clang-capf’
;;           16-Sep-2022 added ‘tjf:cpp/check’
;;           27-Sep-2022 added ‘eglot’
;;           20-Oct-2022 added ‘tjf:cpp/includes’ to ‘tjf:cpp/flags’
;;           21-Oct-2022 added key definition for ‘tjf:cc/insert-docstring’
;;                       added ‘tjf:cpp/set-includes’
;;           04-Jan-2023 fixed ‘tjf:cpp/setup’
;;           13-Apr-2023 removed ‘company-mode’ from completions
;;           06-Jun-2023 changed from ‘tjf:cpp/setup’ to ‘tjf:cpp/hook’ and ‘tjf:cpp/config’
;;           21-Mar-2025 fixed bug in ‘tjf:cpp/warnings’
;;           23-Mar-2025 ‘tjf:cpp-ts’

;;; Code:

(message "Loading tjf-cpp-ts...")
;; (require 'eglot)
(require 'tjf-cc)
;; (require 'tjf-macro)

;;
(defvar tjf:cpp-ts/compiler)
(setq   tjf:cpp-ts/compiler "g++")

(defvar tjf:cpp-ts/debug)
(setq   tjf:cpp-ts/debug "-g")

(defvar tjf:cpp-ts/dialect)
(setq   tjf:cpp-ts/dialect "c++17")

(defvar tjf:cpp-ts/includes)
(setq   tjf:cpp-ts/includes "-I.")

(defvar tjf:cpp-ts/ldflags)
(setq   tjf:cpp-ts/ldflags "-lm -pthread")

(defvar tjf:cpp-ts/makeflags)
(setq   tjf:cpp-ts/makeflags "")

(defvar tjf:cpp-ts/optimization)
(setq   tjf:cpp-ts/optimization "-O")

(defvar tjf:cpp-ts/std)
(setq   tjf:cpp-ts/std (concat "-std=" tjf:cpp-ts/dialect))

(defvar tjf:cpp-ts/warnings)
(setq   tjf:cpp-ts/warnings "-Wall -Wextra -Wconversion")

(defun tjf:cpp-ts/check ()
  "Run ‘cppcheck’ on buffer."
  (interactive)
  (let ((tmp (join "/" `("/tmp" ,(basename))))
        (buf (current-buffer))
        (std (concat "-" tjf:cpp-ts/std)))
    (if (string-equal tjf:cpp-ts/dialect "c++2a")
        (setq std "--std=c++20"))
    (with-temp-buffer
      (insert-buffer-substring buf)
      (write-file tmp)
      (compile (join " " `("cppcheck" "--language=c++" ,std ,tmp))))))

(defun tjf:cpp-ts/compile-file ()
  "Compile current buffer."
  (interactive)
  (compile (join " " `(,tjf:cpp-ts/compiler ,(tjf:cpp-ts/flags) ,(basename) "-o" ,(concat (basename-no-ext) ".o")))))

(defun tjf:cpp-ts/compile-program ()
  "Compile and link the current file."
  (interactive)
  ;; (compile (join " " `(,tjf:cpp-ts/compiler ,(tjf:cpp-ts/flags) ,tjf:cpp-ts/ldflags ,(basename) "-o" ,(basename-no-ext)))))
  (compile (join " " `(,tjf:cpp-ts/compiler ,(tjf:cpp-ts/flags) ,tjf:cpp-ts/ldflags ,(basename) "-o" ,(basename-no-ext)))))

(defun tjf:cpp-ts/flags ()
  "Return the compiler flags."
  (join " " `(,tjf:cpp-ts/std ,tjf:cpp-ts/includes ,tjf:cpp-ts/debug ,tjf:cpp-ts/optimization ,tjf:cpp-ts/warnings)))

(defun tjf:cpp-ts/make ()
  "Build using make."
  (interactive)
  (compile (join " " `("make" ,(concat "-j" (shell-command-to-string "nproc"))))))

(defun tjf:cpp-ts/set-compiler ()
  "Allow the user to set ‘COMPILER’."
  (interactive)
  (let ((compiler (read-shell-command "Compiler: " tjf:cpp-ts/compiler)))
    (unless (string= compiler tjf:cpp-ts/compiler)
      (setq tjf:cpp-ts/compiler compiler))))

(defun tjf:cpp-ts/set-debug ()
  "Allow the user to set ‘DEBUG’ level."
  (interactive)
  (let ((debug (read-shell-command "Debug: " tjf:cpp-ts/debug)))
    (unless (string= debug tjf:cpp-ts/debug)
      (setq tjf:cpp-ts/debug debug))))

(defun tjf:cpp-ts/set-dialect ()
  "Allow the user to set ‘DIALECT’."
  (interactive)
  (let ((dialect (read-shell-command "Dialect: " tjf:cpp-ts/dialect)))
    (setq tjf:cpp-ts/dialect dialect)
    (setq tjf:cpp-ts/std (concat "-std=" dialect))))

(defun tjf:cpp-ts/set-includes ()
  "Allow the user to set -I flags."
  (interactive)
  (let ((flags (read-shell-command "Include flags: " tjf:cpp-ts/includes)))
    (unless (string= flags tjf:cpp-ts/includes)
      (setq tjf:cpp-ts/includes flags))))

(defun tjf:cpp-ts/set-ldflags ()
  "Allow the user to set ‘LDFLAGS’."
  (interactive)
  (let ((flags (read-shell-command "Linker Flags: " tjf:cpp-ts/ldflags)))
    (unless (string= flags tjf:cpp-ts/ldflags)
      (setq tjf:cpp-ts/ldflags flags))))

(defun tjf:cpp-ts/set-makeflags ()
  "Allow the user to set ‘MAKE’ flags."
  (interactive)
  (let ((makeflags (read-shell-command "Makeflags: " tjf:cpp-ts/makeflags)))
    (unless (string= makeflags tjf:cpp-ts/makeflags)
      (setq tjf:cpp-ts/makeflags makeflags))))

(defun tjf:cpp-ts/set-optimization ()
  "Allow the user to set ‘OPTIMIZATION’ level."
  (interactive)
  (let ((optimization (read-shell-command "Optimization: " tjf:cpp-ts/optimization)))
    (unless (string= optimization tjf:cpp-ts/optimization)
      (setq tjf:cpp-ts/optimization optimization))))

(defun tjf:cpp-ts/set-warnings ()
  "Allow the user to set ‘WARNINGS’."
  (interactive)
  (let ((warnings (read-shell-command "Warnings: " tjf:cpp-ts/warnings)))
    (unless (string= warnings tjf:cpp-ts/warnings)
      (setq tjf:cpp-ts/warnings warnings))))

(defun tjf:cpp-ts/config ()
  "C++ mode config function."
  (message "tjf:cpp-ts/config...")
  (define-key c++-ts-mode-map [menu-bar]    nil)
  (define-key c++-ts-mode-map [(control d)] nil)
  (define-key c++-ts-mode-map [(control super \;)] 'tjf:cc/insert-docstring)

  (easy-menu-define tjf-cpp-ts-menu   c++-ts-mode-map "C++" (append '("C++") tjf:cc/menu-text))
  (easy-menu-define cpp-build-menu c++-ts-mode-map "C++ Build" tjf:cpp-ts/build-menu)
  (message "tjf:cpp-ts/config...done"))

(defun tjf:cpp-ts/hook ()
  "C++ mode hook function."
  (setq-local completion-at-point-functions (cons #'eglot-completion-at-point completion-at-point-functions))
  (setq-local completion-at-point-functions (cons #'clang-capf                completion-at-point-functions))

  (abbrev-mode   -1)
  (flycheck-mode -1)

  (flymake-mode)

  ;; (setq flycheck-gcc-language-standard   tjf:cpp-ts/dialect)
  ;; (setq flycheck-clang-language-standard tjf:cpp-ts/dialect)

  (eglot-ensure)

  (imenu-add-to-menubar "Navigate"))

(defun tjf:cpp-ts/syntax-check ()
  "Compile current buffer (syntax check only)."
  (interactive)
  (compile (join " " `(,tjf:cpp-ts/compiler ,(tjf:cpp-ts/flags) "-fsyntax-only" ,(basename)))))

(defvar tjf:cpp-ts/build-menu
  '("Build"
    ["Syntax  Check"   tjf:cpp-ts/syntax-check    t]
    ["Static Analysis" tjf:cpp-ts/check           t]
    ["Compile File"    tjf:cpp-ts/compile-file    t]
    ["Compile Program" tjf:cpp-ts/compile-program t]
    "---"
    ["Make"    tjf:cpp-ts/make t]
    ["Make..." compile      t]
    "---"
    ["Set Compiler..."           tjf:cpp-ts/set-compiler     t]
    ["Set Debug Level..."        tjf:cpp-ts/set-debug        t]
    ["Set Dialect..."            tjf:cpp-ts/set-dialect      t]
    ["Set Linker Flags..."       tjf:cpp-ts/set-ldflags      t]
    ["Set Optimization Level..." tjf:cpp-ts/set-optimization t]
    ["Set Warning Flags..."      tjf:cpp-ts/set-warnings     t]
    "---"
    ["Set Make Flags..." tjf:cpp-ts/set-makeflags t]
    ))

;;
(message "Loading tjf-cpp-ts...done")
(provide 'tjf-cpp-ts)

;;; tjf-cpp-ts.el ends here
