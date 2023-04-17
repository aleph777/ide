;;; tjf-cpp.el --- C++ major mode support -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2021-2023 Tom Fontaine

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
;;; Code:

(message "Loading tjf-cpp...")
(require 'eglot)
(require 'tjf-cc)
(require 'tjf-macro)

;;
(defvar tjf:cpp/compiler)
(setq   tjf:cpp/compiler "g++")

(defvar tjf:cpp/debug)
(setq   tjf:cpp/debug "-g")

(defvar tjf:cpp/dialect)
(setq   tjf:cpp/dialect "c++17")

(defvar tjf:cpp/includes)
(setq   tjf:cpp/includes "-I.")

(defvar tjf:cpp/ldflags)
(setq   tjf:cpp/ldflags "-lm -pthread")

(defvar tjf:cpp/makeflags)
(setq   tjf:cpp/makeflags "")

(defvar tjf:cpp/optimization)
(setq   tjf:cpp/optimization "-O")

(defvar tjf:cpp/std)
(setq   tjf:cpp/std (concat "-std=" tjf:cpp/dialect))

(defvar tjf:cpp/warnings)
(setq   tjf:cpp/warnings "-Wall -Wextra -Wconversion")

;; (defun tjf:cpp/build-sh ()
;;   "Run ‘build.sh’."
;;   (interactive)
;;   (let* ((dir (concat tjf:user/dir-home "Workspace/tenbeauty"))
;;          (cmd (concat "cd " dir " && ./build.sh")))
;;     (compile cmd)
;; !
;; ))

(defun tjf:cpp/check ()
  "Run ‘cppcheck’ on buffer."
  (interactive)
  (let ((tmp (join "/" `("/tmp" ,(basename))))
        (buf (current-buffer))
        (std (concat "-" tjf:cpp/std)))
    (if (string-equal tjf:cpp/dialect "c++2a")
        (setq std "--std=c++20"))
    (with-temp-buffer
      (insert-buffer-substring buf)
      (write-file tmp)
      (compile (join " " `("cppcheck" "--language=c++" ,std ,tmp))))))

(defun tjf:cpp/compile-file ()
  "Compile current buffer."
  (interactive)
  (compile (join " " `(,tjf:cpp/compiler ,(tjf:cpp/flags) ,(basename) "-o" ,(concat (basename-no-ext) ".o")))))

(defun tjf:cpp/compile-program ()
  "Compile and link the current file."
  (interactive)
  ;; (compile (join " " `(,tjf:cpp/compiler ,(tjf:cpp/flags) ,tjf:cpp/ldflags ,(basename) "-o" ,(basename-no-ext)))))
  (compile (join " " `(,tjf:cpp/compiler ,(tjf:cpp/flags) ,tjf:cpp/ldflags ,(basename) "-o" ,(basename-no-ext)))))

(defun tjf:cpp/flags ()
  "Return the compiler flags."
  (join " " `(,tjf:cpp/std ,tjf:cpp/includes ,tjf:cpp/debug ,tjf:cpp/optimization ,tjf:cpp/warnings)))

(defun tjf:cpp/make ()
  "Build using make."
  (interactive)
  (compile (join " " `("make" ,(concat "-j" (shell-command-to-string "nproc"))))))

(defun tjf:cpp/set-compiler ()
  "Allow the user to set ‘COMPILER’."
  (interactive)
  (let ((compiler (read-shell-command "Compiler: " tjf:cpp/compiler)))
    (unless (string= compiler tjf:cpp/compiler)
      (setq tjf:cpp/compiler compiler))))

(defun tjf:cpp/set-debug ()
  "Allow the user to set ‘DEBUG’ level."
  (interactive)
  (let ((debug (read-shell-command "Debug: " tjf:cpp/debug)))
    (unless (string= debug tjf:cpp/debug)
      (setq tjf:cpp/debug debug))))

(defun tjf:cpp/set-dialect ()
  "Allow the user to set ‘DIALECT’."
  (interactive)
  (let ((dialect (read-shell-command "Dialect: " tjf:cpp/dialect)))
    (setq tjf:cpp/dialect dialect)
    (setq tjf:cpp/std (concat "-std=" dialect))))

(defun tjf:cpp/set-includes ()
  "Allow the user to set -I flags."
  (interactive)
  (let ((flags (read-shell-command "Include flags: " tjf:cpp/includes)))
    (unless (string= flags tjf:cpp/includes)
      (setq tjf:cpp/includes flags))))

(defun tjf:cpp/set-ldflags ()
  "Allow the user to set ‘LDFLAGS’."
  (interactive)
  (let ((flags (read-shell-command "Linker Flags: " tjf:cpp/ldflags)))
    (unless (string= flags tjf:cpp/ldflags)
      (setq tjf:cpp/ldflags flags))))

(defun tjf:cpp/set-makeflags ()
  "Allow the user to set ‘MAKE’ flags."
  (interactive)
  (let ((makeflags (read-shell-command "Makeflags: " tjf:cpp/makeflags)))
    (unless (string= makeflags tjf:cpp/makeflags)
      (setq tjf:cpp/makeflags makeflags))))

(defun tjf:cpp/set-optimization ()
  "Allow the user to set ‘OPTIMIZATION’ level."
  (interactive)
  (let ((optimization (read-shell-command "Optimization: " tjf:cpp/optimization)))
    (unless (string= optimization tjf:cpp/optimization)
      (setq tjf:cpp/optimization optimization))))

(defun tjf:cpp/set-warnings ()
  "Allow the user to set ‘WARNINGS’."
  (interactive)
  (let ((warnings (read-shell-command "Warnings: " tjf:cpp/warnings)))
    (unless (string= warnings tjf:cpp/warnings)
      (tjf:cc/set-warnings warnings))))

(defun tjf:cpp/setup ()
  "C++ mode setup function."
  (abbrev-mode -1)
  (flymake-mode -1)
  (flycheck-mode)
  (imenu-add-to-menubar "Navigate")
  ;;
  (c-set-style "gnu")
  (c-set-offset 'case-label '+)

  (define-key c++-mode-map [menu-bar]    nil)
  (define-key c++-mode-map [(control d)] nil)
  (define-key c++-mode-map [(control super \;)] 'tjf:cc/insert-docstring)

  (easy-menu-define tjf-cpp-menu   c++-mode-map "C++" (append '("C++") tjf:cc/menu-text))
  (easy-menu-define cpp-build-menu c++-mode-map "C++ Build" tjf:cpp/build-menu)
  ;;
  (setq flycheck-gcc-language-standard tjf:cpp/dialect)
  (eglot-ensure))

(defun tjf:cpp/syntax-check ()
  "Compile current buffer (syntax check only)."
  (interactive)
  (compile (join " " `(,tjf:cpp/compiler ,(tjf:cpp/flags) "-fsyntax-only" ,(basename)))))

(defvar tjf:cpp/build-menu
  '("Build"
    ["Syntax  Check"   tjf:cpp/syntax-check    t]
    ["Static Analysis" tjf:cpp/check           t]
    ["Compile File"    tjf:cpp/compile-file    t]
    ["Compile Program" tjf:cpp/compile-program t]
    "---"
    ["Make"    tjf:cpp/make t]
    ["Make..." compile      t]
    "---"
    ["Set Compiler..."           tjf:cpp/set-compiler     t]
    ["Set Debug Level..."        tjf:cpp/set-debug        t]
    ["Set Dialect..."            tjf:cpp/set-dialect      t]
    ["Set Linker Flags..."       tjf:cpp/set-ldflags      t]
    ["Set Optimization Level..." tjf:cpp/set-optimization t]
    ["Set Warning Flags..."      tjf:cpp/set-warnings     t]
    "---"
    ["Set Make Flags..." tjf:cpp/set-makeflags t]
    ))

;;
(message "Loading tjf-cpp...done")
(provide 'tjf-cpp)

;;; tjf-cpp.el ends here
