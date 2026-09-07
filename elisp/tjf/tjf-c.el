;;; tjf-c.el --- C major mode support -*-lexical-binding: t-*- ;; -*-Emacs-Lisp-*-

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

(message "Loading from tjf-c...")
(require 'c-ts-mode)
(require 'flycheck)
(require 'tjf-cc)
(require 'tjf-macro)
;;
(defvar tjf:c/gcc)
(setq   tjf:c/gcc tjf:cc/gcc)

(defvar tjf:c/clang)
(setq   tjf:c/clang tjf:cc/clang)

(defvar tjf:c/cflags)
(setq   tjf:c/cflags "-fPIC")

(defvar tjf:c/compiler)
(setq   tjf:c/compiler (or tjf:c/gcc tjf:c/clang))

(defvar tjf:c/debug)
(setq   tjf:c/debug "-g")

(defvar tjf:c/dialect)
(setq   tjf:c/dialect "c18")

(defvar tjf:c/includes)
(setq   tjf:c/includes "-I.")

(defvar tjf:c/ldflags)
(setq   tjf:c/ldflags "-pie -lm -pthread")

(defvar tjf:c/makeflags)
(setq   tjf:c/makeflags "")

(defvar tjf:c/optimization)
(setq   tjf:c/optimization "-O")

(defvar tjf:c/std)
(setq   tjf:c/std (concat "-std=" tjf:c/dialect))

(defvar tjf:c/warnings)
(defvar tjf:c/warnings "-Wall -Wextra -Wpedantic -Werror")

(defvar tjf:c/menu-build)
(setq tjf:c/menu-build
  '("Build"
    ["Syntax  Check"   tjf:c/syntax-check    t]
    ["Static Analysis" tjf:c/check           t]
    ["Compile File"    tjf:c/compile-file    t]
    ["Compile Program" tjf:c/compile-program t]
    ["---" :visible t :enable nil]
    ["Make"    tjf:c/make t]
    ["Make..." compile    t]
    ["---" :visible t :enable nil]
    ["Set Compiler..."           tjf:c/set-compiler     t]
    ["Set Debug Level..."        tjf:c/set-debug        t]
    ["Set Dialect..."            tjf:c/set-dialect      t]
    ["Set Compiler Flags..."     tjf:c/set-cflags       t]
    ["Set Linker Flags..."       tjf:c/set-ldflags      t]
    ["Set Optimization Level..." tjf:c/set-optimization t]
    ["Set Warning Flags..."      tjf:c/set-warnings     t]
    ["---" :visible t :enable nil]
    ["Set Make Flags..." tjf:c/set-makeflags t]
    ))

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
  (join " " `(,tjf:c/std ,tjf:c/includes ,tjf:c/debug ,tjf:c/optimization ,tjf:c/warnings)))

(defun tjf:c/compile-file ()
  "Compile the current file."
  (interactive)
  (compile (join " " `(,tjf:c/compiler ,tjf:c/cflags ,(tjf:c/flags) ,(basename) "-o" ,(concat (basename-no-ext) ".o")))))

(defun tjf:c/compile-program ()
  "Compile and link the current file."
  (interactive)
  (compile (join " " `(,tjf:c/compiler "-fPIE" ,tjf:c/ldflags ,(tjf:c/flags) ,(basename) "-o" ,(basename-no-ext)))))

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

(defun tjf:c/set-cflags ()
  "Allow the user to set ‘CFLAGS’."
  (interactive)
  (let ((flags (read-shell-command "Linker Flags: " tjf:c/cflags)))
    (unless (string= flags tjf:c/cflags)
      (setq tjf:c/cflags flags))))

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
      (tjf:c/set-warnings warnings))))

(defun tjf:c/config ()
  "C mode config function."
  ;; (treesit-install-language-grammar 'c)

  (define-key c-ts-mode-map [menu-bar]    nil)
  (define-key c-ts-mode-map [(control d)] nil)
  (define-key c-ts-mode-map [(control super \;)] 'tjf:cc/insert-docstring)

  (easy-menu-define tjf-cpp-menu   c-ts-mode-map "C" (append '("C") tjf:cc/menu))
  (easy-menu-define cpp-build-menu c-ts-mode-map "C Build" tjf:c/menu-build))

(defun tjf:c/hook ()
  "C mode hook function."
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-keyword
                     #'cape-dabbrev
                     #'cape-file)))

  (abbrev-mode   -1)
  ;; (flymake-mode  -1)
  (flycheck-mode  1)

  (remove-hook 'flymake-diagnostic-functions 'flymake-cc)

  (setq flycheck-gcc-language-standard   tjf:c/dialect)
  (setq flycheck-clang-language-standard tjf:c/dialect)

  (eglot-ensure)

  (imenu-add-to-menubar "Navigate"))

(defun tjf:c/syntax-check ()
  "Compile the current buffer (syntax check only)."
  (interactive)
  (compile (join " " `(,tjf:c/compiler ,(tjf:c/flags) "-fsyntax-only" ,(basename)))))

;;
(message "Loading tjf-c...done")
(provide 'tjf-c)

;;; tjf-c.el ends here
