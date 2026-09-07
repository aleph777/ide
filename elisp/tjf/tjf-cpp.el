;;; tjf-cpp.el --- C++ major mode support -*-lexical-binding: t-*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2021-2026 Tom Fontaine

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

;;; Code:

(message "Loading tjf-cpp...")
(require 'eglot)
(require 'tjf-cc)
(require 'tjf-macro)

;;
(defvar tjf:cpp/g++)
(setq   tjf:cpp/g++ (first-executable '("g++-16"
                                        "g++-15"
                                        "g++-14"
                                        "g++-13"
                                        "g++-12"
                                        "g++-11"
                                        "g++")))

(defvar tjf:cpp/clang++)
(setq   tjf:cpp/clang++ (first-executable '("clang++-22"
                                            "clang++-21"
                                            "clang++-20"
                                            "clang++-19"
                                            "clang++-18"
                                            "clang++-17"
                                            "clang++")))

(defvar tjf:cpp/compiler)
(setq   tjf:cpp/compiler (or tjf:cpp/g++ tjf:cpp/clang++))

(defvar tjf:cpp/debug)
(setq   tjf:cpp/debug "-g")

(defvar tjf:cpp/cxxstd)
(setq   tjf:cpp/cxxstd "c++23")

(defvar tjf:cpp/oflags)
(setq   tjf:cpp/oflags "-fPIC")

(defvar tjf:cpp/iflags)
(setq   tjf:cpp/iflags "-I.")

(defvar tjf:cpp/wflags)
(setq   tjf:cpp/wflags "-Wall -Wextra -Wpedantic -Werror")

(defvar tjf:cpp/ldflags)
(setq   tjf:cpp/ldflags "-pie -lm -pthread")

(defvar tjf:cpp/makeflags)
(setq   tjf:cpp/makeflags "")

(defvar tjf:cpp/optimization)
(setq   tjf:cpp/optimization "-O")

(defvar tjf:cpp/menu-build)
(setq tjf:cpp/menu-build
      '("Build"
        ["Syntax  Check"   tjf:cpp/syntax-check    t]
        ["Static Analysis" tjf:cpp/check           t]
        ["Compile File"    tjf:cpp/compile-file    t]
        ["Compile Program" tjf:cpp/compile-program t]
        ["---" nil :visible t :enable nil]
        ["Make"    tjf:cpp/make t]
        ["Make..." compile      t]
        ["---" nil :visible t :enable nil]
        ["Set Compiler..."           tjf:cpp/set-compiler     t]
        ["Set Dialect..."            tjf:cpp/set-dialect      t]
        ["Set Include Flags..."      tjf:cpp/set-includes     t]
        ["Set Warning Flags..."      tjf:cpp/set-warnings     t]
        ["Set Other Flags..."        tjf:cpp/set-others       t]
        ["Set Linker Flags..."       tjf:cpp/set-ldflags      t]
        ["Set Debug Level..."        tjf:cpp/set-debug        t]
        ["Set Optimization Level..." tjf:cpp/set-optimization t]
        ["---" nil :visible t :enable nil]
        ["Set Make Flags..." tjf:cpp/set-makeflags t]
        ))

(defun tjf:cpp/check ()
  "Run ‘cppcheck’ on buffer."
  (interactive)
  (let ((tmp (join "/" `("/tmp" ,(basename))))
        (buf (current-buffer))
        (std  (if tjf:cpp/cxxstd (concat "--std=" tjf:cpp/cxxstd))))
    (with-temp-buffer
      (insert-buffer-substring buf)
      (write-file tmp)
      (compile (join " " `("cppcheck" "--language=c++" ,std ,tmp))))))

(defun tjf:cpp/syntax-check ()
  "Compile current buffer (syntax check only)."
  (interactive)
  (let ((std  (if tjf:cpp/cxxstd (concat "--std=" tjf:cpp/cxxstd)))
        (flags (join " " `( ,tjf:cpp/iflags ,tjf:cpp/wflags))))
    (compile (join " " `(,tjf:cpp/compiler ,std ,flags "-fsyntax-only" ,(basename))))))

(defun tjf:cpp/compile-file ()
  "Compile current buffer."
  (interactive)
  (let ((std (if tjf:cpp/cxxstd (concat "--std=" tjf:cpp/cxxstd)))
        (flags (join " " `( ,tjf:cpp/iflags ,tjf:cpp/wflags ,tjf:cpp/oflags))))
    (compile (join " " `(,tjf:cpp/compiler ,std ,flags ,(basename) "-o" ,(concat (basename-no-ext) ".o"))))))

(defun tjf:cpp/compile-program ()
  "Compile and link the current file."
  (interactive)
  (let ((std (if tjf:cpp/cxxstd (concat "--std=" tjf:cpp/cxxstd)))
        (flags (join " " `( ,tjf:cpp/iflags ,tjf:cpp/wflags ,tjf:cpp/oflags "-fPIE" ,tjf:cpp/ldflags))))
    (compile (join " " `(,tjf:cpp/compiler ,std ,flags ,(basename) "-o" ,(basename-no-ext))))))

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
  (let ((dialect (read-shell-command "Dialect: " tjf:cpp/cxxstd)))
    (unless (string= dialect tjf:cpp/cxxstd)
      (setq tjf:cpp/cxxstd dialect))))

(defun tjf:cpp/set-includes ()
  "Allow the user to set -I flags."
  (interactive)
  (let ((flags (read-shell-command "Include flags: " tjf:cpp/iflags)))
    (unless (string= flags tjf:cpp/iflags)
      (setq tjf:cpp/iflags flags))))

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

(defun tjf:cpp/set-others ()
  "Allow the user to set ‘OTHERS’."
  (interactive)
  (let ((others (read-shell-command "Others: " tjf:cpp/oflags)))
    (unless (string= others tjf:cpp/oflags)
      (setq tjf:cpp/oflags others))))

(defun tjf:cpp/set-warnings ()
  "Allow the user to set ‘WARNINGS’."
  (interactive)
  (let ((warnings (read-shell-command "Warnings: " tjf:cpp/wflags)))
    (unless (string= warnings tjf:cpp/wflags)
      (setq tjf:cpp/wflags warnings))))

(defun tjf:cpp/config ()
  "C++ mode config function."
  ;; (treesit-install-language-grammar 'cpp)

  (if (eq major-mode 'c++-ts-mode)
      (progn
        (define-key c++-ts-mode-map [menu-bar]    nil)
        (define-key c++-ts-mode-map [(control d)] nil)
        (define-key c++-ts-mode-map [(control super \;)] 'tjf:cc/insert-docstring)

        (easy-menu-define tjf-cpp-menu   c++-ts-mode-map "C++" (append '("C++") tjf:cc/menu))
        (easy-menu-define cpp-build-menu c++-ts-mode-map "C++ Build"            tjf:cpp/menu-build))

    (define-key c++-mode-map [menu-bar]    nil)
    (define-key c++-mode-map [(control d)] nil)
    (define-key c++-mode-map [(control super \;)] 'tjf:cc/insert-docstring)

    (easy-menu-define tjf-cpp-menu   c++-mode-map "C++" (append '("C++") tjf:cc/menu))
    (easy-menu-define cpp-build-menu c++-mode-map "C++ Build"            tjf:cpp/menu-build)))

(defun tjf:cpp/hook ()
  "C++ mode hook function."
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-keyword
                     #'cape-dabbrev
                     #'cape-file)))

  (abbrev-mode   -1)
  (flycheck-mode -1)

  (flymake-mode)

  ;; (setq flycheck-gcc-language-standard   tjf:cpp/dialect)
  ;; (setq flycheck-clang-language-standard tjf:cpp/dialect)

  (eglot-ensure)

  (imenu-add-to-menubar "Navigate"))

;;
(message "Loading tjf-cpp...done")
(provide 'tjf-cpp)

;;; tjf-cpp.el ends here
