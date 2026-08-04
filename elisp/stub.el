;;; default.el --- initialization stub -*-lexical-binding: t-*- ;; --*-no-byte-compile: t ;; *-Emacs-Lisp-*-

(enable-theme 'fontaine)

(defvar tjf:default/package-initialize nil
  "Package initialization guard.")

(unless tjf:default/package-initialize
  (package-initialize)
  (add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/")       t)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("non-gnu"      . "https://elpa.nongnu.org/nongnu/")   t)

  (setq package-archive-priorities '(("gnu"				.	10)
									 ("non-gnu"			.	20)
									 ("melpa-stable"	.	30)
									 ("melpa"			.	40)))
  (setq tjf:default/package-initialize t))
