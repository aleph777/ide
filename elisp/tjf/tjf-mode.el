;;; tjf-mode.el --- [description] -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2023-2023 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   24-Jul-2023

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

(message "Configuring from tjf-mode...")
;;
(defconst tjf:mode/bibtext-mode    '(bibtex-mode bibtex-style-mode))
(defconst tjf:mode/c-mode          '(c-ts-mode c-mode))
(defconst tjf:mode/c++-mode        '(c++-ts-mode c++-mode))
(defconst tjf:mode/cmake-mode      '(cmake-ts-mode cmake-mode))
(defconst tjf:mode/conf-mode       '(conf-mode conf-unix-mode))
(defconst tjf:mode/fortran-mode    '(fortran-mode f90-mode))
(defconst tjf:mode/go-mode         '(go-ts-mode go-mode))
(defconst tjf:mode/help-mode       '(help-mode helpful-mode))
(defconst tjf:mode/html-mode       '(html-ts-mode html-mode html-helper-mode nxhtml-mode))
(defconst tjf:mode/java-mode       '(java-ts-mode java-mode))
(defconst tjf:mode/javascript-mode '(espresso-mode javascript-mode js-mode js2-mode))
(defconst tjf:mode/json-mode       '(json-ts-mode json-mode))
(defconst tjf:mode/julia-mode      '(julia-ts-mode julia-mode))
(defconst tjf:mode/make-mode       '(makefile-automake-mode makefile-bsdmake-mode makefile-gmake-mode makefile-imake-mode makefile-makepp-mode makefile-mode))
(defconst tjf:mode/package-mode    '(package-menu-mode paradox-menu-mode))
(defconst tjf:mode/perl-mode       '(cperl-mode perl-mode))
(defconst tjf:mode/ruby-mode       '(ruby-ts-mode ruby-mode))
(defconst tjf:mode/rust-mode       '(rust-ts-mode rust-mode))
(defconst tjf:mode/sh-script-mode  '(sh-mode shell-script-mode))
(defconst tjf:mode/shell-mode      '(eshell-mode shell-mode))
(defconst tjf:mode/text-mode       '(text-mode indented-text-mode))
(defconst tjf:mode/xml-mode        '(nxml-mode xml-mode))
(defconst tjf:mode/yaml-mode       '(yaml-ts-mode yaml-mode))

;; not for ‘msb’
;;
(defvar tjf:mode/enriched-mode '(fundamental-mode indented-text-mode text-mode))
(defvar tjf:mode/space-mode    '(fundamental-mode indented-text-mode text-mode))


(defun tjf:mode/is-mode? (mode-or-list)
  "Check if ‘MODE-OR-LIST’ is ‘major-mode’."
  (interactive)
  (if (listp mode-or-list)
      (memq major-mode mode-or-list)
    (eq major-mode mode-or-list)))

(defsubst tjf:mode/is-enriched-mode? ()
  "Check if mode is an enriched mode."
  (tjf:mode/is-mode? tjf:mode/enriched-mode))

(defsubst tjf:mode/is-shell-mode? ()
  "Check if mode is a ‘shell-mode’."
  (tjf:mode/is-mode? tjf:mode/shell-mode))

(defsubst tjf:mode/is-not-shell-mode? ()
  "Check if mode is not a ‘shell-mode’."
  (not (tjf:mode/is-mode? tjf:mode/shell-mode)))

;;
(message "Loading tjf-mode...done")
(provide 'tjf-mode)

;;; tjf-mode.el ends here
