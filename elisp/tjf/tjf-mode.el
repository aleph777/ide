;;; tjf-mode.el --- major mode groupings -*-lexical-binding: t-*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2023-2024 Tom Fontaine

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

;; Revision:    25-Mar-2024 added  ‘conf-colon-mode’

;;; Code:

(message "Loading tjf-mode...")

;;
(setq auto-mode-alist
        '(("\\.\\(deb\\|[oi]pk\\)\\'" . archive-mode)
          ("\\.[sS]\\'" . asm-mode)
          ("\\.asm\\'"  . asm-mode)
          ("\\(acinclude\\|aclocal\\|acsite\\)\\.m4\\'" . autoconf-mode)
          ("configure\\.\\(ac\\|in\\)\\'"               . autoconf-mode)
          ("\\.awk\\'"             . awk-mode)
          ("\\.bz\\'"              . bazel-mode)
          ("\\.c\\'"               . c-mode)
          ("\\.\\(C\\|H\\)\\'"     . c-mode)
          ("\\.xs\\'"              . c-mode)
          ("\\.h\\'"               . c-or-c++-mode)
          ("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-mode)
          ("\\.\\(CC\\|HH\\)\\'"              . c++-mode)
          ("\\.\\(cc\\|hh\\)\\'"              . c++-mode)
          ("\\.\\(proto\\|tpp\\)\\'"          . c++-mode)
          ("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-mode)
          ("\\`/etc/\\(?:acpid?/.+\\|aliases\\(?:\\.d/.+\\)?\\|default/.+\\|group-?\\|hosts\\..+\\|inittab\\|ksysguarddrc\\|passwd-?\\|shadow-?\\|sysconfig/.+\\)\\'" . conf-mode)
          ("/\\.?\\(?:gitconfig\\|gnokiirc\\|hgrc\\|kde.*rc\\|mime\\.types\\|wgetrc\\)\\'" . conf-mode)
          ("[/.]c\\(?:on\\)?f\\(?:i?g\\)?\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'"                   . conf-mode-maybe)
          ("/\\.\\(?:gtk\\|net\\|nvidia-settings-\\|screen\\|xmp\\)rc\\'"                  . conf-mode)
          ("\\.ini\\'"      . conf-mode)
          ("\\.cs\\'"       . csharp-mode)
          ("\\.css\\'"      . css-mode)
          ("\\.csv\\'"      . csv-mode)
          ("\\.el\\'"       . emacs-lisp-mode)
          ("\\.emacs\\'"    . emacs-lisp-mode)
          ("\\.f9[05]\\'"   . f90-mode)
          ("\\.f0[38]\\'"   . f90-mode)
          ("\\.[fF]\\'"     . fortran-mode)
          ("\\.for\\'"      . fortran-mode)
          ("\\.go\\'"       . go-mode)
          ("\\.bmp\\'"      . image-mode)
          ("\\.cmyka?\\'"   . image-mode)
          ("\\.gif\\'"      . image-mode)
          ("\\.icon?\\'"    . image-mode)
          ("\\.jpe?g\\'"    . image-mode)
          ("\\.p[bpgn]m\\'" . image-mode)
          ("\\.png\\'"      . image-mode)
          ("\\.rgba?\\'"    . image-mode)
          ("\\.svgz?\\'"    . image-mode)
          ("\\.tga\\'"      . image-mode)
          ("\\.tiff?\\'"    . image-mode)
          ("\\.webp\\'"     . image-mode)
          ("\\.x[bp]m\\'"   . image-mode)
          ("\\.xcf\\'"      . image-mode)
          ("\\.java\\'"     . java-mode)
          ("\\.zst\\'"      . jka-compr)
          ("\\.dz\\'"       . jka-compr)
          ("\\.xz\\'"       . jka-compr)
          ("\\.lzma\\'"     . jka-compr)
          ("\\.lz\\'"       . jka-compr)
          ("\\.g?z\\'"      . jka-compr)
          ("\\.bz2\\'"      . jka-compr)
          ("\\.Z\\'"        . jka-compr)
          ("\\.json\\'"     . json-mode)
          ("\\.ltx\\'"      . latex-mode)
          ("\\.l\\'"        . lisp-mode)
          ("\\.li?sp\\'"    . lisp-mode)
          ("\\.am\\'"                    . makefile-automake-mode)
          ("\\.mk\\'"                    . makefile-gmake-mode)
          ("\\.make\\'"                  . makefile-gmake-mode)
          ("[Mm]akefile\\'"              . makefile-gmake-mode)
          ("Imakefile\\'"                . makefile-imake-mode)
          ("Makeppfile\\(?:\\.mk\\)?\\'" . makefile-makepp-mode)
          ("\\.makepp\\'"                . makefile-makepp-mode)
          ("\\.mk\\'"                    . makefile-gmake-mode)
          ("\\.man\\'"     . nroff-mode)
          ("\\.[1-9]\\'"   . nroff-mode)
          ("\\.org\\'"     . org)
          ("\\.py[iw]?\\'" . python-mode)
          ("\\.p\\'"       . pascal-mode)
          ("\\.pas\\'"     . pascal-mode)
          ("\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\'" . cperl-mode)
          ("\\.[eE]?[pP][sS]\\'" . ps-mode)
          ("\\.r\\'"             . rust-mode)
          ("\\.rb\\'"            . ruby-mode)
          ("\\.[ckz]?sh\\'\\|\\.shar\\'\\|/\\.z?profile\\'"                                                  . sh-mode)
          ("\\.bash\\'"                                                                                      . sh-mode)
          ("\\(/\\|\\`\\)\\.\\(bash_\\(profile\\|history\\|log\\(in\\|out\\)\\)\\|z?log\\(in\\|out\\)\\)\\'" . sh-mode)
          ("\\(/\\|\\`\\)\\.\\(shrc\\|zshrc\\|m?kshrc\\|bashrc\\|t?cshrc\\|esrc\\)\\'"                       . sh-mode)
          ("\\(/\\|\\`\\)\\.\\([kz]shenv\\|xinitrc\\|startxrc\\|xsession\\)\\'"                              . sh-mode)
          ("\\.sql\\'"       . sql-mode)
          ("\\.tar\\'"       . tar-mode)
          ("\\.tgz\\'"       . tar-mode)
          ("\\.tbz2?\\'"     . tar-mode)
          ("\\.txz\\'"       . tar-mode)
          ("\\.tzst\\'"      . tar-mode)
          ("\\.[tT]e[xX]\\'" . tex-mode)
          ("\\.texinfo\\'"   . texinfo-mode)
          ("\\.te?xi\\'"     . texinfo-mode)
          ("\\.te?xt\\'"     . text-mode)
          ("\\.ya?ml\\'"     . yaml-mode)))

;;
(defconst tjf:mode/bibtext-mode    '(bibtex-mode bibtex-style-mode))
(defconst tjf:mode/c-mode          '(c-ts-mode c-mode))
(defconst tjf:mode/c++-mode        '(c++-ts-mode c++-mode))
(defconst tjf:mode/cmake-mode      '(cmake-ts-mode cmake-mode))
(defconst tjf:mode/conf-mode       '(conf-mode conf-colon-mode conf-desktop-mode conf-javaprop-mode conf-ppd-mode                                     conf-space-mode conf-toml-mode conf-unix-mode conf-windows-mode conf-xdefaults-mode))
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
(defconst tjf:mode/enriched-mode '(fundamental-mode indented-text-mode text-mode))
(defconst tjf:mode/space-mode    '(fundamental-mode indented-text-mode text-mode))

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
