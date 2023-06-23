;;; tjf-msb.el --- Msb menu definition -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2023 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   23-Jun-2016

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

;; Revision: 26-Sep-2016 added `clips-log-mode'
;;           13-Jun-2018 added ‘helpful-mode’ to ‘Help’ entry
;;           24-Jun-2019 added ‘groovy’
;;           03-Feb-2021 ‘tjf’ overhaul
;;           02-Jul-2021 added ‘julia-mode’
;;           10-Jan-2023 added ‘cmake-mode’ and ‘csv-mode’
;;           06-Jun-2023 added ‘c-ts-mode’ and ‘c++-ts-mode’
;;           07-Jun-2023 added remaining ts-modes
;;                       removed ‘tjf-flags’ dependency
;;

;;; Code:

(message "Loading tjf-msb...")
(require 'msb)

;;; overload
(with-eval-after-load 'msb
  '(defun msb-menu-bar-update-buffers (&optional arg)
     "A re-written version of `menu-bar-update-buffers'."
     ;; If user discards the Buffers item, play along.
     (when (and (lookup-key (current-global-map) [menu-bar buffer])
                (or (not (fboundp 'frame-or-buffer-changed-p))
                    (frame-or-buffer-changed-p)
                    arg))
       (let ((frames (frame-list))
             buffers-menu frames-menu)
         ;; Make the menu of buffers proper.
         (setq msb--last-buffer-menu (msb--create-buffer-menu))
         ;; Skip the `keymap' symbol.
         (setq buffers-menu (cdr msb--last-buffer-menu))
         ;; Make a Frames menu if we have more than one frame.
         (when (cdr frames)
           (let* ((frame-length (length frames))
                  (f-title  (format "Windows (%d)" frame-length)))  ;; tjf
             ;; List only the N most recently selected frames
             (when (and (integerp msb-max-menu-items)
                        (> msb-max-menu-items 1)
                        (> frame-length msb-max-menu-items))
               (setcdr (nthcdr msb-max-menu-items frames) nil))
             (setq frames-menu
                   (nconc
                    (list 'frame f-title '(nil) 'keymap f-title)
                    (mapcar
                     (lambda (frame)
                       (nconc
                        (list (frame-parameter frame 'name)
                              (frame-parameter frame 'name)
                              (cons nil nil))
                        `(lambda ()
                           (interactive) (menu-bar-select-frame ,frame))))
                     frames)))))
         (setcdr global-buffers-menu-map
                 (if (and buffers-menu frames-menu)
                     ;; Combine Frame and Buffers menus with separator between
                     (nconc (list "Buffers and Frames" frames-menu
                                  (and msb-separator-diff '(separator "--")))
                            (cdr buffers-menu))
                   buffers-menu))))))

(defsubst tjf:msb/is-mode? (mode-or-list)
  "Check if ‘MODE-OR-LIST’ is ‘major-mode’."
  (interactive)
  (if (listp mode-or-list)
      (memq major-mode mode-or-list)
    (eq major-mode mode-or-list)))

(defconst tjf:msb/bibtext-mode    '(bibtex-mode bibtex-style-mode))
(defconst tjf:msb/c-mode          '(c-ts-mode c-mode))
(defconst tjf:msb/c++-mode        '(c++-ts-mode c++-mode))
(defconst tjf:msb/cmake-mode      '(cmake-ts-mode cmake-mode))
(defconst tjf:msb/conf-mode       '(conf-mode conf-unix-mode))
(defconst tjf:msb/fortran-mode    '(fortran-mode f90-mode))
(defconst tjf:msb/go-mode         '(go-ts-mode go-mode))
(defconst tjf:msb/help-mode       '(help-mode helpful-mode))
(defconst tjf:msb/html-mode       '(html-ts-mode html-mode html-helper-mode nxhtml-mode))
(defconst tjf:msb/java-mode       '(java-ts-mode java-mode))
(defconst tjf:msb/javascript-mode '(espresso-mode javascript-mode js-mode js2-mode))
(defconst tjf:msb/json-mode       '(json-ts-mode json-mode))
(defconst tjf:msb/julia-mode      '(julia-ts-mode julia-mode))
(defconst tjf:msb/make-mode       '(makefile-gmake-mode makefile-mode makefile-automake-mode makefile-bsdmake-mode makefile-imake-mode makefile-makepp-mode))
(defconst tjf:msb/package-mode    '(package-menu-mode paradox-menu-mode))
(defconst tjf:msb/perl-mode       '(cperl-mode perl-mode))
(defconst tjf:msb/ruby-mode       '(ruby-ts-mode ruby-mode))
(defconst tjf:msb/rust-mode       '(rust-ts-mode rust-mode))
(defconst tjf:msb/sh-script-mode  '(sh-mode shell-script-mode))
(defconst tjf:msb/text-mode       '(text-mode indented-text-mode))
(defconst tjf:msb/text-mode       '(nxml-mode xml-mode))
(defconst tjf:msb/yaml-mode       '(yaml-ts-mode yaml-mode))

(defvar msb--u-menus)
(setq msb--u-menus
      '(;; - 1000
        ((and (boundp 'server-buffer-clients) server-buffer-clients 'multi) 1010 "Clients (%d)")
        ((and (get-buffer-process (current-buffer))                 'multi) 1020 "Processes (%d)")
        ((and (boundp 'vc-mode) vc-mode                             'multi) 1030 "Version Control (%d)")
        ((and buffer-file-name (buffer-modified-p)                  'multi) 1040 "Changed files (%d)")

        ;; A 2000
        ((tjf:msb/is-mode? 'ada-mode)                  2000 "Ada Files (%d)")
        ((tjf:msb/is-mode? 'archive-mode)              2001 "Archive Files (%d)")
        ((tjf:msb/is-mode? 'asm-mode)                  2002 "Assembly Files (%d)")
        ((tjf:msb/is-mode? 'awk-mode)                  2003 "Awk Files (%d)")
        ;; B 2010
        ((tjf:msb/is-mode? 'bat-mode)                  2010 "Batch Files (%d)")
        ((tjf:msb/is-mode? 'tjf:msb/bibtext-mode)      2011 "Bibtex Files (%d)")
        ;; C 2020
        ((tjf:msb/is-mode? 'tjf:msb/c-mode)            2020 "C Files  (%d)")
        ((tjf:msb/is-mode? 'csharp-mode)               2021 "C# Files  (%d)")
        ((tjf:msb/is-mode? tjf:msb/c++-mode)           2022 "C++ Files  (%d)")
        ((tjf:msb/is-mode? 'clips-mode)                2023 "CLIPS Files  (%d)")
        ((tjf:msb/is-mode? 'clips-log-mode)            2024 "CLIPS LogFiles  (%d)")
        ((tjf:msb/is-mode? 'tjf:msb/cmake-mode)        2025 "CMake Files  (%d)")
        ((tjf:msb/is-mode? 'tjf:msb/conf-mode)         2026 "Configuration Files  (%d)")
        ((tjf:msb/is-mode? 'css-mode)                  2027 "CSS Files  (%d)")
        ((tjf:msb/is-mode? 'csv-mode)                  2028 "CSV Files  (%d)")
        ((tjf:msb/is-mode? 'Custom-mode)               2029 "Custimization (%d)")
        ;; D 2030
        ((tjf:msb/is-mode? 'doctex-mode)               2030 "Doctex Files (%d)")
        ;; E 2040
        ((tjf:msb/is-mode? 'emacs-lisp-mode)           2040 "Elisp Files (%d)")
        ;; F 2050
        ((tjf:msb/is-mode? tjf:msb/fortran-mode)       2050 "Fortran Files (%d)")
        ;; G 2060
        ((tjf:msb/is-mode? tjf:msb/go-mode)            2060 "Go Files (%d)" )
        ((tjf:msb/is-mode? 'groovy-mode)               2061 "Groovy Files (%d)" )
        ;; H 2070
        ((tjf:msb/is-mode? tjf:msb/help-mode)          2070 "Help Files (%d)")
        ((tjf:msb/is-mode? tjf:msb/html-mode)          2071 "HTML Files (%d)")
        ;; I 2080
        ((tjf:msb/is-mode? 'icon-mode)                 2080 "Icon Files (%d)")
        ((tjf:msb/is-mode? 'idl-mode)                  2081 "IDL Files (%d)")
        ((tjf:msb/is-mode? 'image-mode)                2082 "Image Files (%d)")
        ;; J 2090
        ((tjf:msb/is-mode? tjf:msb/java-mode)          2090 "Java Files (%d)")
        ((tjf:msb/is-mode? tjf:msb/javascript-mode)    2091 "Javascript Files (%d)")
        ((tjf:msb/is-mode? tjf:msb/json-mode)          2092 "JSON Files (%d)")
        ((tjf:msb/is-mode? tjf:msb/julia-mode)         2093 "Julia Files (%d)")
        ;; K 3000
        ;; L 3010
        ((tjf:msb/is-mode? 'latex-mode)                3010 "Latex Files (%d)")
        ((tjf:msb/is-mode? 'lisp-mode)                 3011 "Lisp Files (%d)")
        ((tjf:msb/is-mode? 'log-mode)                  3012 "Log Files (%d)")
        ((tjf:msb/is-mode? 'lua-mode)                  3013 "Lua Files (%d)")
        ;; M 3020
        ((tjf:msb/is-mode? tjf:msb/make-mode)          3020 "Make Files (%d)")
        ((tjf:msb/is-mode? 'Man-mode)                  3021 "Manuals (%d)")
        ((tjf:msb/is-mode? 'matlab-mode)               3022 "MATLAB Files (%d)")
        ;; N 3030
        ;; O 3040
        ((tjf:msb/is-mode? 'org-mode)                  3040 "Org Files (%d)")
        ;; P 3050
        ((tjf:msb/is-mode? tjf:msb/package-mode)       3050 "Packages (%d)")
        ((tjf:msb/is-mode? 'pascal-mode)               3051 "Pascal (%d)")
        ((tjf:msb/is-mode? tjf:msb/perl-mode)          3052 "Perl Files (%d)")
        ((tjf:msb/is-mode? 'perl6-mode)                3053 "Rakudo Files (%d)")
        ((tjf:msb/is-mode? 'ps-mode)                   3054 "Postscript Files (%d)")
        ((tjf:msb/is-mode? 'python-mode)               3055 "Python Files (%d)")
        ;; Q 3060
        ;; R 3070
        ((tjf:msb/is-mode? tjf:msb/ruby-mode)          3070 "Ruby Files (%d)")
        ((tjf:msb/is-mode? tjf:msb/rust-mode)          3071 "Rust Files (%d)")
        ;; S 3080
        ((tjf:msb/is-mode? 'scheme-mode)               3080 "Scheme Files (%d)")
        ((tjf:msb/is-mode? tjf:msb/sh-script-mode)     3082 "Shell Scripts (%d)")
        ((tjf:msb/is-mode? 'sql-mode)                  3083 "SQL Scripts (%d)")
        ;; T 3090
        ((tjf:msb/is-mode? 'tar-mode)                  3090 "Tar Files (%d)")
        ((tjf:msb/is-mode? 'tcl-mode)                  3091 "Tcl Files (%d)")
        ((tjf:msb/is-mode? 'texinfo-mode)              3092 "Texinfo Files (%d)")
        ((tjf:msb/is-mode? tjf:msb/text-mode)          3093 "Text Files (%d)")
        ;; U 4000
        ;; V 4010
        ((tjf:msb/is-mode? 'vhdl-mode)                 4010 "VHDL Files (%d)")
        ;; W 4020
        ;; X 4030
        ((tjf:msb/is-mode? tjf:msb/text-mode)          4030 "XML Files (%d)")
        ;; Y 4040/
        ((tjf:msb/is-mode? tjf:msb/yaml-mode)          4040 "YAML Files (%d)")
        ;; Z 4050

        ;; OTHER 5000
        ((and buffer-file-name (string-match "^\\.[^/]*$" (buffer-name)) 'no-multi) 5000 "Hidden Files (%d)")
        ((and buffer-file-name 'no-multi)                                           5001 "Other Files (%d)")
        ((and (string-match "^copy of " (buffer-name)) 'no-multi)                   5002 "Copy Buffers (%d)")
        ((and (eq major-mode 'compilation-mode) 'no-multi)                          5010 "Compilation Log (%d)")
        ((and msb-display-invisible-buffers-p (msb-invisible-buffer-p) 'no-multi)   5020 "Invisible Buffers (%d)")
        ;; Catchup for all non-file buffers
        ('no-multi                                                                  5099 "Other Buffers (%d)")))

(msb-mode)

(setq msb-menu-cond msb--u-menus)

;;
(message "Loading tjf-msb...done")
(provide 'tjf-msb)

;;; tjf-msb.el ends here
