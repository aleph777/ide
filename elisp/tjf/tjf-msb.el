;;; tjf-msb.el --- Msb menu definition -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2021 Tom Fontaine

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

;; Revision: 26-Sep-2016 Added `clips-log-mode'
;;           13-Jun-2018 Added ‘helpful-mode’ to ‘Help’ entry
;;           24-Jun-2019 Added ‘groovy’
;;           03-Feb-2021 ‘tjf’ overhaul
;;

;;; Code:

(message "Loading tjf-msb...")
(require 'msb)
(require 'tjf-flags)

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

(defvar msb--u-menus '(((and (boundp 'server-buffer-clients) server-buffer-clients 'multi)         1010 "Clients (%d)")
                       ((and (get-buffer-process (current-buffer))                 'multi)         1020 "Processes (%d)")
                       ((and (boundp 'vc-mode) vc-mode                             'multi)         1030 "Version Control (%d)")
                       ((and buffer-file-name (buffer-modified-p)                  'multi)         1040 "Changed files (%d)")
                       ;; A 2000
                       ((eq major-mode 'ada-mode)                                                  2000 "Ada Files (%d)")
                       ((eq major-mode 'archive-mode)                                              2001 "Archive Files (%d)")
                       ((eq major-mode 'asm-mode)                                                  2002 "Assembly Files (%d)")
                       ((eq major-mode 'awk-mode)                                                  2003 "Awk Files (%d)")
                       ;; B 2010
                       ((eq major-mode 'bat-mode)                                                  2010 "Batch Files (%d)")
                       ((memq major-mode '(bibtex-mode bibtex-style-mode))                         2011 "Bibtex Files (%d)")
                       ;; C 2020
                       ((eq major-mode 'c-mode)                                                    2020 "C Files  (%d)")
                       ((eq major-mode 'csharp-mode)                                               2021 "C# Files  (%d)")
                       ((eq major-mode 'c++-mode)                                                  2022 "C++ Files  (%d)")
                       ((eq major-mode 'clips-mode)                                                2023 "CLIPS Files  (%d)")
                       ((eq major-mode 'clips-log-mode)                                            2024 "CLIPS LogFiles  (%d)")
                       ((memq major-mode '(conf-mode conf-unix-mode))                              2025 "Configuration Files  (%d)")
                       ((eq major-mode 'css-mode)                                                  2026 "CSS Files  (%d)")
                       ((eq major-mode 'Custom-mode)                                               2027 "Custimization (%d)")
                       ;; D 2030
                       ((eq major-mode 'doctex-mode)                                               2030 "Doctex Files (%d)")
                       ;; E 2040
                       ((eq major-mode 'emacs-lisp-mode)                                           2040 "Elisp Files (%d)")
                       ;; F 2050
                       ((memq major-mode '(fortran-mode f90-mode))                                 2050 "Fortran Files (%d)")
                       ;; G 2060
                       ((eq major-mode 'groovy-mode)                                               2060 "Groovy Files (%d)" )
                       ;; H 2070
                       ((memq major-mode '(help-mode helpful-mode))                                2070 "Help Files (%d)")
                       ((tjf:flags/is-html-mode?)                                                  2071 "HTML Files (%d)")
                       ;; I 2080
                       ((eq major-mode 'icon-mode)                                                 2080 "Icon Files (%d)")
                       ((eq major-mode 'idl-mode)                                                  2081 "IDL Files (%d)")
                       ((eq major-mode 'image-mode)                                                2082 "Image Files (%d)")
                       ;; J 2090
                       ((eq major-mode 'java-mode)                                                 2090 "Java Files (%d)")
                       ((tjf:flags/is-javascript-mode?)                                            2091 "Javascript Files (%d)")
                       ((eq major-mode 'json-mode)                                                 2092 "JSON Files (%d)")
                       ;; K 3000
                       ;; L 3010
                       ((eq major-mode 'latex-mode)                                                3010 "Latex Files (%d)")
                       ((eq major-mode 'lisp-mode)                                                 3011 "Lisp Files (%d)")
                       ((eq major-mode 'log-mode)                                                  3012 "Log Files (%d)")
                       ((eq major-mode 'lua-mode)                                                  3013 "Lua Files (%d)")
                       ;; M 3020
                       ((tjf:flags/is-make-mode?)                                                  3020 "Make Files (%d)")
                       ((eq major-mode 'Man-mode)                                                  3021 "Manuals (%d)")
                       ((eq major-mode 'matlab-mode)                                               3022 "MATLAB Files (%d)")
                       ;; N 3030
                       ;; O 3040
                       ((eq major-mode 'org-mode)                                                  3040 "Org Files (%d)")
                       ;; P 3050
                       ((memq major-mode '(package-menu-mode paradox-menu-mode))                   3050 "Packages (%d)")
                       ((eq major-mode 'pascal-mode)                                               3051 "Postscript Files (%d)")
                       ((tjf:flags/is-perl-mode?)                                                  3052 "Perl Files (%d)")
                       ((eq major-mode 'perl6-mode)                                                3053 "Perl6 Files (%d)")
                       ((eq major-mode 'ps-mode)                                                   3054 "Postscript Files (%d)")
                       ((eq major-mode 'python-mode)                                               3055 "Python Files (%d)")
                       ;; Q 3060
                       ;; R 3070
                       ((eq major-mode 'ruby-mode)                                                 3070 "Ruby Files (%d)")
                       ;; S 3080
                       ((eq major-mode 'scheme-mode)                                               3080 "Scheme Files (%d)")
                       ((tjf:flags/is-shell-script-mode?)                                          3082 "Shell Scripts (%d)")
                       ((eq major-mode 'sql-mode)                                                  3083 "SQL Scripts (%d)")
                       ;; T 3090
                       ((eq major-mode 'tar-mode)                                                  3090 "Tar Files (%d)")
                       ((eq major-mode 'tcl-mode)                                                  3091 "Tcl Files (%d)")
                       ((eq major-mode 'texinfo-mode)                                              3092 "Texinfo Files (%d)")
                       ((tjf:flags/is-text-mode?)                                                  3093 "Text Files (%d)")
                       ;; U 4000
                       ;; V 4010
                       ((eq major-mode 'vhdl-mode)                                                 4010 "VHDL Files (%d)")
                       ;; W 4020
                       ;; X 4030
                       ((tjf:flags/is-xml-mode?)                                                   4030 "XML Files (%d)")
                       ;; Y 4040
                       ((eq major-mode 'yaml-mode)                                                 4040 "YAML Files (%d)")
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
