;;; package-install.el --- [description] -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2020-2020 Thomas Fontaine

;; Author: Thomas Fontaine
;; Date:   13-Jun-2020

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

(message "Configuring from package-install...")
;;
;;
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-refresh-contents)

(package-install 'anzu)
(package-install 'async)
(package-install 'bm)
(package-install 'clean-aindent-mode)
(package-install 'ccls)
(package-install 'company)
(package-install 'cuda-mode)
(package-install 'dash)
(package-install 'diminish)
(package-install 'eglot)
(package-install 'ergoemacs-mode)
(package-install 'f)
(package-install 'filladapt)
(package-install 'flycheck)
(package-install 'flycheck-pos-tip)
(package-install 'fringe-helper)
(package-install 'git-gutter)
(package-install 'git-gutter-fringe)
(package-install 'groovy-mode)
(package-install 'helpful)
(package-install 'highlight-operators)
(package-install 'json-mode)
(package-install 'langtool)
(package-install 'loccur)
(package-install 'lua-mode)
(package-install 'magit)
(package-install 'matlab-mode)
(package-install 'minions)
(package-install 'modern-cpp-font-lock)
(package-install 'mic-paren)
;; (package-install 'org-mode)		;
(package-install 'paradox)
;; (package-install 'perl6-mode)
(package-install 'popwin)
(package-install 'powerline)
(package-install 'powerthesaurus)
;; (package-install 'pretty-column)
(package-install 'projectile)
(package-install 'python)
(package-install 'rainbow-delimiters)
(package-install 's)
(package-install 'sdcv)
(package-install 'shift-number)
(package-install 'smartparens)
(package-install 'smooth-scrolling)
(package-install 'tabbar)
(package-install 'treemacs)
(package-install 'treemacs-magit)
(package-install 'treemacs-projectile)
(package-install 'undo-tree)
(package-install 'volatile-highlights)
(package-install 'whitespace)
(package-install 'ws-butler)
(package-install 'yaml-mode)


(message "Loading package-install...done")

;;; package-install.el ends here
