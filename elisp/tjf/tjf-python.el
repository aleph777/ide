;;; tjf-python.el --- Python mode support -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2023 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   19-Apr-2016

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

;; Revision: 23-Jun-2016 removed globally set `semantic-mode'
;;           03-Feb-2021 ‘tjf’ overhaul
;;           16-Apr-2022 added ‘tjf:python/convert’
;;           20-Apr-2022 added ‘tjf:python/insert-license’
;;           21-Apr-2022 updated ‘tjf:python/insert-me’
;;           13-Sep-2022 added ‘python-completion-at-point’
;;           06-Jun-2023 changed from ‘tjf:python/setup’ to ‘tjf:python/hook’ and ‘tjf:python/config’
;;                       fixed shebang
;;

;;; Code:

(require 'python)
(require 'flycheck)
(require 'smartparens-python)
(require 'tjf-edit)

(message "Loading tjf-python...")

(defvar tjf:python/coding)
(setq   tjf:python/coding "# -*-coding: utf-8-*- ; -*-Python-*-")

(defvar tjf:python/shebang)
(setq   tjf:python/shebang (concat "#!/usr/bin/env -S  " tjf:python/coding))

(defvar tjf:python/me)
(setq   tjf:python/me "__me__ = os.path.basename(__file__)")

(defvar tjf:python/module-template)
(setq   tjf:python/module-template (concat tjf:user/dir-elisp "templates/python-module-header-work.py"))

(defvar tjf:python/script-template)
(setq   tjf:python/script-template (concat tjf:user/dir-elisp "templates/python-script-header-work.py"))

(defun tjf:python/config ()
  "Python mode config function.")

;;;###autoload
(defun tjf:python/convert ()
  "Convert the current file into Python."
  (interactive "*")
  (insert (concat tjf:python/coding "\n\n"))
  (set-auto-mode))

(defun tjf:python/hook ()
  "Python mode hook function."
  (setq-local completion-at-point-functions '(eglot-completion-at-point
                                              anaconda-mode-complete
                                              python-completion-at-point
                                              cape-keyword
                                              cape-dabbrev
                                              cape-file
                                              cape-history))
  (flycheck-mode -1)
  (setq-local imenu-create-index-function #'python-imenu-create-index) ;; only language where this is defined
  (imenu-add-to-menubar "Navigate"))

(defun tjf:python/insert-header (header)
  "Insert the python module boilerplate HEADER at the top of the file."
  (goto-char (point-min))
  (insert-file-contents header)
  (let* ((year   (format-time-string "%Y"))
         (author (user-full-name))
         (title  (file-name-nondirectory (buffer-name)))
         (date   (tjf:date/today tjf:date/dd-mon-yyyy)))
    (tjf:edit/fill-skeleton "<<<SHEBANG>>>" tjf:python/shebang)
    (tjf:edit/fill-skeleton "<<<YEAR>>>"    year)
    (tjf:edit/fill-skeleton "<<<AUTHOR>>>"  author)
    (tjf:edit/fill-skeleton "<<<TITLE>>>"   title)
    (tjf:edit/fill-skeleton "<<<DATE>>>"    date)))

(defun tjf:python/insert-me ()
  "Insert the `_ME_' variable declaration."
  (interactive "*")
  (insert (concat "\n" tjf:python/me "\n\n")))

(defun tjf:python/insert-module-header ()
  "Insert the python module boilerplate at the top of the file."
  (interactive "*")
  (tjf:python/insert-header tjf:python/module-template))

(defun tjf:python/insert-script-header ()
  "Insert script boilerplate at point."
  (interactive "*")
  (tjf:python/insert-header tjf:python/script-template))

(defun tjf:python/insert-shebang ()
  "Insert the python shebang at the top of the file."
  (interactive "*")
  (goto-char (point-min))
  (insert (concat tjf:python/shebang "\n\n")))

(defun tjf:python/insert-usage ()
  "Insert the script usage code."
  (interactive "*")
  (insert-file-contents (concat tjf:user/dir-elisp "templates/python-script-usage.py")))

;;
(easy-menu-define tjf:python/menu python-mode-map "Python Mode menu"
  '("Python"
    :help "Python-specific Features"
    ["Insert shebang"         tjf:python/insert-shebang         :active (tjf:flags/is-rw?)]
    ["Insert Script Header"   tjf:python/insert-script-header   :active (tjf:flags/is-rw?)]
    ["Insert Module Skeleton" tjf:python/insert-module-header   :active (tjf:flags/is-rw?)]
    ["Insert _ME_"            tjf:python/insert-me              :active (tjf:flags/is-rw?)]
    ["Insert Script Usage"    tjf:python/insert-usage           :active (tjf:flags/is-rw?)]
    "---"
    ["Shift region left"  python-indent-shift-left  :active mark-active :help "Shift region left by a single indentation step"]
    ["Shift region right" python-indent-shift-right :active mark-active :help "Shift region right by a single indentation step"]
    "-"
    ["Mark def/class"     mark-defun         :help "Mark outermost definition around point"]
    "--"
    ["Start interpreter" run-python                   :help "Run inferior Python process in a separate buffer"]
    ["Switch to shell"   python-shell-switch-to-shell :help "Switch to running inferior Python process"]
    ["Eval string"       python-shell-send-string     :help "Eval string in inferior Python session"]
    ["Eval buffer"       python-shell-send-buffer     :help "Eval buffer in inferior Python session"]
    ["Eval statement"    python-shell-send-statement  :help "Eval statement in inferior Python session"]
    ["Eval region"       python-shell-send-region     :help "Eval region in inferior Python session"]
    ["Eval defun"        python-shell-send-defun      :help "Eval defun in inferior Python session"]
    ["Eval file"         python-shell-send-file       :help "Eval file in inferior Python session"]
    ["Debugger"          pdb                          :help "Run pdb under GUD"]
    "----"
    ["Check file"      python-check          :help "Check file for errors"]
    ["Help on symbol"  python-eldoc-at-point :help "Get help on symbol at point"]
    ["Complete symbol" completion-at-point   :help "Complete symbol before point"]))

;;
(message "Loading tjf-python...done")
(provide 'tjf-python)

;;; tjf-python.el ends here
