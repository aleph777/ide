;;; tjf-python.el --- Python mode support -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2022 Tom Fontaine

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

;; Revision: 23-Jun-2016 Removed globally set `semantic-mode'
;;           03-Feb-2021 ‘tjf’ overhaul
;;           16-Apr-2022 Added ‘tjf:python/convert’
;;           20-Apr-2022 Added ‘tjf:python/insert-license’
;;           21-Apr-2022 Updated ‘tjf:python/insert-me’
;;


;;; Code:

(require 'python)
(require 'flycheck)
(require 'smartparens-python)

(message "Loading tjf-python...")

(defvar tjf:python/shebang     "#!/usr/bin/env python3\n# -*-Python-*-\n\n")

;;
(defun tjf:python/convert ()
  "Convert the current file into Python."
  (interactive "*")
  (tjf:python/insert-module-skeleton)
  (set-auto-mode))

(defun tjf:python/insert-license ()
  "Insert the code license."
  (interactive "*")
  (insert-file-contents (concat tjf:user/dir-elisp "templates/license.py")))

(defun tjf:python/insert-me ()
  "Insert the `_ME_' variable declaration."
  (interactive "*")
  (insert "\n__me__ = os.path.basename(__file__)\n\n"))

(defun tjf:python/insert-script-header ()
  "Insert script boilerplate at point."
  (interactive "*")
  "Insert script boilerplate at point."
  (insert-file-contents (concat tjf:user/dir-elisp "templates/python-script-header.py"))
  (let* ((year   (format-time-string "%Y-%Y"))
         (author (user-full-name))
         (title  (file-name-nondirectory (buffer-name)))
         (date   (tjf:date/today tjf:date/dd-mon-yyyy)))
    (save-excursion
      (search-forward "<<<YEAR>>>")
      (replace-match year t)
      (search-forward "<<<AUTHOR>>>")
      (replace-match author t)
      (search-forward "<<<TITLE>>>")
      (replace-match title t)
      (search-forward "<<<DATE>>>")
      (replace-match date t))))

(defun tjf:python/insert-script-skeleton ()
  "Insert the python shebang and script boilerplate at the top of the file."
  (interactive "*")
  (save-excursion
    (tjf:python/insert-shebang)
    (tjf:python/insert-script-header)))

(defun tjf:python/insert-module-skeleton ()
  "Insert the python module boilerplate at the top of the file."
  (interactive "*")
  (insert-file-contents (concat tjf:user/dir-elisp "templates/python-module-header.py"))
  (let* ((year   (format-time-string "%Y-%Y"))
         (author (user-full-name))
         (title  (file-name-nondirectory (buffer-name)))
         (date   (tjf:date/today tjf:date/dd-mon-yyyy)))
    (save-excursion
      (search-forward "<<<YEAR>>>")
      (replace-match year t)
      (search-forward "<<<AUTHOR>>>")
      (replace-match author t)
      (search-forward "<<<TITLE>>>")
      (replace-match title t)
      (search-forward "<<<DATE>>>")
      (replace-match date t))))

(defun tjf:python/insert-shebang ()
  "Insert the python shebang at the top of the file."
  (interactive "*")
  (goto-char (point-min))
  (insert tjf:python/shebang))

(defun tjf:python/setup ()
  "Set up Python mode."
  (setq-local imenu-create-index-function #'python-imenu-create-index)
  (imenu-add-to-menubar "Navigate")
  (flycheck-mode))

(defun tjf:python/insert-usage ()
  "Insert the script usage code."
  (interactive "*")
  (tjf:python/insert-me)
  (insert-file-contents (concat tjf:user/dir-elisp "templates/python-script-usage.py")))

(easy-menu-define tjf:python/menu python-mode-map "Python Mode menu"
  '("Python"
    :help "Python-specific Features"
    ["Insert shebang"         tjf:python/insert-shebang         :active (tjf:flags/is-rw?)]
    ["Insert Script Header"   tjf:python/insert-script-header   :active (tjf:flags/is-rw?)]
    ["Insert Script Skeleton" tjf:python/insert-script-skeleton :active (tjf:flags/is-rw?)]
    ["Insert Module Skeleton" tjf:python/insert-module-skeleton :active (tjf:flags/is-rw?)]
    ["Insert License"         tjf:python/insert-license         :active (tjf:flags/is-rw?)]
    ["Insert _ME_"            tjf:python/insert-me              :active (tjf:flags/is-rw?)]
    ["Insert Script Usage"    tjf:python/insert-usage           :active (tjf:flags/is-rw?)]
    "---"
    ["Shift region left"  python-indent-shift-left  :active mark-active :help "Shift region left by a single indentation step"]
    ["Shift region right" python-indent-shift-right :active mark-active :help "Shift region right by a single indentation step"]
    "-"
    ["Mark def/class"     mark-defun         :help "Mark outermost definition around point"]
    "--"
    ("Skeletons")
    "---"
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
