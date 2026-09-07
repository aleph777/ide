;;; tjf-grammar.el --- treesitter grammar compiler -*-lexical-binding: t-*- ;; --*-no-byte-compile: t ;; *-Emacs-Lisp-*-

;;; Commentary:

;; (setq treesit-language-source-alist
;;       '((bash       . "https://github.com/tree-sitter/tree-sitter-bash")      "v0.25.1"))
;;         (c          . "https://github.com/tree-sitter/tree-sitter-c")         "v0.24.2"))
;;         (c-sharp    . "https://github.com/tree-sitter/tree-sitter-c-sharp")   "v0.20.0"))
;;         (cmake      . "https://github.com/uyha/tree-sitter-cmake")            "v0.4.1"))
;;         (cpp        . "https://github.com/tree-sitter/tree-sitter-cpp")       "v0.23.4"))
;;         (css        . "https://github.com/tree-sitter/tree-sitter-css")       "v0.25.0"))
;;         (html       . "https://github.com/tree-sitter/tree-sitter-html")      "v0.23.2"))
;;         (javascript . "https://github.com/tree-sitter/tree-sitter-javascript")"v0.25.0"))
;;         (json       . "https://github.com/tree-sitter/tree-sitter-json")      "v0.24.8"))
;;         (latex      . "https://github.com/latex-lsp/tree-sitter-latex")       "v0.3.0"))
;;         (make       . "https://github.com/alemuller/tree-sitter-make")        "main"))
;;         (markdown   . "https://github.com/MDeiml/tree-sitter-markdown")       "v0.5.2"))
;;         (pod        . "https://github.com/tree-sitter-perl/tree-sitter-pod")  "release"))
;;         (perl       . "https://github.com/tree-sitter-perl/tree-sitter-perl") "release"))
;;         (rust       . "https://github.com/tree-sitter/tree-sitter-rust")      "v0.24.2"))
;;         (python     . "https://github.com/tree-sitter/tree-sitter-python")    "v0.25.0"))
;;         (toml       . "https://github.com/tree-sitter/tree-sitter-toml")      "v0..5.1"))
;;         (yaml       . "https://github.com/ikatyang/tree-sitter-yaml")         "v0.5.0"))))

;; All :lang values
;; (message "[%s]" (mapcar #'treesit-auto-recipe-lang treesit-auto-recipe-list))
;; => (awk bash ...)

;; All :url values
;; (mapcar #'treesit-auto-recipe-url treesit-auto-recipe-list)

;; All :ext values
;; (mapcar #'treesit-auto-recipe-ext treesit-auto-recipe-list)

;; (seq-find (lambda (r) (eq (treesit-auto-recipe-lang r) 'bash))
;;           treesit-auto-recipe-list)

;; (let ((r (seq-find (lambda (r) (eq (treesit-auto-recipe-lang r) 'bash))
;;                    treesit-auto-recipe-list)))
;;   (treesit-auto-recipe-url r))

;; => "https://github.com/tree-sitter/tree-sitter-bash"

;;; Code:

(require 'cl-lib)
(require 'tjf-macro)
(require 'treesit)

(defvar tjf:grammar/languages)
(setq   tjf:grammar/languages '(awk
                                bash
                                c
                                c-sharp
                                clojure
                                cmake
                                commonlisp
                                cpp
                                css
                                dockerfile
                                gitcommit
                                go
                                html
                                java
                                javascript
                                json
                                julia
                                kotlin
                                lua
                                make
                                markdown
                                org
                                perl
                                proto
                                python
                                r
                                ruby
                                rust
                                sql
                                toml
                                typescript
                                vue
                                yaml
                                zig))

(defvar tjf:grammar/language-url-alist)
(setq   tjf:grammar/language-url-alist '((awk        . "https://github.com/Beaglefoot/tree-sitter-awk")
                                         (bash       . "https://github.com/tree-sitter/tree-sitter-bash")
                                         (c          . "https://github.com/tree-sitter/tree-sitter-c")
                                         (c-sharp    . "https://github.com/tree-sitter/tree-sitter-c-sharp")
                                         (clojure    . "https://github.com/sogaiu/tree-sitter-clojure")
                                         (cmake      . "https://github.com/uyha/tree-sitter-cmake")
                                         (cpp        . "https://github.com/tree-sitter/tree-sitter-cpp")
                                         (css        . "https://github.com/tree-sitter/tree-sitter-css")
                                         (dockerfile . "https://github.com/camdencheek/tree-sitter-dockerfile")
                                         (go         . "https://github.com/tree-sitter/tree-sitter-go")
                                         (html       . "https://github.com/tree-sitter/tree-sitter-html")
                                         (java       . "https://github.com/tree-sitter/tree-sitter-java")
                                         (javascript . "https://github.com/tree-sitter/tree-sitter-javascript")
                                         (json       . "https://github.com/tree-sitter/tree-sitter-json")
                                         (julia      . "https://github.com/tree-sitter/tree-sitter-julia")
                                         (kotlin     . "https://github.com/fwcd/tree-sitter-kotlin")
                                         (latex      . "https://github.com/latex-lsp/tree-sitter-latex")
                                         (lua        . "https://github.com/tree-sitter-grammars/tree-sitter-lua")
                                         (make       . "https://github.com/tree-sitter-grammars/tree-sitter-make")
                                         (markdown   . "https://github.com/tree-sitter-grammars/tree-sitter-markdown")
                                         (org        . "https://github.com/milisims/tree-sitter-org")
                                         (pod        . "https://github.com/tree-sitter-perl/tree-sitter-pod")
                                         (perl       . "https://github.com/tree-sitter-perl/tree-sitter-perl")
                                         (python     . "https://github.com/tree-sitter/tree-sitter-python")
                                         (r          . "https://github.com/r-lib/tree-sitter-r")
                                         (ruby       . "https://github.com/tree-sitter/tree-sitter-ruby")
                                         (rust       . "https://github.com/tree-sitter/tree-sitter-rust")
                                         (sql        . "https://github.com/DerekStride/tree-sitter-sql")
                                         (toml       . "https://github.com/tree-sitter/tree-sitter-toml")
                                         (typescript . "https://github.com/tree-sitter/tree-sitter-typescript")
                                         (yaml       . "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
                                         ))

(defun tjf:grammar/url (language)
  "Find LANGUAGE and return associated URL."
  (let ((r (seq-find (lambda (r) (eq (treesit-auto-recipe-lang r) language)) treesit-auto-recipe-list)))
    (treesit-auto-recipe-url r)))

(defun tjf:grammar/refresh-grammars ()
  "Find the latest releases of the listed grammars and install them."
  (interactive)
  (setq treesit-language-source-alist nil)
  (dolist (a tjf:grammar/language-url-alist)
        (let* ((lang (car a))
               (url  (cdr a))
               (slt  "show-last-tag ")
               (cmd  (concat slt url))
               (tag  (get-shell-output cmd))
               (pair `(,url ,tag))
               (item  `(,lang . ,pair))
               )
          (if tag
              (progn
                (add-to-list 'treesit-language-source-alist item)
                (treesit-install-language-grammar lang))
            (message "[%s: NONE]" lang))
          )))

(provide 'tjf-grammar)
;;; tjf-grammar.el ends here
