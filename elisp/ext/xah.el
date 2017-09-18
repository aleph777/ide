(message "Loading xah...")
;;

(defun xah-insert-bracket-pair (*left-bracket *right-bracket)
  "Wrap or Insert a matching bracket and place cursor in between.

If there's a text selection, wrap brackets around it. Else, smartly decide wrap
or insert. (basically, if there's no char after cursor, just insert bracket
pair.)

*left-bracket ＆ *right-bracket are strings.

URL `http://ergoemacs.org/emacs/elisp_insert_brackets_by_pair.html'
Version 2015-04-19"
  (if (use-region-p)
      (progn
        (let (
              (-p1 (region-beginning))
              (-p2 (region-end)))
          (goto-char -p2)
          (insert *right-bracket)
          (goto-char -p1)
          (insert *left-bracket)
          (goto-char (+ -p2 2))))
    (progn ; no text selection
      (if
          (or
           (looking-at "[^-_[:alnum:]]")
           (eq (point) (point-max)))
          (progn
            (insert *left-bracket *right-bracket)
            (search-backward *right-bracket ))
        (progn
          (let (-p1 -p2)
            ;; basically, want all alphanumeric, plus hyphen and underscore, but don't want space or punctuations. Also want chinese.
            ;; 我有一帘幽梦，不知与谁能共。多少秘密在其中，欲诉无人能懂。
            (skip-chars-backward "-_[:alnum:]")
            (setq -p1 (point))
            (skip-chars-forward "-_[:alnum:]")
            (setq -p2 (point))
            (goto-char -p2)
            (insert *right-bracket)
            (goto-char -p1)
            (insert *left-bracket)
            (goto-char (+ -p2 (length *left-bracket)))))))))

;; Now we (define) the commands:

(defun xah-insert-paren ()
  (interactive "*")
  (xah-insert-bracket-pair "(" ")") )

(defun xah-insert-bracket ()
  (interactive "*")
  (xah-insert-bracket-pair "[" "]") )

(defun xah-insert-brace ()
  (interactive "*")
  (xah-insert-bracket-pair "{" "}") )

(defun xah-insert-lt ()
  (interactive "*")
  (xah-insert-bracket-pair "<" ">") )

(defun xah-insert-tag ()
  (interactive "*")
  (xah-insert-bracket-pair "<" "/>") )

(defun xah-insert-emacs-quote ()
  (interactive "*")
  (xah-insert-bracket-pair "‘" "’") )

(defun xah-insert-double-quote ()
  (interactive "*")
  (xah-insert-bracket-pair "\"" "\"") )

(defun xah-insert-single-quote ()
  (interactive "*")
  (xah-insert-bracket-pair "'" "'") )

(defun xah-comment-dwim ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line.

URL `http://ergoemacs.org/emacs/emacs_toggle_comment_by_line.html'
Version 2016-10-25"
  (interactive "*")
  (if (region-active-p)
      (comment-dwim nil)
    (let ((-lbp (line-beginning-position))
          (-lep (line-end-position)))
      (if (eq -lbp -lep)
          (progn
            (comment-dwim nil))
        (if (eq (point) -lep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region -lbp -lep)
            (forward-line )))))))

(defun xah-some (*list)
  "return t if at least one element is true"
  (eval `(or ,@ *list)))

(defun xah-every (*list)
  "return t if at least one element is true"
  (eval `(and ,@ *list)));;; xah.el ends here

(defun xah-clean-empty-lines (&optional *begin *end *n)
  "Replace repeated blank lines to just 1.
Works on whole buffer or text selection, respects `narrow-to-region'.

*N is the number of newline chars to use in replacement.
If 0, it means lines will be joined.
By befault, *N is 2. It means, 1 visible blank line.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2017-01-27"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (when (not *begin)
    (setq *begin (point-min) *end (point-max)))
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (progn
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil "NOERROR")
          (replace-match (make-string (if *n *n 2) 10)))))))

(defun xah-clean-whitespace (&optional *begin *end)
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or text selection, respects `narrow-to-region'.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2016-10-15"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (when (not *begin)
    (setq *begin (point-min) *end (point-max)))
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (progn
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+\n" nil "NOERROR")
          (replace-match "\n")))
      (xah-clean-empty-lines (point-min) (point-max))
      (progn
        (goto-char (point-max))
        (while (equal (char-before) 32) ; char 32 is space
          (delete-char -1))))))

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.

URL ‘http://ergoemacs.org/emacs/modernization_upcase-word.html’ Version 2016-01-08"
  (interactive "*")
  (let (
        (deactivate-mark nil)
        ξp1 ξp2)
    (if (use-region-p)
        (setq ξp1 (region-beginning)
              ξp2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]")
        (setq ξp1 (point))
        (skip-chars-forward "[:alnum:]")
        (setq ξp2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region ξp1 ξp2)
      (put this-command 'state 1))
     ((equal 1 (get this-command 'state))
      (upcase-region ξp1 ξp2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region ξp1 ξp2)
      (put this-command 'state 0)))))

(message "Loading xah...done")
(provide 'xah)
