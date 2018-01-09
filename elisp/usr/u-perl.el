;;; u-perl.el --- cperl-mode support for GNU Emacs -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 1999-2018 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   15-Dec-1999

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

;; Revision: 22-Jun-2000 Deleted ‘usr-perl-menu’ defvar and used ‘easy-menu-define’ instead of ‘easy-menu-add-item’
;;           23-Jun-2000 Added ‘usr-perl-shebang’, ‘usr-perl-insert-shebang’, changed defun names to ‘usr-perl-...’
;;           26-Jun-2000 Modified ‘usr-perl-insert-script-version’ to use trailing \n for used modules
;;           17-Feb-2005 Modified ‘usr-perl-insert-script-version’ to use TJF instead of ACC
;;           18-Apr-2006 Changed ‘usr-perl-shebang’ to use /local/bin/perl
;;           01-Jun-2006 Changed ‘usr-perl-insert-script-version’ to use MDC instead of TJF
;;           22-Jun-2006 Added ‘usr-perl-bin’
;;                       Changed ‘usr-perl-shebang’ to use ‘usr-perl-bin’
;;           25-Aug-2006 Changed ‘usr-perl-insert-shebang’ to detect for remote file name and adjust shebang
;;           02-Feb-2010 Added exit message
;;           25-Mar-2015 Updated ‘usr-perl-insert’ functions and associated menu entries
;;           03-Apr-2015 Added ‘usr-perl-insert-me’ and associated menu entry
;;                       Updated ‘usr-perl-insert-usage’ to use ‘usr-perl-insert-me’
;;           14-Apr-2015 Added ‘usr-perl-insert-oo-module-template’ and ‘usr-perl-insert-fn-module-template’
;;                       Revised associated menu entries
;;           21-Apr-2015 Fixed regex in ‘usr-perl-insert-usage’
;;                       Updated ‘usr-perl-insert-me’ to use ‘$__ME__’
;;           09-Dec-2015 Set TAB to ‘indent-for-tab-command’
;;           17-Jan-2016 Updated for new standard user interface
;;           02-Feb-2016 Added ‘neotree’
;;           23-Feb-2016 Added ‘cperl-init-faces’
;;                       Removed ‘neotree’
;;           28-Feb-2016 Refactored as ‘u-perl’
;;           03-Mar-2016 Updated to use ‘yasnippet’
;;           09-Mar-2016 Added use ‘v5.10’ to ‘perl-insert-script-header’
;;           18-Apr-2016 Updated for ‘use-package’
;;           23-Jun-2016 Removed globally set ‘semantic-mode’
;;           07-Sep-2016 Added ‘plsense’ support
;;           14-Nov-2016 Added ‘perl-init-faces’ to fix syntax highlighting
;;           18-Jan-2017 Removed ‘plsense’ support
;;                       Updated header formats
;;           19-Jan-2017 Disabled ‘abbrev-mode’
;;           03-Jul-2017 Set ‘flycheck-perl-include-path’ in ‘perl-setup’
;;

;;; Code:

(message "Loading u-perl...")
(require 'cperl-mode)
(require 'u-frame)
(require 'u-date)
(require 'u-variables)
;; (require 'plsense)

(defvar perl-which       "/usr/bin/perl")
(defvar perl-shebang     (concat "#!" perl-which " -w    # -*-Perl-*-\n"))
(defvar perl-min-version "5.010")

(defun perl-setup()
  "`eval-after-load' target for perl-mode."
  (imenu-add-to-menubar "Navigate")
  (abbrev-mode -1)
  (let ((perllib (getenv "PERLLIB")))
    (unless perllib
      (setq perllib (concat user-dir-home "lib:" user-dir-home "local/lib")))
    (setq flycheck-perl-include-path (split-string perllib ":")))
  (flycheck-mode))

(defun perl-insert-shebang ()
  "Insert the perl shebang at the top of the file."
  (interactive "*")
  (goto-char (point-min))
  (insert perl-shebang))

(defun perl-insert-script-header ()
  "Insert script boilerplate at point."
  (interactive "*")
  (insert-file-contents (concat user-dir-home "elisp/templates/perl-script-header.pl"))
  (let* ((year   (format-time-string "%Y-%Y"))
         (author (user-full-name))
         (title  (file-name-nondirectory (buffer-name)))
         (date   (get-dd-mon-yyyy)))
    (save-excursion
      (search-forward "<<<YEAR>>>")
      (replace-match year t)
      (search-forward "<<<AUTHOR>>>")
      (replace-match author t)
      (search-forward "<<<TITLE>>>")
      (replace-match title t)
      (search-forward "<<<AUTHOR>>>")
      (replace-match author t)
      (search-forward "<<<DATE>>>")
      (replace-match date t))))

(defun perl-insert-script-skeleton ()
  "Insert the perl shebang and script boilerplate at the top of the file."
  (interactive "*")
  (save-excursion
    (perl-insert-shebang)
    (perl-insert-script-header)))

(defun perl-insert-me ()
  "Insert the `_ME_' variable declaration."
  (interactive "*")
  (insert "use constant _ME_ => $0 =~ m=([^/]+)$=;\n\n"))

(defun perl-insert-usage ()
  "Insert the script usage code."
  (interactive "*")
  (perl-insert-me)
  (insert "if((@ARGV == 0) || ($ARGV[0] =~ /(?^:^(?:-(?:(?:-(?:h(?:elp)?|\\?)|h(?:elp)?)$|\\?)|\\?$))/))\n")
  (insert "{\n")
  (insert "  print \"\\nUsage: $me\\n\\n\";\n")
  (insert "  exit;\n")
  (insert "}\n"))

;;;###autoload
(defun convert-to-perl ()
  "Convert the current file into a Perl script."
  (interactive "*")
  (perl-insert-script-skeleton)
  (set-auto-mode)
  (reset-frame-size))

(defun perl-fill-out-template ()
  "Replace template fields with the associated data."
  (let* ((dirbase   (file-name-nondirectory (directory-file-name default-directory)))
         (modbase   (replace-regexp-in-string ".pm$" "" (file-name-nondirectory (buffer-file-name))))
         (package   (if (not (equal dirbase "lib")) (concat dirbase "::" modbase) modbase))
         (year   (format-time-string "%Y-%Y"))
         (author (user-full-name))
         (date   (get-dd-mon-yyyy)))
    (search-forward "<<<PACKAGE>>>")
    (replace-match package t)
    (search-forward "<<<YEAR>>>")
    (replace-match year t)
    (search-forward "<<<AUTHOR>>>")
    (replace-match author t)
    (search-forward "<<<AUTHOR>>>")
    (replace-match author t)
    (search-forward "<<<DATE>>>")
    (replace-match date t)
    (if (search-forward "<<<PACKAGE>>>" (point-max) t)
        (replace-match package t))
    ))

(defun perl-insert-oo-module-template ()
  "Insert a template for an Object-Oriented module."
  (interactive "*")
  (goto-char (point-min))
  (save-excursion
    (insert-file-contents (concat user-dir-home "elisp/templates/perl-oo-module.pm")))
  (perl-fill-out-template))

(defun perl-insert-fn-module-template ()
  "Insert a template for an Functional module."
  (interactive "*")
  (goto-char (point-min))
  (save-excursion
    (insert-file-contents (concat user-dir-home "elisp/templates/perl-fn-module.pm")))
  (perl-fill-out-template))

(defun perl-insert-module-header ()
  "Insert a header for a Perl module."
  (interactive "*")
  (save-excursion
    (insert-file-contents (concat user-dir-home "elisp/templates/perl-module-header.pm")))
  (perl-fill-out-template))

(defun perl-indent-function ()
  "Indentation function for `perl-mode'."
  (interactive "*")
  (save-excursion
    (beginning-of-defun)
    (cperl-indent-exp)))

(defun perl-init-faces ()
  "Replace `cperl-init-faces' in order to (wastefully) fix highlightling."
  (condition-case errs
      (progn
	(require 'font-lock)
	(and (fboundp 'font-lock-fontify-anchored-keywords)
	     (featurep 'font-lock-extra)
	     (message "You have an obsolete package `font-lock-extra'.  Install `choose-color'."))
	(let (t-font-lock-keywords t-font-lock-keywords-1 font-lock-anchored)
	  (if (fboundp 'font-lock-fontify-anchored-keywords)
	      (setq font-lock-anchored t))
	  (setq
	   t-font-lock-keywords
	   (list
	    `("[ \t]+$" 0 ',cperl-invalid-face t)
	    (cons
	     (concat
	      "\\(^\\|[^$@%&\\]\\)\\<\\("
	      (mapconcat
	       'identity
	       '("if" "until" "while" "elsif" "else" "unless" "for"
		 "foreach" "continue" "exit" "die" "last" "goto" "next"
		 "redo" "return" "local" "exec" "sub" "do" "dump" "use" "our"
		 "require" "package" "eval" "my" "BEGIN" "END" "CHECK" "INIT")
	       "\\|")			; Flow control
	      "\\)\\>") 2)		; was "\\)[ \n\t;():,|&]"
					; In what follows we use `type' style
					; for overwritable builtins
	    (list
	     (concat
	      "\\(^\\|[^$@%&\\]\\)\\<\\("
	      ;; "CORE" "__FILE__" "__LINE__" "abs" "accept" "alarm"
	      ;; "and" "atan2" "bind" "binmode" "bless" "caller"
	      ;; "chdir" "chmod" "chown" "chr" "chroot" "close"
	      ;; "closedir" "cmp" "connect" "continue" "cos" "crypt"
	      ;; "dbmclose" "dbmopen" "die" "dump" "endgrent"
	      ;; "endhostent" "endnetent" "endprotoent" "endpwent"
	      ;; "endservent" "eof" "eq" "exec" "exit" "exp" "fcntl"
	      ;; "fileno" "flock" "fork" "formline" "ge" "getc"
	      ;; "getgrent" "getgrgid" "getgrnam" "gethostbyaddr"
	      ;; "gethostbyname" "gethostent" "getlogin"
	      ;; "getnetbyaddr" "getnetbyname" "getnetent"
	      ;; "getpeername" "getpgrp" "getppid" "getpriority"
	      ;; "getprotobyname" "getprotobynumber" "getprotoent"
	      ;; "getpwent" "getpwnam" "getpwuid" "getservbyname"
	      ;; "getservbyport" "getservent" "getsockname"
	      ;; "getsockopt" "glob" "gmtime" "gt" "hex" "index" "int"
	      ;; "ioctl" "join" "kill" "lc" "lcfirst" "le" "length"
	      ;; "link" "listen" "localtime" "lock" "log" "lstat" "lt"
	      ;; "mkdir" "msgctl" "msgget" "msgrcv" "msgsnd" "ne"
	      ;; "not" "oct" "open" "opendir" "or" "ord" "pack" "pipe"
	      ;; "quotemeta" "rand" "read" "readdir" "readline"
	      ;; "readlink" "readpipe" "recv" "ref" "rename" "require"
	      ;; "reset" "reverse" "rewinddir" "rindex" "rmdir" "seek"
	      ;; "seekdir" "select" "semctl" "semget" "semop" "send"
	      ;; "setgrent" "sethostent" "setnetent" "setpgrp"
	      ;; "setpriority" "setprotoent" "setpwent" "setservent"
	      ;; "setsockopt" "shmctl" "shmget" "shmread" "shmwrite"
	      ;; "shutdown" "sin" "sleep" "socket" "socketpair"
	      ;; "sprintf" "sqrt" "srand" "stat" "substr" "symlink"
	      ;; "syscall" "sysopen" "sysread" "system" "syswrite" "tell"
	      ;; "telldir" "time" "times" "truncate" "uc" "ucfirst"
	      ;; "umask" "unlink" "unpack" "utime" "values" "vec"
	      ;; "wait" "waitpid" "wantarray" "warn" "write" "x" "xor"
	      "a\\(bs\\|ccept\\|tan2\\|larm\\|nd\\)\\|"
	      "b\\(in\\(d\\|mode\\)\\|less\\)\\|"
	      "c\\(h\\(r\\(\\|oot\\)\\|dir\\|mod\\|own\\)\\|aller\\|rypt\\|"
	      "lose\\(\\|dir\\)\\|mp\\|o\\(s\\|n\\(tinue\\|nect\\)\\)\\)\\|"
	      "CORE\\|d\\(ie\\|bm\\(close\\|open\\)\\|ump\\)\\|"
	      "e\\(x\\(p\\|it\\|ec\\)\\|q\\|nd\\(p\\(rotoent\\|went\\)\\|"
	      "hostent\\|servent\\|netent\\|grent\\)\\|of\\)\\|"
	      "f\\(ileno\\|cntl\\|lock\\|or\\(k\\|mline\\)\\)\\|"
	      "g\\(t\\|lob\\|mtime\\|e\\(\\|t\\(p\\(pid\\|r\\(iority\\|"
	      "oto\\(byn\\(ame\\|umber\\)\\|ent\\)\\)\\|eername\\|w"
	      "\\(uid\\|ent\\|nam\\)\\|grp\\)\\|host\\(by\\(addr\\|name\\)\\|"
	      "ent\\)\\|s\\(erv\\(by\\(port\\|name\\)\\|ent\\)\\|"
	      "ock\\(name\\|opt\\)\\)\\|c\\|login\\|net\\(by\\(addr\\|name\\)\\|"
	      "ent\\)\\|gr\\(ent\\|nam\\|gid\\)\\)\\)\\)\\|"
	      "hex\\|i\\(n\\(t\\|dex\\)\\|octl\\)\\|join\\|kill\\|"
	      "l\\(i\\(sten\\|nk\\)\\|stat\\|c\\(\\|first\\)\\|t\\|e"
	      "\\(\\|ngth\\)\\|o\\(c\\(altime\\|k\\)\\|g\\)\\)\\|m\\(sg\\(rcv\\|snd\\|"
	      "ctl\\|get\\)\\|kdir\\)\\|n\\(e\\|ot\\)\\|o\\(pen\\(\\|dir\\)\\|"
	      "r\\(\\|d\\)\\|ct\\)\\|p\\(ipe\\|ack\\)\\|quotemeta\\|"
	      "r\\(index\\|and\\|mdir\\|e\\(quire\\|ad\\(pipe\\|\\|lin"
	      "\\(k\\|e\\)\\|dir\\)\\|set\\|cv\\|verse\\|f\\|winddir\\|name"
	      "\\)\\)\\|s\\(printf\\|qrt\\|rand\\|tat\\|ubstr\\|e\\(t\\(p\\(r"
	      "\\(iority\\|otoent\\)\\|went\\|grp\\)\\|hostent\\|s\\(ervent\\|"
	      "ockopt\\)\\|netent\\|grent\\)\\|ek\\(\\|dir\\)\\|lect\\|"
	      "m\\(ctl\\|op\\|get\\)\\|nd\\)\\|h\\(utdown\\|m\\(read\\|ctl\\|"
	      "write\\|get\\)\\)\\|y\\(s\\(read\\|call\\|open\\|tem\\|write\\)\\|"
	      "mlink\\)\\|in\\|leep\\|ocket\\(pair\\|\\)\\)\\|t\\(runcate\\|"
	      "ell\\(\\|dir\\)\\|ime\\(\\|s\\)\\)\\|u\\(c\\(\\|first\\)\\|"
	      "time\\|mask\\|n\\(pack\\|link\\)\\)\\|v\\(alues\\|ec\\)\\|"
	      "w\\(a\\(rn\\|it\\(pid\\|\\)\\|ntarray\\)\\|rite\\)\\|"
	      "x\\(\\|or\\)\\|__\\(FILE__\\|LINE__\\|PACKAGE__\\)"
	      "\\)\\>") 2 'font-lock-type-face)
	    ;; In what follows we use `other' style
	    ;; for nonoverwritable builtins
	    ;; Somehow 's', 'm' are not auto-generated???
	    (list
	     (concat
	      "\\(^\\|[^$@%&\\]\\)\\<\\("
	      ;; "AUTOLOAD" "BEGIN" "CHECK" "DESTROY" "END" "INIT" "__END__" "chomp"
	      ;; "chop" "defined" "delete" "do" "each" "else" "elsif"
	      ;; "eval" "exists" "for" "foreach" "format" "goto"
	      ;; "grep" "if" "keys" "last" "local" "map" "my" "next"
	      ;; "no" "our" "package" "pop" "pos" "print" "printf" "push"
	      ;; "q" "qq" "qw" "qx" "redo" "return" "scalar" "shift"
	      ;; "sort" "splice" "split" "study" "sub" "tie" "tr"
	      ;; "undef" "unless" "unshift" "untie" "until" "use"
	      ;; "while" "y"
	      "AUTOLOAD\\|BEGIN\\|CHECK\\|cho\\(p\\|mp\\)\\|d\\(e\\(fined\\|lete\\)\\|"
	      "o\\)\\|DESTROY\\|e\\(ach\\|val\\|xists\\|ls\\(e\\|if\\)\\)\\|"
	      "END\\|for\\(\\|each\\|mat\\)\\|g\\(rep\\|oto\\)\\|INIT\\|if\\|keys\\|"
	      "l\\(ast\\|ocal\\)\\|m\\(ap\\|y\\)\\|n\\(ext\\|o\\)\\|our\\|"
	      "p\\(ackage\\|rint\\(\\|f\\)\\|ush\\|o\\(p\\|s\\)\\)\\|"
	      "q\\(\\|q\\|w\\|x\\|r\\)\\|re\\(turn\\|do\\)\\|s\\(pli\\(ce\\|t\\)\\|"
	      "ay\\|calar\\|tudy\\|ub\\|hift\\|ort\\)\\|t\\(r\\|ie\\)\\|"
	      "u\\(se\\|n\\(shift\\|ti\\(l\\|e\\)\\|def\\|less\\)\\)\\|"
	      "while\\|y\\|__\\(END\\|DATA\\)__" ;__DATA__ added manually
	      "\\|[sm]"			; Added manually
	      "\\)\\>") 2 'cperl-nonoverridable-face)
	    '("-[rwxoRWXOezsfdlpSbctugkTBMAC]\\>\\([ \t]+_\\>\\)?" 0
	      font-lock-function-name-face keep) ; Not very good, triggers at "[a-z]"
	    ;; This highlights declarations and definitions differently.
	    ;; We do not try to highlight in the case of attributes:
	    ;; it is already done by `cperl-find-pods-heres'
	    (list (concat "\\<sub"
			  cperl-white-and-comment-rex ; whitespace/comments
			  "\\([^ \n\t{;()]+\\)" ; 2=name (assume non-anonymous)
			  "\\("
			    cperl-maybe-white-and-comment-rex ;whitespace/comments?
			    "([^()]*)\\)?" ; prototype
			  cperl-maybe-white-and-comment-rex ; whitespace/comments?
			  "[{;]")
		  2 (if cperl-font-lock-multiline
			'(if (eq (char-after (cperl-1- (match-end 0))) ?\{ )
			     'font-lock-function-name-face
			   'font-lock-variable-name-face)
		      ;; need to manually set 'multiline' for older font-locks
		      '(progn
			 (if (< 1 (count-lines (match-beginning 0)
					       (match-end 0)))
			     (put-text-property
			      (+ 3 (match-beginning 0)) (match-end 0)
			      'syntax-type 'multiline))
			 (if (eq (char-after (cperl-1- (match-end 0))) ?\{ )
			     'font-lock-function-name-face
			   'font-lock-variable-name-face))))
	    '("\\<\\(package\\|require\\|use\\|import\\|no\\|bootstrap\\)[ \t]+\\([a-zA-z_][a-zA-z_0-9:]*\\)[ \t;]" ; require A if B;
	      2 font-lock-function-name-face)
	    '("^[ \t]*format[ \t]+\\([a-zA-z_][a-zA-z_0-9:]*\\)[ \t]*=[ \t]*$"
	      1 font-lock-function-name-face)
	    (cond ((featurep 'font-lock-extra)
		   '("\\([]}\\\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
		     (2 font-lock-string-face t)
		     (0 '(restart 2 t)))) ; To highlight $a{bc}{ef}
		  (font-lock-anchored
		   '("\\([]}\\\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
		     (2 font-lock-string-face t)
		     ("\\=[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
		      nil nil
		      (1 font-lock-string-face t))))
		  (t '("\\([]}\\\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
		       2 font-lock-string-face t)))
	    '("[[ \t{,(]\\(-?[a-zA-Z0-9_:]+\\)[ \t]*=>" 1
	      font-lock-string-face t)
	    '("^[ \t]*\\([a-zA-Z0-9_]+[ \t]*:\\)[ \t]*\\($\\|{\\|\\<\\(until\\|while\\|for\\(each\\)?\\|do\\)\\>\\)" 1
	      font-lock-constant-face)	; labels
	    '("\\<\\(continue\\|next\\|last\\|redo\\|goto\\)\\>[ \t]+\\([a-zA-Z0-9_:]+\\)" ; labels as targets
	      2 font-lock-constant-face)
	    (cond ((featurep 'font-lock-extra)
		   '("^[ \t]*\\(my\\|local\\|our\\)[ \t]*\\(([ \t]*\\)?\\([$@%*][a-zA-Z0-9_:]+\\)\\([ \t]*,\\)?"
		     (3 font-lock-variable-name-face)
		     (4 '(another 4 nil
				  ("\\=[ \t]*,[ \t]*\\([$@%*][a-zA-Z0-9_:]+\\)\\([ \t]*,\\)?"
				   (1 font-lock-variable-name-face)
				   (2 '(restart 2 nil) nil t)))
			nil t)))	; local variables, multiple
		  (font-lock-anchored
		   ;; 1=my_etc, 2=white? 3=(+white? 4=white? 5=var
		   `(,(concat "\\<\\(my\\|local\\|our\\)"
				  cperl-maybe-white-and-comment-rex
				  "\\(("
				     cperl-maybe-white-and-comment-rex
				  "\\)?\\([$@%*]\\([a-zA-Z0-9_:]+\\|[^a-zA-Z0-9_]\\)\\)")
		       (5 ,(if cperl-font-lock-multiline
				 'font-lock-variable-name-face
			       '(progn  (setq cperl-font-lock-multiline-start
					      (match-beginning 0))
					'font-lock-variable-name-face)))
		       (,(concat "\\="
				   cperl-maybe-white-and-comment-rex
				   ","
				   cperl-maybe-white-and-comment-rex
				   "\\([$@%*]\\([a-zA-Z0-9_:]+\\|[^a-zA-Z0-9_]\\)\\)")
			;; Bug in font-lock: limit is used not only to limit
			;; searches, but to set the "extend window for
			;; facification" property.  Thus we need to minimize.
			,(if cperl-font-lock-multiline
			     '(if (match-beginning 3)
				  (save-excursion
				    (goto-char (match-beginning 3))
				    (condition-case nil
					(forward-sexp 1)
				      (error
				       (condition-case nil
					   (forward-char 200)
					 (error nil)))) ; typeahead
				    (1- (point))) ; report limit
				(forward-char -2)) ; disable continued expr
			     '(if (match-beginning 3)
				  (point-max) ; No limit for continuation
				(forward-char -2))) ; disable continued expr
			,(if cperl-font-lock-multiline
			       nil
			     '(progn	; Do at end
				;; "my" may be already fontified (POD),
				;; so cperl-font-lock-multiline-start is nil
				(if (or (not cperl-font-lock-multiline-start)
					(> 2 (count-lines
					      cperl-font-lock-multiline-start
					      (point))))
				    nil
				  (put-text-property
				   (1+ cperl-font-lock-multiline-start) (point)
				   'syntax-type 'multiline))
				(setq cperl-font-lock-multiline-start nil)))
			(3 font-lock-variable-name-face))))
		  (t '("^[ \t{}]*\\(my\\|local\\|our\\)[ \t]*\\(([ \t]*\\)?\\([$@%*][a-zA-Z0-9_:]+\\)"
		       3 font-lock-variable-name-face)))
	    '("\\<for\\(each\\)?\\([ \t]+\\(my\\|local\\|our\\)\\)?[ \t]*\\(\\$[a-zA-Z_][a-zA-Z_0-9]*\\)[ \t]*("
	      4 font-lock-variable-name-face)
	    ;; Avoid $!, and s!!, qq!! etc. when not fontifying syntactically
	    '("\\(?:^\\|[^smywqrx$]\\)\\(!\\)" 1 font-lock-negation-char-face)
	    '("\\[\\(\\^\\)" 1 font-lock-negation-char-face prepend)))
	  (setq
	   t-font-lock-keywords-1
	   (and (fboundp 'turn-on-font-lock) ; Check for newer font-lock
		;; not yet as of XEmacs 19.12, works with 21.1.11
		(or
		 (not (featurep 'xemacs))
		 (string< "21.1.9" emacs-version)
		 (and (string< "21.1.10" emacs-version)
		      (string< emacs-version "21.1.2")))
		'(
		  ("\\(\\([@%]\\|\\$#\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)" 1
		   (if (eq (char-after (match-beginning 2)) ?%)
		       'cperl-hash-face
		     'cperl-array-face)
		   t)			; arrays and hashes
		  ("\\(\\([$@]+\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)[ \t]*\\([[{]\\)"
		   1
		   (if (= (- (match-end 2) (match-beginning 2)) 1)
		       (if (eq (char-after (match-beginning 3)) ?{)
			   'cperl-hash-face
			 'cperl-array-face) ; arrays and hashes
		     font-lock-variable-name-face) ; Just to put something
		   t)
		  ("\\(@\\|\\$#\\)\\(\\$+\\([a-zA-Z_:][a-zA-Z0-9_:]*\\|[^ \t\n]\\)\\)"
		   (1 cperl-array-face)
		   (2 font-lock-variable-name-face))
		  ("\\(%\\)\\(\\$+\\([a-zA-Z_:][a-zA-Z0-9_:]*\\|[^ \t\n]\\)\\)"
		   (1 cperl-hash-face)
		   (2 font-lock-variable-name-face))
		  )))
	  (if cperl-highlight-variables-indiscriminately
	      (setq t-font-lock-keywords-1
		    (append t-font-lock-keywords-1
			    (list '("\\([$*]{?\\sw+\\)" 1
				    font-lock-variable-name-face)))))
	  (setq cperl-font-lock-keywords-1
		(if cperl-syntaxify-by-font-lock
		    (cons 'cperl-fontify-update
			  t-font-lock-keywords)
		  t-font-lock-keywords)
		cperl-font-lock-keywords cperl-font-lock-keywords-1
		cperl-font-lock-keywords-2 (append
					   cperl-font-lock-keywords-1
					   t-font-lock-keywords-1)))
	(if (fboundp 'ps-print-buffer) (cperl-ps-print-init))
	(if (or (featurep 'choose-color) (featurep 'font-lock-extra))
	    (eval			; Avoid a warning
	     '(font-lock-require-faces
	       (list
		;; Color-light    Color-dark      Gray-light      Gray-dark Mono
		(list 'font-lock-comment-face
		      ["Firebrick"	"OrangeRed" 	"DimGray"	"Gray80"]
		      nil
		      [nil		nil		t		t	t]
		      [nil		nil		t		t	t]
		      nil)
		(list 'font-lock-string-face
		      ["RosyBrown"	"LightSalmon" 	"Gray50"	"LightGray"]
		      nil
		      nil
		      [nil		nil		t		t	t]
		      nil)
		(list 'font-lock-function-name-face
		      (vector
		       "Blue"		"LightSkyBlue"	"Gray50"	"LightGray"
		       (cdr (assq 'background-color ; if mono
				  (frame-parameters))))
		      (vector
		       nil		nil		nil		nil
		       (cdr (assq 'foreground-color ; if mono
				  (frame-parameters))))
		      [nil		nil		t		t	t]
		      nil
		      nil)
		(list 'font-lock-variable-name-face
		      ["DarkGoldenrod"	"LightGoldenrod" "DimGray"	"Gray90"]
		      nil
		      [nil		nil		t		t	t]
		      [nil		nil		t		t	t]
		      nil)
		(list 'font-lock-type-face
		      ["DarkOliveGreen"	"PaleGreen" 	"DimGray"	"Gray80"]
		      nil
		      [nil		nil		t		t	t]
		      nil
		      [nil		nil		t		t	t])
		(list 'font-lock-warning-face
		      ["Pink"		"Red"		"Gray50"	"LightGray"]
		      ["gray20"		"gray90"
							"gray80"	"gray20"]
		      [nil		nil		t		t	t]
		      nil
		      [nil		nil		t		t	t]
		      )
		(list 'font-lock-constant-face
		      ["CadetBlue"	"Aquamarine" 	"Gray50"	"LightGray"]
		      nil
		      [nil		nil		t		t	t]
		      nil
		      [nil		nil		t		t	t])
		(list 'cperl-nonoverridable-face
		      ["chartreuse3"	("orchid1" "orange")
		       nil		"Gray80"]
		      [nil		nil		"gray90"]
		      [nil		nil		nil		t	t]
		      [nil		nil		t		t]
		      [nil		nil		t		t	t])
		(list 'cperl-array-face
		      ["blue"		"yellow" 	nil		"Gray80"]
		      ["lightyellow2"	("navy" "os2blue" "darkgreen")
		       "gray90"]
		      t
		      nil
		      nil)
		(list 'cperl-hash-face
		      ["red"		"red"	 	nil		"Gray80"]
		      ["lightyellow2"	("navy" "os2blue" "darkgreen")
		       "gray90"]
		      t
		      t
		      nil))))
	  ;; Do it the dull way, without choose-color
	  (defvar cperl-guessed-background nil
	    "Display characteristics as guessed by cperl.")
	  (cperl-force-face font-lock-constant-face
			    "Face for constant and label names")
	  (cperl-force-face font-lock-variable-name-face
			    "Face for variable names")
	  (cperl-force-face font-lock-type-face
			    "Face for data types")
	  (cperl-force-face cperl-nonoverridable-face
			    "Face for data types from another group")
	  (cperl-force-face font-lock-warning-face
			    "Face for things which should stand out")
	  (cperl-force-face font-lock-comment-face
			    "Face for comments")
	  (cperl-force-face font-lock-function-name-face
			    "Face for function names")
	  (cperl-force-face cperl-hash-face
			    "Face for hashes")
	  (cperl-force-face cperl-array-face
			    "Face for arrays")
	  (if (and
	       (not (cperl-is-face 'cperl-array-face))
	       (cperl-is-face 'font-lock-emphasized-face))
	      (copy-face 'font-lock-emphasized-face 'cperl-array-face))
	  (if (and
	       (not (cperl-is-face 'cperl-hash-face))
	       (cperl-is-face 'font-lock-other-emphasized-face))
	      (copy-face 'font-lock-other-emphasized-face 'cperl-hash-face))
	  (if (and
	       (not (cperl-is-face 'cperl-nonoverridable-face))
	       (cperl-is-face 'font-lock-other-type-face))
	      (copy-face 'font-lock-other-type-face 'cperl-nonoverridable-face))
	  (let ((background
		 (if (boundp 'font-lock-background-mode)
		     font-lock-background-mode
		   'light))
		(face-list (and (fboundp 'face-list) (face-list))))
	    (defvar cperl-guessed-background
	      (if (and (boundp 'font-lock-display-type)
		       (eq font-lock-display-type 'grayscale))
		  'gray
		background)
	      "Background as guessed by CPerl mode")
	    (and (not (cperl-is-face 'font-lock-constant-face))
		 (cperl-is-face 'font-lock-reference-face)
		 (copy-face 'font-lock-reference-face 'font-lock-constant-face))
	    (if (cperl-is-face 'font-lock-type-face) nil
	      (copy-face 'default 'font-lock-type-face)
	      (cond
	       ((eq background 'light)
		(set-face-foreground 'font-lock-type-face
				     (if (x-color-defined-p "seagreen")
					 "seagreen"
				       "sea green")))
	       ((eq background 'dark)
		(set-face-foreground 'font-lock-type-face
				     (if (x-color-defined-p "os2pink")
					 "os2pink"
				       "pink")))
	       (t
		(set-face-background 'font-lock-type-face "gray90"))))
	    (if (cperl-is-face 'cperl-nonoverridable-face)
		nil
	      (copy-face 'font-lock-type-face 'cperl-nonoverridable-face)
	      (cond
	       ((eq background 'light)
		(set-face-foreground 'cperl-nonoverridable-face
				     (if (x-color-defined-p "chartreuse3")
					 "chartreuse3"
				       "chartreuse")))
	       ((eq background 'dark)
		(set-face-foreground 'cperl-nonoverridable-face
				     (if (x-color-defined-p "orchid1")
					 "orchid1"
				       "orange")))))
	    (if (cperl-is-face 'font-lock-variable-name-face) nil
	      (copy-face 'italic 'font-lock-variable-name-face))
	    (if (cperl-is-face 'font-lock-constant-face) nil
	      (copy-face 'italic 'font-lock-constant-face))))
	(setq cperl-faces-init t))
    (error (message "cperl-init-faces (ignored): %s" errs))))

(setq cperl-indent-region-fix-constructs nil cperl-hairy t
      cperl-style-alist (append cperl-style-alist '(("TJF"
                                                     (cperl-indent-level               .  2)
                                                     (cperl-brace-offset               .  0)
                                                     (cperl-continued-brace-offset     . -2)
                                                     (cperl-label-offset               . -2)
                                                     (cperl-extra-newline-before-brace .  t)
                                                     (cperl-merge-trailing-else        .  nil)
                                                     (cperl-continued-statement-offset .  2)))))

(cperl-set-style "TJF")
(cperl-init-faces)

(define-key cperl-mode-map [menu-bar] nil)

(easy-menu-define u-perl-menu cperl-mode-map "U-Perl"
  '("Perl"
    ["Beginning Of Function" beginning-of-defun :active t]
    ["End Of Function"       end-of-defun       :active t]
    ["Mark Function"         mark-defun         :active t]
    "---"
    ["Insert Shebang"         perl-insert-shebang         :active t]
    ["Insert Script Header"   perl-insert-script-header   :active t]
    ["Insert Script Skeleton" perl-insert-script-skeleton :active t]
    ["Insert _ME_"            perl-insert-me              :active t]
    ["Insert Script Usage"    perl-insert-usage           :active t]
    "---"
    ["Insert OO Module Template" perl-insert-oo-module-template :active t]
    ["Insert FN Module Template" perl-insert-fn-module-template :active t]
    ["Insert Module Header"      perl-insert-module-header      :active t]
    "---"
    ["Check Minimum Perl Version" (message (substring (shell-command-to-string (concat "check-minimum-version " (buffer-file-name))) 0 -1))]
    ))

(easy-menu-define u-perl-build-menu cperl-mode-map "Perl Build"
  '("Build"
    ["Syntax Check" (compile (concat "export PERLLIB=$HOME/lib:$HOME/local/lib;/usr/bin/perl -c "  (file-name-nondirectory (buffer-file-name)))) :active t]
    ["Critique"     (compile (concat "critique " (file-name-nondirectory (buffer-file-name)))) :active t]
    ))

(cperl-define-key "\t" 'indent-for-tab-command)
(perl-init-faces)

;; (plsense-config-default)
;; (plsense-server-start)

(add-hook 'cperl-mode-hook 'perl-setup)
;;
(message "Loading u-perl...done")
(provide 'u-perl)

;;; u-perl.el ends here
