;;; u-cc.el --- cc-mode support for GNU Emacs -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 1999-2019 Tom Fontaine

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

;; Revision: 23-Jun-2000 Changed substatement-open to 0
;;                       Added `usr-cc-menu'
;;           26-Jun-2000 Added define-key to remove C menu from `c-mode-map'
;;           02-Feb-2010 Added exit message
;;           30-Aug-2012 Fixed for modern cc-mode
;;           26-Mar-2015 Added `usr-c-setup', `usr-c++-setup', and `usr-java-setup'
;;                       Now using (c-add-style c-style-alist) instead of directly setting `c-style-alist'
;;           06-May-2015 Added `usr-doc-string'
;;                       Added explicit `usr-java-setup'
;;           18-Aug-2015 Removed style "tjf"; changed to "bsd" with `c-basic-offset' of 4
;;           28-Sep-2015 Added `usr-cc-reformat-buffer' functionality
;;           20-Oct-2015 Added `usr-cpp-comment-region'
;;           04-Dec-2015 Fixed `usr-cpp-comment-region'
;;           15-Jan-2016 Added semantic/imenu support to `usr-c-setup', `usr-c++-setup', and `usr-java-setup'
;;           17-Jan-2016 Updated for new standard user interface
;;           02-Feb-2016 Added `neotree'
;;           12-Feb-2016 Updated C and C++ Build menus
;;           29-Feb-2016 Changed from `usr-' to `u-'
;;                       Removed`neotree'
;;           03-Mar-2016 Changed `setq' to `setq-local' in `u-c-setup'
;;                       Updated to use `yasnippet'
;;           09-Mar-2016 Fixed `u-c-setup' and `u-java-setup' to sort `imenu'
;;           18-Apr-2016 Updated for `use-package'
;;           03-May-2016 Unbound local setting for `C-d'
;;           23-Jun-2016 Removed globally set `semantic-mode'
;;           07-Sep-2016 Added support for `ac-clang'
;;           14-Jan-2017 Removed support for `ac-clang'
;;           21-Mar-2017 Added ‘u-cc-insert-boilerplate’
;;                       Added ‘u-cc-insert-header-skeleton’
;;                       Added ‘u-cc-insert-source-skeleton’
;;           28-Apr-2017 Updated ‘u-cc-insert-boilerplate’, ‘u-cc-insert-header-skeleton’, ‘u-cc-insert-source-skeleton’
;;                       Updated ‘u-cc-mode-menu-text’
;;           02-May-2017 Added ‘u-cc-insert-summary’
;;                       Updated ‘u-cc-mode-menu-text’
;;           24-Jan-2019 Added ‘Go to Definition’ to ‘u-cc-mode-menu-text’
;;                       Added ‘Format File’ to ‘u-cc-mode-menu-text’
;;                       Removed ‘u-cc-reformat-buffer’ in favor of ‘eglot-format’
;;           03-May-2019 Added KDRP C++ support
;;           18-Jun-2019 Turned off ‘flymake-mode’
;;

;;; Code:

(message "Loading u-cc...")
(require 'cc-mode)
(require 'flycheck)
(require 'eglot)
(require 'u-macro)
(require 'f)
(require 's)

(defvar u-cc-indent-program "indent ")

(defvar u-cc-indent-options (concat
                             "-i4 "   ; set indentation level to 4 spaces
                             "-cli4 " ; case label indent of 4 spaces
                             "-ci4 "  ; Continuation indent of 4 spaces
                             "-st "   ; write to STDOUT
                             "-bad "  ; blank lines after declarations
                             "-bap "  ; blank lines after procedures
                             "-bli0 " ; indent braces 0 spaces
                             "-ncs "  ; do not put a space after cast operators
                             "-npcs " ; do not put space after the function in function calls
                             "-nut "  ; use spaces instead of tabs
                             "-npsl " ; put the type of a procedure on the same line as its name
                             "-fca "  ; format all comments
                             "-lc79 " ; set maximum line length for comment formatting to 79
                             "-fc1 "  ; format comments in the first column
                             "-sob "  ; swallow optional blank lines
                             "-lp "   ; line up continued lines at parentheses
                             "-nsai " ; do not put a space after every if
                             "-nsaf " ; do not put a space after every for
                             "-nsaw " ; do not put a space after every while
                             ))

(defvar u-cc-indent-command (concat u-cc-indent-program u-cc-indent-options))

(defvar u-cc-mode-menu-text
  '(
    ["Insert Header File Skeleton" u-cc-insert-header-skeleton   :active (is-rw?)]
    ["Insert Source File Skeleton" u-cc-insert-source-skeleton   :active (is-rw?)]
    ["Insert Boilerplate"          u-cc-insert-boilerplate       :active (is-rw?)]
    ["Insert Summary"              u-cc-insert-summary           :active (is-rw?)]
    ["Format File"                 eglot-format                  :active (is-rw?)]
    ["Flush Region Pragmas"       (flush-lines "pragma.+region") :active (is-rw?)] ;; what was this for?
    "---"
    ["Go to Definition" xref-find-definitions]
    "---"
    ["Beginning Of Function" beginning-of-defun :active t]
    ["End Of Function"       end-of-defun       :active t]
    ["Mark Function"         c-mark-function    :active t]
    "---"
    ["Fill Comment Paragraph"c-fill-paragraph :active t]
    ;;    ["Convert comment to docstring" u-docstring    :enable (or c++-mode java-mode)]
    "---"
    ["Backward Statement" c-beginning-of-statement :active t]
    ["Forward  Statement" c-end-of-statement       :active t]
    "---"
    ["Up Conditional"       c-up-conditional       :active t]
    ["Backward Conditional" c-backward-conditional :active t]
    ["Forward  Conditional" c-forward-conditional  :active t]
    ))

;; (defvar tutorial--point-before-chkeys 0
;;   "Point before display of key changes.")
;; (make-variable-buffer-local 'tutorial--point-before-chkeys)

(defvar u-cc-cflags "-Wall -g")
(make-variable-buffer-local 'u-cc-cflags)

(defvar u-cc-ldflags "-lm -pthread")
(make-variable-buffer-local 'u-cc-ldflags)

(defvar u-cc-makeflags "")
(make-variable-buffer-local 'u-cc-makeflags)

(defun u-c-setup()
  "C-mode setup function."
  (abbrev-mode -1)
  (flymake-mode -1)
  (flycheck-mode)
  (imenu-add-to-menubar "Navigate")
  ;;
  (setq flycheck-gcc-language-standard "c11"
        u-cc-cflags (concat " -std=c11 " u-cc-cflags))
  (setq-local comment-start "// ")
  (setq-local comment-end ""))

(defun u-c++-setup ()
  "C++-mode setup function."
  (abbrev-mode -1)
  (flymake-mode -1)
  (flycheck-mode)
  (imenu-add-to-menubar "Navigate")
  ;;
  (setq flycheck-gcc-language-standard "c++11"
        u-cc-cflags (concat "-std=c++11 " u-cc-cflags)))

(defun u-java-setup ()
  "Java-mode setup function."
  (abbrev-mode -1)
  (flymake-mode -1)
  (flycheck-mode -1)
  (imenu-add-to-menubar "Navigate")
  ;;
  )

(defun u-docstring ()
  "Convert C++-style comments '^ *//' to a docstring."
  (interactive "*")
  (let* ((exp "^ *// *")
         (beg (progn
                (beginning-of-line)
                (and (looking-at exp) (progn (while (looking-at exp) (forward-line -1)) (forward-line 1) (point)))))
         (end (and beg (progn (while (looking-at exp) (forward-line 1)) (point)))))
    (if beg
        (let ((opn " /**\n")
              (com "  * ")
              (cls "  */\n"))
          (goto-char end)
          (insert cls)
          (save-excursion
            (goto-char end)
            (while (re-search-backward exp beg t)
              (replace-match com nil nil))
            (goto-char beg)
            (insert opn))
          (indent-region beg (point)))
      (error "Not in an eligible comment"))))

(defun u-cc-set-cflags ()
  "Allow the user to set `CFLAGS'."
  (interactive)
  (let ((flags (read-shell-command "Compiler Flags: " u-cc-cflags)))
    (unless (equal flags u-cc-cflags)
      (setq u-cc-cflags flags))))

(defun u-cc-set-ldflags ()
  "Allow the user to set `LDFLAGS'."
  (interactive)
  (let ((flags (read-shell-command "Linker Flags: " u-cc-ldflags)))
    (unless (equal flags u-cc-ldflags)
      (setq u-cc-ldflags flags))))

(defun u-cc-set-makeflags ()
  "Allow the user to set `command-line' arguments to `make'."
  (interactive)
  (let ((flags (read-shell-command "Make Flags: " u-cc-makeflags)))
    (unless (equal flags u-cc-makeflags)
      (setq u-cc-makeflags flags))))

(defun whitespace-respace-function()
  "Replaces all (non-line changing) series of whitespaces by a single space, then reindent"
  (interactive)
  (save-excursion
    ; from https://www.emacswiki.org/emacs/BasicNarrowing
    (save-restriction
      ; narrows regexp replace on region
      (c-mark-function)
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (while (re-search-forward "[[:space:]]+" nil t)
        (replace-match " " nil nil)
        )
      )
    (c-mark-function)
    (indent-region (region-beginning) (region-end) nil)))

(defun u-cc-insert-boilerplate (filename)
  "Insert a C/C++/Java module boilerplate for FILENAME."
  (interactive "*")
  (goto-char (point-min))
  (save-excursion
    (insert-file-contents (concat user-dir-home "elisp/templates/cc-skeleton.cpp")))
  (let ((year (format-time-string "%Y-%Y")))
    (save-excursion
      (search-forward "<<<FILENAME>>>" (point-max) t)
      (replace-match filename t)
      (search-forward "<<<CHOLDER>>>")
      (replace-match user-copyright-holder t)
      (search-forward "<<<YEAR>>>")
      (replace-match year t)
      (search-forward "<<<CHOLDER>>>") (replace-match user-copyright-holder t))))

(defun u-cc-insert-header-skeleton ()
  "Insert a header skeleton."
  (interactive "*")
  (goto-char (point-min))
  (let ((basename-h (concat (upcase (basename)) "_H")))
    (insert "#ifndef " basename-h "\n")
    (insert "#define " basename-h "\n\n")
    (insert "#endif\n")
    (u-cc-insert-boilerplate (basename-full))))

(defun u-cc-insert-source-skeleton ()
  "Insert a source file skeleton."
  (interactive "*")
  (goto-char (point-min))
  (let* ((dir        (basename (substring (dirname) 0 -1)))
         (basename-h (concat (basename) ".h"))
         (inc-file   (concat dir "/" basename-h)))
    (insert (concat "#include \"" inc-file "\"\n\n"))
    (u-cc-insert-boilerplate (basename-full))))

(defun u-cc-insert-summary ()
  "Insert a summary block at POINT."
  (interactive "*")
  (let ((end-point))
    (save-excursion
      (insert " /// <summary>\n")
      (insert " ///   \n")
      (insert " /// </summary>\n")
      (setq end-point (point)))
    (indent-region (point) end-point)))

(define-key c-mode-map    [menu-bar] nil)
(define-key c++-mode-map  [menu-bar] nil)
(define-key java-mode-map [menu-bar] nil)

(define-key c-mode-map    [(control d)] nil)
(define-key c++-mode-map  [(control d)] nil)
(define-key java-mode-map [(control d)] nil)

(easy-menu-define u-cpp-menu  c++-mode-map  "C++"  (append '("C++")  u-cc-mode-menu-text))
(easy-menu-define u-c-menu    c-mode-map    "C"    (append '("C")    u-cc-mode-menu-text))
(easy-menu-define u-java-menu java-mode-map "Java" (append '("Java") u-cc-mode-menu-text))

(easy-menu-define u-java-menu java-mode-map "Java Build"
  '("Build"
    ["Compile File" (compile (concat "javac -Xlint " (file-name-nondirectory (buffer-file-name)))) t]
    ))

;; (easy-menu-define u-cpp-build-menu c++-mode-map "C++ Build"
;;   '("Build"
;;     ["Syntax Check" (compile (concat "g++ " u-cc-cflags " -fsyntax-only " (file-name-nondirectory (buffer-file-name)))) t]
;;     ["Compile File" (let* ((fnn  (file-name-nondirectory   (buffer-file-name)))
;;                            (fnse (file-name-sans-extension fnn)))
;;                       (compile (concat "g++ " u-cc-cflags  " -c " fnn " -o " fnse ".o"))) t]
;;     ["Compile Program" (let* ((fnn  (file-name-nondirectory   (buffer-file-name)))
;;                               (fnse (file-name-sans-extension fnn)))
;;                          (compile (concat "g++ " u-cc-cflags " " u-cc-ldflags " " fnn " -o " fnse))) t]
;;     "---"
;;     ["Make"    (compile (concat "make " u-cc-makeflags)) t]
;;     ["Make..." compile                                     t]
;;     "---"
;;     ["Set Compiler Flags..." (u-cc-set-cflags)    t]
;;     ["Set Linker Flags..."   (u-cc-set-ldflags)   t]
;;     ["Set Make Flags..."     (u-cc-set-makeflags) t]
;;     ))

;; =============================================================================

(defvar kdrp-root-path (or (getenv "ROOT_PATH") (concat (s-join "/" (list (getenv "HOME") "sandstone")))))
(defvar kdrp-sandstone (s-join "/" (list kdrp-root-path "sandstone")))

(defvar kdrp-select-build-config "Select Build Configuration [EmbeddedDebug]")
(defvar kdrp-select-application  "Select Application [PremiumWithModus]")
(defvar kdrp-select-library      "Select Library [libBrewEngineSyncModule]")

(defvar kdrp-config "EmbeddedDebug")

(defvar kdrp-application "PremiumWithModus")
(defvar kdrp-library     "libBrewEngineSyncModule")

(defvar kdrp-make-library                (format "make -j4 config=%s"    kdrp-config))
(defvar kdrp-make-library-clean          (format "make clean config=%s " kdrp-config))

(defvar kdrp-make-selected-library       (format "cd %s && make %s config=%s" kdrp-sandstone kdrp-library kdrp-config))
(defvar kdrp-make-selected-library-clean (s-join " " (list kdrp-make-selected-library "target=clean")))

(defvar kdrp-make-application            (format "cd %s && make %s config=%s" kdrp-sandstone kdrp-application kdrp-config))
(defvar kdrp-make-application-clean      (s-join " " (list kdrp-make-application "target=clean")))
(defvar kdrp-make-application-realclean  (s-join " " (list kdrp-make-application "target=realclean")))

(defun kdrp-set-config (config)
  "Set KDRP build configuration to CONFIG."
  (setq kdrp-config  config
        kdrp-select-build-config         (format "Select Build Configuration [%s]" config)
        kdrp-make-library                (format "make -j4 config=%s"    kdrp-config)
        kdrp-make-library-clean          (format "make clean config=%s " kdrp-config)
        kdrp-make-selected-library       (format "cd %s && make %s config=%s" kdrp-sandstone kdrp-library kdrp-config)
        kdrp-make-selected-library-clean (s-join " " (list kdrp-make-selected-library "target=clean"))
        kdrp-make-application            (format "cd %s && make %s config=%s" kdrp-sandstone kdrp-application kdrp-config)
        kdrp-make-application-clean      (s-join " " (list kdrp-make-application "target=clean"))
        kdrp-make-application-realclean  (s-join " " (list kdrp-make-application "target=realclean"))))

(defun kdrp-set-app (app)
  "Set KDRP application to APP."
  (setq kdrp-application app
        kdrp-select-application         (format "Select Application [%s]" app)
        kdrp-make-application           (format "cd %s && make %s config=%s" kdrp-sandstone kdrp-application kdrp-config)
        kdrp-make-application-clean     (s-join " " (list kdrp-make-application "target=clean"))
        kdrp-make-application-realclean (s-join " " (list kdrp-make-application "target=realclean"))))

(defun kdrp-set-lib (lib)
  "Set KDRP library to LIB."
  (setq kdrp-library lib
        kdrp-select-library              (format "Select Library [%s]" lib)
        kdrp-make-selected-library       (format "cd %s && make %s config=%s" kdrp-sandstone kdrp-library kdrp-config)
        kdrp-make-selected-library-clean (s-join " " (list kdrp-make-selected-library "target=clean"))))

(defun kdrp-make-file ()
  "Compile the current file."
  (let* ((object (concat (f-base (buffer-name)) ".o"))
         (ttb    (s-join "/" (list "." kdrp-config object)))
         (this   (s-join " " (list "make" ttb kdrp-config))))
    (compile this)))

(defun arm? ()
    "Is build configuration set for arm architecture?"
  (or (string= kdrp-config "EmbeddedDebug")
      (string= kdrp-config "EmbeddedRelease")))

(defun config? (config)
    "Is CONFIG == ‘kdrp-build-config’?"
    (string= kdrp-config config))

(defun app? (app)
    "Is APP == ‘kdrp-application’?"
  (string= kdrp-application app))

(defun lib? (lib)
    "Is LIB == ‘kdrp-library’?"
  (string= kdrp-library lib))

(easy-menu-define u-cpp-build-menu c++-mode-map "C++ Build"
  '("Build"
    (kdrp-select-build-config
     ["EmbeddedDebug"   (kdrp-set-config "EmbeddedDebug")   :active t :style toggle :selected (config? "EmbeddedDebug")]
     ["Debug"           (kdrp-set-config "Debug")           :active t :style toggle :selected (config? "Debug")]
     ["EmbeddedRelease" (kdrp-set-config "EmbeddedRelease") :active t :style toggle :selected (config? "EmbeddedRelease")]
     ["Release"         (kdrp-set-config "Release")         :active t :style toggle :selected (config? "Release")])
    (kdrp-select-library
     ["libBrewEngineSyncModule"   (kdrp-set-lib "libBrewEngineSyncModule")   :active t style toggle :selected (lib? "libBrewEngineSyncModule")]
     ["libBrewerComponents"       (kdrp-set-lib "libBrewerComponents")       :active t style toggle :selected (lib? "libBrewerComponents")]
     ["libBuiltInTest"            (kdrp-set-lib "libBuiltInTest")            :active t style toggle :selected (lib? "libBuiltInTest")]
     ["libCloudFileTransfers"     (kdrp-set-lib "libCloudFileTransfers")     :active t style toggle :selected (lib? "libCloudFileTransfers")]
     ["libCommunications"         (kdrp-set-lib "libCommunications")         :active t style toggle :selected (lib? "libCommunications")]
     ["libConnectivity-SDK"       (kdrp-set-lib "libConnectivity-SDK")       :active t style toggle :selected (lib? "libConnectivity-SDK")]
     ["libCrankPresentationLayer" (kdrp-set-lib "libCrankPresentationLayer") :active t style toggle :selected (lib? "libCrankPresentationLayer")]
     ["libDataCommon"             (kdrp-set-lib "libDataCommon")             :active t style toggle :selected (lib? "libDataCommon")]
     ["libDataModel"              (kdrp-set-lib "libDataModel")              :active t style toggle :selected (lib? "libDataModel")]
     ["libDataModelClient"        (kdrp-set-lib "libDataModelClient")        :active t style toggle :selected (lib? "libDataModelClient")]
     ["libDevices"                (kdrp-set-lib "libDevices")                :active t style toggle :selected (lib? "libDevices")]
     ["libExceptions"             (kdrp-set-lib "libExceptions")             :active t style toggle :selected (lib? "libExceptions")]
     ["libGenericIot"             (kdrp-set-lib "libGenericIot")             :active t style toggle :selected (lib? "libGenericIot")]
     ["libImageCapture"           (kdrp-set-lib "libImageCapture")           :active t style toggle :selected (lib? "libImageCapture")]
     ["libImageSearch"            (kdrp-set-lib "libImageSearch")            :active t style toggle :selected (lib? "libImageSearch")]
     ["libLinuxAssert"            (kdrp-set-lib "libLinuxAssert")            :active t style toggle :selected (lib? "libLinuxAssert")]
     ["libLinuxI2c"               (kdrp-set-lib "libLinuxI2c")               :active t style toggle :selected (lib? "libLinuxI2c")]
     ["libLinuxInterrupt"         (kdrp-set-lib "libLinuxInterrupt")         :active t style toggle :selected (lib? "libLinuxInterrupt")]
     ["libLinuxSerial"            (kdrp-set-lib "libLinuxSerial")            :active t style toggle :selected (lib? "libLinuxSerial")]
     ["libLinuxUart"              (kdrp-set-lib "libLinuxUart")              :active t style toggle :selected (lib? "libLinuxUart")]
     ["libLinuxUtilities"         (kdrp-set-lib "libLinuxUtilities")         :active t style toggle :selected (lib? "libLinuxUtilities")]
     ["libPodDetection"           (kdrp-set-lib "libPodDetection")           :active t style toggle :selected (lib? "libPodDetection")]
     ["libProductRecauth"         (kdrp-set-lib "libProductRecauth")         :active t style toggle :selected (lib? "libProductRecauth")]
     ["libRecipeManagement"       (kdrp-set-lib "libRecipeManagement")       :active t style toggle :selected (lib? "libRecipeManagement")]
     ["libSimulators"             (kdrp-set-lib "libSimulators")             :active t style toggle :selected (lib? "libSimulators")]
     ["libSoftwareUpdate"         (kdrp-set-lib "libSoftwareUpdate")         :active t style toggle :selected (lib? "libSoftwareUpdate")]
     ["libStateMachines"          (kdrp-set-lib "libStateMachines")          :active t style toggle :selected (lib? "libStateMachines")]
     ["libSystemCommon"           (kdrp-set-lib "libSystemCommon")           :active t style toggle :selected (lib? "libSystemcommon")]
     ["libUtilities"              (kdrp-set-lib "libUtilities")              :active t style toggle :selected (lib? "libUtilities")]
     ["libSubsystemTests"         (kdrp-set-lib "libSubsystemTests")         :active t style toggle :selected (lib? "libSubsystemTests")])
    (kdrp-select-application
     ["PremiumWithModus"    (kdrp-set-app "PremiumWithModus")    :active t      :style toggle :selected (app? "PremiumWithModus")]
     ["PremiumWithModusBit" (kdrp-set-app "PremiumWithModusBit") :active (arm?) :style toggle :selected (app? "PremiumWithModusBit")]
     ["K2500"               (kdrp-set-app "K2500")               :active t      :style toggle :selected (app? "K2500")]
     ["K2500Bit"            (kdrp-set-app "K2500Bit")            :active t      :style toggle :selected (app? "K2500Bit")]
     ["K3500"               (kdrp-set-app "K3500")               :active t      :style toggle :selected (app? "K3500")]
     ["K3500Bit"            (kdrp-set-app "K3500Bit")            :active (arm?) :style toggle :selected (app? "K3500Bit")]
     ["tools"               (kdrp-set-app "tools")               :active (arm?) :style toggle :selected (app? "tools")])
    "---"
    ["Compile This File" (kdrp-make-file) :active (string= (f-ext (buffer-name)) "cpp")]
    "---"
    ["Clean Current Library"  (compile kdrp-make-library-clean)          :active t]
    ["Build Current Library"  (compile kdrp-make-library)                :active t]
    ["Clean Selected Library" (compile kdrp-make-selected-library-clean) :active t]
    ["Build Selected Library" (compile kdrp-make-selected-library)       :active t]
    "---"
    ["Clean Current Application"        (compile kdrp-make-application-clean)     :active t]
    ["Really Clean Current Application" (compile kdrp-make-application-realclean) :active t]
    ["Build Current Application"        (compile kdrp-make-application)           :active t]))

;; =============================================================================

(easy-menu-define u-cpp-build-menu c-mode-map "C Build"
  '("Build"
    ["Syntax Check" (compile (concat "gcc " u-cc-cflags " -fsyntax-only " (file-name-nondirectory (buffer-file-name)))) t]
    ["Compile File" (let* ((fnn  (file-name-nondirectory   (buffer-file-name)))
                           (fnse (file-name-sans-extension fnn)))
                      (compile (concat "gcc " u-cc-cflags  " -c " fnn " -o " fnse ".o"))) t]
    ["Compile Program" (let* ((fnn  (file-name-nondirectory   (buffer-file-name)))
                              (fnse (file-name-sans-extension fnn)))
                         (compile (concat "gcc " u-cc-cflags " " u-cc-ldflags " " fnn " -o " fnse))) t]
    "---"
    ["Make"    (compile (concat "make " u-cc-makeflags)) t]
    ["Make..." compile                                   t]
    "---"
    ["Set Compiler Flags..." (u-cc-set-cflags)    t]
    ["Set Linker Flags..."   (u-cc-set-ldflags)   t]
    ["Set Make Flags..."     (u-cc-set-makeflags) t]
    ))
                                        ;
(setq c-default-style "bsd" c-basic-offset  4)
(c-set-offset 'case-label '+)



;; (add-hook 'c++-mode-hook  'u-c++-setup)
;; (add-hook 'c-mode-hook    'u-c-setup)
;; (add-hook 'java-mode-hook 'u-java-setup)

;; (add-hook 'c-mode-common-hook
;; 	  (lambda ()
;; 	    (when (featurep 'filladapt)
;; 	      (c-setup-filladapt))))

;; ("tjf"
;;   (c-basic-offset . 4)
;;   (c-comment-only-line-offset . 0)
;;   (c-offsets-alist
;;    (statement-block-intro . +)
;;    (knr-argdecl-intro . +)
;;    (substatement-open . 0)
;;    (label . -)
;;    (statement-cont . +)))
                                        ;
(message "Loading u-cc...done")
(provide 'u-cc)

;;; u-cc.el ends here
