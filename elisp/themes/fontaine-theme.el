;;; fontaine-theme.el --- Dark on light theme -*-Emacs-Lisp-*-

;;         Copyright © 2016-2021  Tom Fontaine

;; Author: Tom Fontaine
;; Date:   19-Aug-2016

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

;; Revision:    16-Nov-2016 Added vhl/default-face
;;              04-Jan-2017 Added material colors
;;              05-Jan-2017 Converting from blue to Mint
;;              25-Apr-2017 Added flat  colors
;;              19-Jun-2018 Added multiple palettes
;;              02-Apr-2019 Changed font-family selection method
;;                          Added ‘Hack’ and made it the default font
;;                          Added ‘Source Code Pro’
;;              04-Apr-2019 Added ‘Cousine’
;;                          Added ‘CamingoCode’
;;                          Added ‘Fantasque’
;;              09-Jun-2019 Added ‘Nord’ colors
;;                          Added ‘Srcsery’ colors
;;              09-Jul-2019 Added ‘Victor’ font
;;              21-Jan-2020 Added ‘JetBrains’ font
;;              28-Jan-2020 Added ‘Iosevka’ and ‘IosevkaSlab’ font
;;              23-Sep-2020 Added ‘canva’ colors
;;                          Cleaned up mint definitions
;;              05-Oct-2020 Major overhaul
;;              20-Oct-2020 Added variable pitch fonts
;;              26-Oct-2020 Added ‘Segoe UI' font
;;              14-Jan-2021 Fixed copyright
;;                          load colors programmatically
;;                          rainbow-delimiters use wcag hue progression
;;              09-Feb-2021 Changed ‘variable-pitch’ font to Nimbus Sans
;;                          Fixed broken font lookups
;;

;; =============================================================================
;; Standard Faces
;; Here are the standard faces for specifying text appearance. You can
;; apply them to specific text when you want the effects they produce.
;;
;; bold
;; This face uses a bold variant of the default font.
;;
;; bold-italic
;; This face uses a bold italic variant of the default font.
;;
;; default
;; This face is used for ordinary  text that doesn’t specify any face.
;; Its background color is used as the frame’s background color.
;;
;; fixed-pitch
;; This face forces use of a fixed-width font. It’s reasonable to
;; customize this face to use a different fixed-width font, if you
;; like, but you should not make it a variable-width font.
;;
;; fixed-pitch-serif
;; This face is like fixed-pitch, except the font has serifs and looks
;; more like traditional typewriting.
;;
;; italic
;; This face uses an italic variant of the default font.
;;
;; shadow
;; This face is used for making the text less noticeable than the
;; surrounding ordinary text. Usually this can be achieved by using
;; shades of gray in contrast with either black or white default
;; foreground color.
;;
;; underline
;; This face underlines text.
;;
;; variable-pitch
;; This face forces use of a variable-width font.

;; =============================================================================
;; Here’s an incomplete list of faces used to highlight parts of the
;; text temporarily for specific purposes. (Many other modes define
;; their own faces for this purpose.)
;;
;; escape-glyph
;; The face for displaying control characters and escape sequences
;;
;; highlight
;; This face is used for text highlighting in various contexts, such
;; as when the mouse cursor is moved over a hyperlink.
;;
;; homoglyph
;; The face for displaying lookalike characters, i.e., characters that
;; look like but are not the characters being represented
;;
;; isearch
;; This face is used to highlight the current Isearch match
;;
;; lazy-highlight
;; This face is used to highlight lazy matches for Isearch and Query
;; Replace (matches other than the current one).
;;
;; nobreak-hyphen
;; The face for displaying no-break hyphen characters (see Text Display).
;;
;; nobreak-space
;; The face for displaying no-break space characters
;;
;; query-replace
;; This face is used to highlight the current Query Replace match
;;
;; region
;; This face is used for displaying an active region (see Mark). When
;; Emacs is built with GTK+ support, its colors are taken from the
;; current GTK+ theme.
;;
;; secondary-selection
;; This face is used for displaying a secondary X selection
;;
;; trailing-whitespace
;; The face for highlighting excess spaces and tabs at the end of a
;; line when show-trailing-whitespace is non-nil

;; =============================================================================
;; The following faces control the appearance of parts of the Emacs frame:
;;
;; cursor
;; The :background attribute of this face specifies the color of the
;; text cursor.
;;
;; fringe
;; The face for the fringes to the left and right of windows on
;; graphic displays. (The fringes are the narrow portions of the Emacs
;; frame between the text area and the window’s right and left borders.)
;;
;; header-line
;; Similar to mode-line for a window’s header line, which appears at
;; the top of a window just as the mode line appears at the bottom.
;; Most windows do not have a header line—only some special modes,
;; such Info mode, create one.
;;
;; header-line-highlight
;; Similar to highlight and mode-line-highlight, but used for
;; mouse-sensitive portions of text on header lines. This is a
;; separate face because the header-line face might be customized in a
;; way that does not interact well with highlight.
;;
;; minibuffer-prompt
;; This face is used for the prompt strings displayed in the
;; minibuffer. By default, Emacs automatically adds this face to the
;; value of minibuffer-prompt-properties, which is a list of text
;; properties (see Text Properties in the Emacs Lisp Reference Manual)
;; used to display the prompt text. (This variable takes effect when
;; you enter the minibuffer.)
;;
;; mode-line-buffer-id
;; This face is used for buffer identification parts in the mode line.
;;
;; mode-line
;; This face is used for the mode line of the currently selected
;; window, and for menu bars when toolkit menus are not used. By
;; default, it’s drawn with shadows for a raised effect on graphical
;; displays, and drawn as the inverse of the default face on
;; non-windowed terminals.
;;
;; mode-line-highlight
;; Like highlight, but used for mouse-sensitive portions of text on
;; mode lines. Such portions of text typically pop up tooltips (see
;; Tooltips) when the mouse pointer hovers above them.
;;
;; mouse
;; This face determines the color of the mouse pointer.
;;
;; mode-line-inactive
;; Like mode-line, but used for mode lines of the windows other than
;; the selected one (if mode-line-in-non-selected-windows is non-nil).
;; This face inherits from mode-line, so changes in that face affect
;; mode lines in all windows.
;;
;; tab-line
;; Similar to mode-line for a window’s tab line, which appears at the
;; top of a window with tabs representing window buffers.
;;
;; tooltip
;; This face is used for tooltip text. By default, if Emacs is built
;; with GTK+ support, tooltips are drawn via GTK+ and this face has no
;; effect. See Tooltips.
;;
;; vertical-border
;; This face is used for the vertical divider between windows on text
;; terminals.
;;

;;; Code:

(message "Loading fontaine-theme...")

(deftheme fontaine)

(let* ((themes-dir (if (bound-and-true-p tjf:user/dir-themes)
                       tjf:user/dir-themes
                     (concate user-dir-home "elisp/themes/")))
       (colors-dir (concat themes-dir "colors/")))
  (mapc #'load-file (file-expand-wildcards (concat colors-dir "*.el"))))

;; Windows fonts
;;   Consolas
;;   Office Code Pro
;;   Office Code Pro D
;;   Source Code Pro
;;   Cousine
;;   Fantasque
;;   Courier New

;;   Iosevka,Iosevka Light Extended:style=Light Extended,Regular

(let* ((font-family-list (font-family-list))
       (camingo         (car (member "CamingoCode"         font-family-list)))
       (consolas        (car (member "Consolas"            font-family-list)))
       (courier         (car (member "Courier 10 Pitch"    font-family-list)))
       (cousine         (car (member "Cousine"             font-family-list)))
       (deja-vu         (car (member "DejaVu Sans Mono"    font-family-list)))
       (fantasque       (car (member "Fantasque Sans Mono" font-family-list)))
       (firacode        (car (member "Fira Code"           font-family-list)))
       (hack            (car (member "Hack"                font-family-list)))
       (input           (car (member "Input"               font-family-list)))
       (iosevka         (car (member "Iosevka"             font-family-list)))
       (iosevka-slab    (car (member "Iosevka Slab"        font-family-list)))
       (jet             (car (member "Jet"                 font-family-list)))
       (source-code-pro (car (member "Source Code Pro"     font-family-list)))
       (victor          (car (member "Victor Mono"         font-family-list)))

       (avenir-next (car (member "Avenir Next Rounded Pro" font-family-list)))
       (dejavu-sans (car (member "DejaVu Sans"             font-family-list)))
       (open-sans   (car (member "Open Sans"               font-family-list)))
       (nimbus-sans (car (member "Nimbus Sans"             font-family-list)))
       (noto-sans   (car (member "Noto Sans"               font-family-list)))
       (roboto      (car (member "Roboto"                  font-family-list)))
       (segoe-ui    (car (member "Segoe UI"                font-family-list)))
       (source-sans (car (member "Source Sans Pro"         font-family-list)))

       (fontaine/fixed-pitch-family    (or hack deja-vu consolas source-code-pro jet firacode iosevka iosevka-slab cousine camingo fantasque victor input courier))
       (fontaine/variable-pitch-family (or nimbus-sans roboto avenir-next noto-sans source-sans dejavu-sans open-sans segoe-ui))

       (fontaine/fixed-pitch-height    120)
       (fontaine/variable-pitch-height 120)

       (mint-y/green   "#8fa876")
       (mint-y/green-2 "#b3c4a2")

       (mint-x/black "#2f2f2f")
       (mint-x/white "#cccccc")

       (ubuntu/bg-shell "#2d0922")

       ;; theme palette

       ;; theme palette foregrounds - minimum WCAG AA, preferably WCAG AAA

       (fontaine/fg-black x11/black)
       (fontaine/fg-gray  nord/polar-night-nord-2)
       (fontaine/fg-white mint-x/white)

       (fontaine/fg-red          canva/red-pepper)
       (fontaine/fg-red-yellow   wcag/010)
       (fontaine/fg-yellow-red   canva/camouflage)
       (fontaine/fg-yellow       wcag/060)
       (fontaine/fg-yellow-green canva/forest-green)
       (fontaine/fg-green-yellow wcag/090)
       (fontaine/fg-green        material/green-900)
       (fontaine/fg-green-cyan   wcag/140)
       (fontaine/fg-cyan-green   ibm/teal-60)
       (fontaine/fg-cyan         dutch/turkish-aqua)
       (fontaine/fg-cyan-blue    french/forest-blues)
       (fontaine/fg-blue-cyan    material/blue-900)
       (fontaine/fg-blue         x11/blue2)
       (fontaine/fg-blue-magenta ryb/blue-purple)
       (fontaine/fg-magenta-blue ryb/purple)
       (fontaine/fg-magenta      wcag/300)
       (fontaine/fg-magenta-red  x11/maroon4)
       (fontaine/fg-red-magenta  ibm/magenta-70)

       (fontaine/fg-red-dark     x11/firebrick4)
       (fontaine/fg-blue-dark    dutch/20000-leagues-under-the-sea)

       ;; theme palette backgrounds - minimum (vs. black) WCAG AA, preferably WCAG AAA

       (fontaine/bg-black   mint-x/black)
       (fontaine/bg-gray    x11/gray95)
       (fontaine/bg-white   x11/white)

       (fontaine/bg-red          indian/georgia-peach)
       (fontaine/bg-red-yellow   chinese/bruschetta-tomato)
       (fontaine/bg-yellow-red   ryb/orange)
       (fontaine/bg-yellow       ryb/yellow)
       (fontaine/bg-yellow-green dutch/energos)
       (fontaine/bg-green-yellow x11/lawn-green)
       (fontaine/bg-green        x11/green2)
       (fontaine/bg-green-cyan   chinese/ufo-green)
       (fontaine/bg-cyan-green   american/light-greenish-blue)
       (fontaine/bg-cyan         turkish/electric-blue)
       (fontaine/bg-cyan-blue    turkish/neon-blue)
       (fontaine/bg-blue-cyan    ibm/ultramarine-20)
       (fontaine/bg-blue         american/shy-moment)
       (fontaine/bg-blue-magenta ibm/indigo-20)
       (fontaine/bg-magenta-blue x11/medium-orchid1)
       (fontaine/bg-magenta      x11/orchid1)
       (fontaine/bg-magenta-red  canadian/jigglypuff)
       (fontaine/bg-red-magenta  russian/rogue-pink)

       ;; theme elements

       (fontaine/added              fontaine/fg-green)
       (fontaine/array              fontaine/fg-blue-magenta)
       (fontaine/bookmark-bg        fontaine/bg-red-yellow)
       (fontaine/bookmark-fg        fontaine/bg-white)
       (fontaine/builtin            fontaine/fg-cyan)
       (fontaine/comment            fontaine/fg-red-dark)
       (fontaine/constant           fontaine/fg-green)
       (fontaine/current-line       fontaine/bg-white)
       (fontaine/cursor             fontaine/fg-black)
       (fontaine/default-bg         fontaine/bg-gray)
       (fontaine/default-fg         fontaine/fg-black)
       (fontaine/deleted            fontaine/fg-red)
       (fontaine/escape             fontaine/fg-blue-magenta)
       (fontaine/error              fontaine/fg-red)
       (fontaine/execute            fontaine/fg-magenta-red)
       (fontaine/function           fontaine/fg-blue)
       (fontaine/hash               fontaine/fg-magenta)
       (fontaine/inactive-1         ibm/cool-gray-70)
       (fontaine/inactive-2         ibm/cool-gray-60)
       (fontaine/keyword            fontaine/fg-black)
       (fontaine/lazy-highlight     fontaine/bg-blue-cyan)
       (fontaine/line-number        fontaine/fg-black)
       (fontaine/match              fontaine/bg-cyan)
       (fontaine/modified           fontaine/fg-blue-magenta)
       (fontaine/next-error         fontaine/bg-red)
       (fontaine/non-overridable    fontaine/fg-gray)
       (fontaine/paren-mismatch-bg  fontaine/bg-red)
       (fontaine/paren-mismatch-fg  fontaine/fg-white)
       (fontaine/paren-no-match-bg  fontaine/bg-yellow)
       (fontaine/paren-no-match-fg  fontaine/fg-red)
       (fontaine/prompt             fontaine/fg-blue-cyan)
       (fontaine/rainbow-1          wcag/180)
       (fontaine/rainbow-2          wcag/220)
       (fontaine/rainbow-3          wcag/240)
       (fontaine/rainbow-4          wcag/280)
       (fontaine/rainbow-5          wcag/320)
       (fontaine/rainbow-6          wcag/020)
       (fontaine/rainbow-7          wcag/060)
       (fontaine/rainbow-8          wcag/100)
       (fontaine/rainbow-9          wcag/140)
       (fontaine/region             fontaine/bg-yellow)
       (fontaine/secondary          fontaine/bg-yellow-red)
       (fontaine/shadow             fontaine/fg-gray)
       (fontaine/string             fontaine/fg-yellow)
       (fontaine/success            fontaine/fg-green)
       (fontaine/type               fontaine/fg-blue-dark)
       (fontaine/variable           fontaine/fg-blue-cyan)
       (fontaine/volatile-highlight fontaine/bg-red-yellow)
       (fontaine/warning            fontaine/fg-red-yellow)
       (fontaine/whitespace         fontaine/bg-magenta)

       ;; theme elements: mode-line / powerline

       (fontaine/mode-line-box         x11/gray50)
       (fontaine/mode-line-active-bg   fontaine/bg-black)
       (fontaine/mode-line-active-fg   fontaine/fg-white)
       (fontaine/mode-line-inactive-bg fontaine/bg-black)
       (fontaine/mode-line-inactive-fg fontaine/fg-white)

       (fontaine/powerline-active0-bg   mint-y/green)
       (fontaine/powerline-active1-bg   mint-y/green-2)
       (fontaine/powerline-active2-bg   fontaine/mode-line-active-bg)

       (fontaine/powerline-active0-fg   fontaine/fg-black)
       (fontaine/powerline-active1-fg   fontaine/fg-black)
       (fontaine/powerline-active2-fg   fontaine/mode-line-active-fg)
       (fontaine/powerline-inactive0-bg fontaine/mode-line-inactive-bg)
       (fontaine/powerline-inactive1-bg fontaine/inactive-1)
       (fontaine/powerline-inactive2-bg fontaine/inactive-2)
       (fontaine/powerline-inactive0-fg fontaine/mode-line-inactive-fg)
       (fontaine/powerline-inactive1-fg fontaine/fg-white)
       (fontaine/powerline-inactive2-fg fontaine/fg-white)

       ;; theme elements: tabbar

       (fontaine/tabbar-bg            fontaine/bg-black)
       (fontaine/tabbar-fg            fontaine/fg-white)
       (fontaine/tabbar-modified-bg   x11/red)
       (fontaine/tabbar-modified-fg   x11/white)
       (fontaine/tabbar-sel-mod-fg    fontaine/fg-red)
       (fontaine/tabbar-selected-fg   fontaine/fg-black)
       (fontaine/tabbar-unselected-bg fontaine/inactive-1))

  (message "Defining faces...")

  ;; (defface clips-constant-face '((t (:foreground ,fontaine/constant)))
  ;;   "CLIPS constant face."
  ;;   :group 'font-lock-faces)
  ;; (defvar clips-constant-face (make-face 'clips-constant-face))

  ;; (defface clips-control-face '((t (:foreground ,fontaine/builtin :weight bold)))
  ;;   "CLIPS control face."
  ;;   :group 'font-lock-faces)
  ;; (defvar clips-control-face (make-face 'clips-control-face))

  ;; (defface clips-declaration-face '((t (:foreground ,fontaine/non-overridable :weight bold)))
  ;;   "CLIPS declaration face."
  ;;   :group 'font-lock-faces)
  ;; (defvar clips-declaration-face (make-face 'clips-declaration-face))

  ;; (defface clips-function-face '((t (:foreground ,fontaine/function :weight bold)))
  ;;   "CLIPS function face."
  ;;   :group 'font-lock-faces)
  ;; (defvar clips-function-face (make-face 'clips-function-face))

  ;; (defface clips-global-variable-face '((t (:foreground ,fontaine/variable :weight bold)))
  ;;   "CLIPS global variable face."
  ;;   :group 'font-lock-faces)
  ;; (defvar clips-global-variable-face (make-face 'clips-global-variable-face))

  ;; (defface clips-logical-face '((t (:foreground ,fontaine/fg-yellow-red :weight bold)))
  ;;   "CLIPS logical face."
  ;;   :group 'font-lock-faces)
  ;; (defvar clips-logical-face (make-face 'clips-logical-face))

  ;; (defface clips-object-match-face '((t (:foreground ,fontaine/fg-magenta-blue :weight bold)))
  ;;   "CLIPS global variable face."
  ;;   :group 'font-lock-faces)
  ;; (defvar clips-object-match-face (make-face 'clips-object-match-face))

  ;; (defface clips-variable-face '((t (:foreground ,fontaine/variable)))
  ;;   "CLIPS variable face."
  ;;   :group 'font-lock-faces)
  ;; (defvar clips-variable-face (make-face 'clips-variable-face))

  ;; (defface clips-verb-face '((t(:foreground ,fontaine/fg-green :weight bold) ))
  ;;   "CLIPS verb face."
  ;;   :group 'font-lock-faces)
  ;; (defvar clips-verb-face (make-face 'clips-verb-face))


  (defface fontaine/mode-line-base `((t (:inherit variable-pitch :box (:line-width 1 :color ,fontaine/mode-line-box)))) "" :group 'font-lock-faces)
  (defface fontaine/powerline-red  `((t (:inherit variable-pitch :foreground ,fontaine/warning))) "" :group 'font-lock-faces)
  (defface fontaine/tabline-base   `((t (:inherit default        :family ,fontaine/variable-pitch-family :height 0.8))) "" :group 'font-lock-faces)

  (message "Defining faces...done")

  (custom-theme-set-faces
   `fontaine

   `(default        ((t (:family ,fontaine/fixed-pitch-family    :height ,fontaine/fixed-pitch-height    :foreground ,fontaine/default-fg :background ,fontaine/default-bg))))
   `(fixed-pitch    ((t (:family ,fontaine/fixed-pitch-family    :height ,fontaine/fixed-pitch-height    :foreground ,fontaine/default-fg))))
   `(variable-pitch ((t (:family ,fontaine/variable-pitch-family :height ,fontaine/variable-pitch-height :foreground ,fontaine/default-fg))))


   `(bold   ((t :weight bold)))
   `(italic ((t :slant  italic)))

   `(bold-italic ((t :inherit (bold italic))))

   ;; not fully defined - need to see an example first
   `(homoglyph         ((t :foreground ,fontaine/bg-white :background ,fontaine/fg-black)))
   `(mm-command-output ((t :foreground ,fontaine/bg-white :background ,fontaine/fg-black)))
   `(nobreak-hyphen    ((t :foreground ,fontaine/bg-white :background ,fontaine/fg-black)))
   `(nobreak-space     ((t :foreground ,fontaine/bg-white :background ,fontaine/fg-black)))

   ;; basic settings

;;;;; annotate
   ;; `(annotate-annotation ((t :inherit fontaine-theme-subtle-blue)))
   ;; `(annotate-annotation-secondary ((t :inherit fontaine-theme-subtle-green)))
   ;; `(annotate-highlight ((t :background ,blue-nuanced-bg :underline ,blue-intense)))
   ;; `(annotate-highlight-secondary ((t :background ,green-nuanced-bg :underline ,green-intense)))

   `(anzu-mode-line ((t (:inherit minibuffer-prompt :family ,fontaine/fixed-pitch-family :foreground ,fontaine/match :weight bold))))

   `(bm-face                   ((t :background "hotpink" :foreground "blue" :extend t)))
   `(bm-fringe-face            ((t :background "yellow" :foreground "red" )))
   `(bm-fringe-persistent-face ((t :background ,fontaine/bookmark-bg :foreground ,fontaine/bookmark-fg )))

;; (set-face-attribute 'bm-fringe-persistent-face nil :foreground "white" :background material/fg-orange-800))

;;;;; company-mode
   ;; `(company-echo-common ((t :foreground ,magenta-alt-other)))
   ;; `(company-preview ((t :background ,bg-dim :foreground ,fg-dim)))
   ;; `(company-preview-common ((t :foreground ,blue-alt)))
   ;; `(company-preview-search ((t :inherit fontaine-theme-special-calm)))
   ;; `(company-scrollbar-bg ((t :background ,bg-active)))
   ;; `(company-scrollbar-fg ((t :background ,fg-active)))
   ;; `(company-template-field ((t :inherit fontaine-theme-intense-magenta)))
   ;; `(company-tooltip ((t :background ,bg-alt :foreground ,fg-alt)))
   ;; `(company-tooltip-annotation ((t :inherit fontaine-theme-slant :foreground ,fg-special-cold)))
   ;; `(company-tooltip-annotation-selection ((t :inherit bold :foreground ,fg-main)))
   ;; `(company-tooltip-common ((t :inherit bold :foreground ,blue-alt)))
   ;; `(company-tooltip-common-selection ((t :foreground ,fg-main)))
   ;; `(company-tooltip-mouse ((t :inherit fontaine-theme-intense-blue)))
   ;; `(company-tooltip-search ((t :inherit (fontaine-theme-refine-cyan bold))))
   ;; `(company-tooltip-search-selection ((t :inherit (fontaine-theme-intense-green bold) :underline t)))
   ;; `(company-tooltip-selection ((t :inherit (fontaine-theme-subtle-cyan bold))))
;;;;; company-posframe
   ;; `(company-posframe-active-backend-name ((t :inherit bold :background ,bg-active :foreground ,blue-active)))
   ;; `(company-posframe-inactive-backend-name ((t :background ,bg-active :foreground ,fg-active)))
   ;; `(company-posframe-metadata ((t :background ,bg-inactive :foreground ,fg-inactive)))

;;;;; compilation
   ;; `(compilation-column-number ((t :foreground ,magenta-alt-other)))
   ;; `(compilation-error ((t :inherit fontaine-theme-bold :foreground ,red)))
   ;; `(compilation-info ((t :inherit fontaine-theme-bold :foreground ,fg-special-cold)))
   ;; `(compilation-line-number ((t :foreground ,fg-special-warm)))
   ;; `(compilation-mode-line-exit ((t :inherit fontaine-theme-bold :foreground ,blue-active)))
   ;; `(compilation-mode-line-fail ((t :inherit fontaine-theme-bold :foreground ,red-active)))
   ;; `(compilation-mode-line-run ((t :inherit fontaine-theme-bold :foreground ,magenta-active)))
   ;; `(compilation-warning ((t :inherit fontaine-theme-bold :foreground ,yellow)))

   `(cursor ((t (:background ,fontaine/cursor))))

   `(comint-highlight-input  ((t (:inherit bold))))
   `(comint-highlight-prompt ((t (:inherit bold :foreground ,fontaine/prompt))))

   `(compilation-column-number ((t (:weight bold))))
   `(compilation-line-number   ((t (:weight bold))))
   `(compilation-warning       ((t (:foreground ,fontaine/warning :weight bold))))

   `(cperl-array-face          ((t (:foreground ,fontaine/array           :weight bold))))
   `(cperl-hash-face           ((t (:foreground ,fontaine/hash            :weight bold))))
   `(cperl-nonoverridable-face ((t (:foreground ,fontaine/non-overridable :weight bold))))

;;;;; eglot
   ;; `(eglot-mode-line ((t :inherit fontaine-theme-bold :foreground ,magenta-active)))

   `(error ((t (:foreground ,fontaine/error :weight bold))))

   `(escape-glyph ((t :inherit bold-italic :foreground ,fontaine/escape)))

   `(file-name-shadow ((t (:foreground ,fontaine/shadow))))

;;;;; fold-this
   ;; `(fold-this-overlay ((t :inherit fontaine-theme-special-mild)))

   `(font-lock-builtin-face           ((t (:inherit bold        :foreground ,fontaine/builtin))))
   `(font-lock-comment-face           ((t (:inherit italic      :foreground ,fontaine/comment))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face          ((t (:inherit bold        :foreground ,fontaine/constant))))
   `(font-lock-doc-face               ((t (:inherit bold-italic :foreground ,fontaine/comment))))
   `(font-lock-function-name-face     ((t (:inherit bold        :foreground ,fontaine/function))))
   `(font-lock-keyword-face           ((t (:inherit bold        :foreground ,fontaine/keyword))))
   `(font-lock-negation-char-face     ((t (:inherit bold))))
   `(font-lock-string-face            ((t (:inherit italic      :foreground ,fontaine/string))))
   `(font-lock-type-face              ((t (:inherit bold        :foreground ,fontaine/type))))
   `(font-lock-variable-name-face     ((t (:inherit bold        :foreground ,fontaine/variable))))
   `(font-lock-warning-face           ((t (:inherit bold        :foreground ,fontaine/warning))))

   `(fringe  ((t (:background nil))))

   `(git-gutter:added    ((t (:foreground ,fontaine/added    :weight bold))))
   `(git-gutter:deleted  ((t (:foreground ,fontaine/deleted  :weight bold))))
   `(git-gutter:modified ((t (:foreground ,fontaine/modified :weight bold))))
   ;; `(git-gutter:separator ((t :inherit fontaine-theme-fringe-cyan)))
   ;; `(git-gutter:unchanged ((t :inherit fontaine-theme-fringe-magenta)))

;;;;; helpful
   ;; `(helpful-heading ((t :inherit fontaine-theme-heading-1)))

   `(highlight ((t (:background ,fontaine/match))))

   `(hl-line ((t (:background ,fontaine/current-line))))

   `(isearch ((t (:background ,fontaine/match))))

   `(lazy-highlight ((t (:background ,fontaine/lazy-highlight))))

   `(line-number              ((t (:foreground ,fontaine/line-number))))
   `(line-number-current-line ((t (:foreground ,fontaine/line-number :background ,fontaine/current-line :weight bold))))
   ;; `(line-number-major-tick ((t :inherit (bold default) :background ,yellow-nuanced-bg :foreground ,yellow-nuanced-fg)))
   ;; `(line-number-minor-tick ((t :inherit (bold default) :background ,bg-inactive :foreground ,fg-inactive)))

;;;;; lsp-mode
   ;; `(lsp-face-highlight-read ((t :inherit fontaine-theme-subtle-blue :underline t)))
   ;; `(lsp-face-highlight-textual ((t :inherit fontaine-theme-subtle-blue)))
   ;; `(lsp-face-highlight-write ((t :inherit (fontaine-theme-refine-blue bold))))
   ;; `(lsp-face-semhl-constant ((t :foreground ,blue-alt-other)))
   ;; `(lsp-face-semhl-deprecated
   ;;   ((,(append '((supports :underline (:style wave))) class)
   ;;     :foreground ,yellow :underline (:style wave))
   ;;    (t :foreground ,yellow :underline t)))
   ;; `(lsp-face-semhl-enummember ((t :foreground ,blue-alt-other)))
   ;; `(lsp-face-semhl-field ((t :foreground ,cyan-alt)))
   ;; `(lsp-face-semhl-field-static ((t :inherit fontaine-theme-slant :foreground ,cyan-alt)))
   ;; `(lsp-face-semhl-function ((t :foreground ,magenta)))
   ;; `(lsp-face-semhl-method ((t :foreground ,magenta)))
   ;; `(lsp-face-semhl-namespace ((t :inherit fontaine-theme-bold :foreground ,magenta-alt)))
   ;; `(lsp-face-semhl-preprocessor ((t :foreground ,red-alt-other)))
   ;; `(lsp-face-semhl-static-method ((t :inherit fontaine-theme-slant :foreground ,magenta)))
   ;; `(lsp-face-semhl-type-class ((t :foreground ,magenta-alt)))
   ;; `(lsp-face-semhl-type-enum ((t :foreground ,magenta-alt)))
   ;; `(lsp-face-semhl-type-primitive ((t :inherit fontaine-theme-slant :foreground ,magenta-alt)))
   ;; `(lsp-face-semhl-type-template ((t :inherit fontaine-theme-slant :foreground ,magenta-alt)))
   ;; `(lsp-face-semhl-type-typedef ((t :inherit fontaine-theme-slant :foreground ,magenta-alt)))
   ;; `(lsp-face-semhl-variable ((t :foreground ,cyan)))
   ;; `(lsp-face-semhl-variable-local ((t :foreground ,cyan)))
   ;; `(lsp-face-semhl-variable-parameter ((t :foreground ,cyan-alt-other)))
   ;; `(lsp-lens-face ((t :height 0.8 :foreground ,fg-alt)))
   ;; `(lsp-lens-mouse-face ((t :height 0.8 :foreground ,blue-alt-other :underline t)))
   ;; `(lsp-ui-doc-background ((t :background ,bg-alt)))
   ;; `(lsp-ui-doc-header ((t :background ,bg-header :foreground ,fg-header)))
   ;; `(lsp-ui-doc-url ((t :inherit button)))
   ;; `(lsp-ui-peek-filename ((t :foreground ,fg-special-warm)))
   ;; `(lsp-ui-peek-footer ((t :background ,bg-header :foreground ,fg-header)))
   ;; `(lsp-ui-peek-header ((t :background ,bg-header :foreground ,fg-header)))
   ;; `(lsp-ui-peek-highlight ((t :inherit fontaine-theme-subtle-blue)))
   ;; `(lsp-ui-peek-line-number ((t :inherit shadow)))
   ;; `(lsp-ui-peek-list ((t :background ,bg-dim)))
   ;; `(lsp-ui-peek-peek ((t :background ,bg-alt)))
   ;; `(lsp-ui-peek-selection ((t :inherit fontaine-theme-subtle-cyan)))
   ;; `(lsp-ui-sideline-code-action ((t :foreground ,yellow)))
   ;; `(lsp-ui-sideline-current-symbol ((t :inherit bold :height 0.99 :box (:line-width -1 :style nil) :foreground ,fg-main)))
   ;; `(lsp-ui-sideline-symbol ((t :inherit bold :height 0.99 :box (:line-width -1 :style nil) :foreground ,fg-alt)))
   ;; `(lsp-ui-sideline-symbol-info ((t :inherit italic :height 0.99)))

;;;;; make-mode (makefiles)
   ;; `(makefile-makepp-perl ((t :background ,cyan-nuanced-bg)))
   ;; `(makefile-space ((t :background ,magenta-nuanced-bg)))

   `(match ((t (:background ,fontaine/match))))

   `(minibuffer-prompt ((t (:inherit bold))))

   `(mode-line           ((t (:inherit fontaine/mode-line-base :background ,fontaine/mode-line-active-bg   :foreground ,fontaine/mode-line-active-fg))))
   `(mode-line-inactive  ((t (:inherit fontaine/mode-line-base :background ,fontaine/mode-line-inactive-bg :foreground ,fontaine/mode-line-inactive-fg))))

   `(next-error ((t (:background ,fontaine/bg-red :foreground ,fontaine/default-fg))))

;;;;; paradox
   ;; `(paradox-archive-face ((t :foreground ,fg-special-mild)))
   ;; `(paradox-comment-face ((t :inherit font-lock-comment-face)))
   ;; `(paradox-commit-tag-face ((t :inherit fontaine-theme-refine-magenta :box t)))
   ;; `(paradox-description-face ((t :foreground ,fg-special-cold)))
   ;; `(paradox-description-face-multiline ((t :foreground ,fg-special-cold)))
   ;; `(paradox-download-face ((t :inherit fontaine-theme-bold :foreground ,blue-alt-other)))
   ;; `(paradox-highlight-face ((t :inherit fontaine-theme-bold :foreground ,cyan-alt-other)))
   ;; `(paradox-homepage-button-face ((t :foreground ,magenta-alt-other :underline t)))
   ;; `(paradox-mode-line-face ((t :inherit bold :foreground ,cyan-active)))
   ;; `(paradox-name-face ((t :foreground ,blue :underline t)))
   ;; `(paradox-star-face ((t :foreground ,magenta)))
   ;; `(paradox-starred-face ((t :foreground ,magenta-alt)))

   `(paren-face-match    ((t (:background ,fontaine/match))))
   `(paren-face-mismatch ((t (:background ,fontaine/paren-mismatch-bg :foreground ,fontaine/paren-mismatch-fg))))
   `(paren-face-no-match ((t (:background ,fontaine/paren-no-match-bg :foreground ,fontaine/paren-no-match-fg))))

   `(powerline-active0   ((t (:inherit fontaine/mode-line-base :background ,fontaine/powerline-active0-bg   :foreground ,fontaine/powerline-active0-fg :weight bold))))
   `(powerline-active1   ((t (:inherit fontaine/mode-line-base :background ,fontaine/powerline-active1-bg   :foreground ,fontaine/powerline-active1-fg))))
   `(powerline-active2   ((t (:inherit fontaine/mode-line-base :background ,fontaine/powerline-active2-bg   :foreground ,fontaine/powerline-active2-fg))))

   `(powerline-inactive0 ((t (:inherit fontaine/mode-line-base :background ,fontaine/powerline-inactive0-bg :foreground ,fontaine/powerline-inactive0-fg :weight bold))))
   `(powerline-inactive1 ((t (:inherit fontaine/mode-line-base :background ,fontaine/powerline-inactive1-bg :foreground ,fontaine/powerline-inactive1-fg))))
   `(powerline-inactive2 ((t (:inherit fontaine/mode-line-base :background ,fontaine/powerline-inactive2-bg :foreground ,fontaine/powerline-inactive2-fg))))

   `(rainbow-delimiters-base-face     ((t (:inherit fixed-pitch :weight bold))))
   `(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-1))))
   `(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-2))))
   `(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-3))))
   `(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-4))))
   `(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-5))))
   `(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-6))))
   `(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-7))))
   `(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-8))))
   `(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-9))))

   `(rainbow-delimiters-mismatched-face ((t (:inherit paren-face-mismatch :weight bold))))
   `(rainbow-delimiters-unmatched-face  ((t (:inherit paren-face-no-match :weight bold))))

   `(rectangle-preview ((t (:background ,fontaine/region))))

   `(region ((t (:background ,fontaine/region))))

   `(secondary-selection ((t (:background ,fontaine/secondary))))

;;;;; shell-script-mode
   ;; `(sh-heredoc ((t :foreground ,blue-alt)))
   `(sh-quoted-exec ((t :foreground ,fontaine/execute)))

   `(shadow ((t (:foreground ,fontaine/shadow))))

;;;;; smartparens
   ;; `(sp-pair-overlay-face ((t :inherit fontaine-theme-special-warm)))
   ;; `(sp-show-pair-enclosing ((t :inherit fontaine-theme-special-mild)))
   ;; `(sp-show-pair-match-face ((t ,@(fontaine-themes--paren bg-paren-match bg-paren-match-intense) :foreground ,fg-main)))
   ;; `(sp-show-pair-mismatch-face ((t :inherit fontaine-theme-intense-red)))
   ;; `(sp-wrap-overlay-closing-pair ((t :inherit sp-pair-overlay-face)))
   ;; `(sp-wrap-overlay-face ((t :inherit sp-pair-overlay-face)))
   ;; `(sp-wrap-overlay-opening-pair ((t :inherit sp-pair-overlay-face)))
   ;; `(sp-wrap-tag-overlay-face ((t :inherit sp-pair-overlay-face)))

;;;;; smerge
   ;; `(smerge-base ((t :inherit fontaine-theme-diff-changed)))
   ;; `(smerge-lower ((t :inherit fontaine-theme-diff-added)))
   ;; `(smerge-markers ((t :background ,bg-diff-neutral-2 :foreground ,fg-diff-neutral-2)))
   ;; `(smerge-refined-added ((t :inherit fontaine-theme-diff-refine-added)))
   ;; `(smerge-refined-changed ((t)))
   ;; `(smerge-refined-removed ((t :inherit fontaine-theme-diff-refine-removed)))
   ;; `(smerge-upper ((t :inherit fontaine-theme-diff-removed)))

   `(success ((t (:foreground ,fontaine/success :weight bold))))

   `(tjf:tabline/default    ((t (:inherit fontaine/tabline-base :background ,fontaine/tabbar-bg          :foreground ,fontaine/tabbar-fg))))
   `(tjf:tabline/modified   ((t (:inherit tjf:tabline/default   :background ,fontaine/tabbar-modified-bg :foreground ,fontaine/tabbar-modified-fg))))
   `(tjf:tabline/selected   ((t (:inherit fontaine/tabline-base :weight bold))))
   `(tjf:tabline/sel-mod    ((t (:inherit tjf:tabline/selected                                           :foreground ,fontaine/tabbar-sel-mod-fg))))
   `(tjf:tabline/unselected ((t (:inherit tjf:tabline/default   :background ,fontaine/tabbar-unselected-bg))))
   `(tjf:tabline/button     ((t (:inherit tjf:tabline/unselected))))

   `(trailing-whitespace ((t (:background ,fontaine/whitespace))))

;;;;; treemacs
   ;; `(treemacs-directory-collapsed-face ((t :foreground ,magenta-alt)))
   ;; `(treemacs-directory-face ((t :inherit dired-directory)))
   ;; `(treemacs-file-face ((t :foreground ,fg-main)))
   ;; `(treemacs-fringe-indicator-face ((t :foreground ,fg-main)))
   ;; `(treemacs-git-added-face ((t :foreground ,green-intense)))
   ;; `(treemacs-git-conflict-face ((t :inherit (fontaine-theme-intense-red bold))))
   ;; `(treemacs-git-ignored-face ((t :inherit shadow)))
   ;; `(treemacs-git-modified-face ((t :foreground ,yellow-alt-other)))
   ;; `(treemacs-git-renamed-face ((t :foreground ,cyan-alt-other)))
   ;; `(treemacs-git-unmodified-face ((t :foreground ,fg-main)))
   ;; `(treemacs-git-untracked-face ((t :foreground ,red-alt-other)))
   ;; `(treemacs-help-column-face ((t :inherit fontaine-theme-bold :foreground ,magenta-alt-other :underline t)))
   ;; `(treemacs-help-title-face ((t :foreground ,blue-alt-other)))
   ;; `(treemacs-on-failure-pulse-face ((t :inherit fontaine-theme-intense-red)))
   ;; `(treemacs-on-success-pulse-face ((t :inherit fontaine-theme-intense-green)))
   ;; `(treemacs-root-face ((t :inherit bold :foreground ,blue-alt-other :height 1.2 :underline t)))
   ;; `(treemacs-root-remote-disconnected-face ((t :inherit treemacs-root-remote-face :foreground ,yellow)))
   ;; `(treemacs-root-remote-face ((t :inherit treemacs-root-face :foreground ,magenta)))
   ;; `(treemacs-root-remote-unreadable-face ((t :inherit treemacs-root-unreadable-face)))
   ;; `(treemacs-root-unreadable-face ((t :inherit treemacs-root-face :strike-through t)))
   ;; `(treemacs-tags-face ((t :foreground ,blue-alt)))
   ;; `(treemacs-tags-face ((t :foreground ,magenta-alt)))

   `(vhl/default-face ((t (:background ,fontaine/volatile-highlight))))

   `(warning ((t (:foreground ,fontaine/warning :weight bold))))

   `(web-mode-json-key-face ((t (:weight bold))))

;;;;; which-function-mode
   ;; `(which-func ((t :foreground ,magenta-active)))

;;;;; whitespace-mode
   ;; `(whitespace-big-indent ((t :inherit fontaine-theme-subtle-red)))
   ;; `(whitespace-empty ((t :inherit fontaine-theme-intense-magenta)))
   ;; `(whitespace-hspace ((t :background ,bg-whitespace :foreground ,fg-whitespace)))
   ;; `(whitespace-indentation ((t :background ,bg-whitespace :foreground ,fg-whitespace)))
   ;; `(whitespace-line ((t :background ,bg-alt)))
   ;; `(whitespace-newline ((t :background ,bg-whitespace :foreground ,fg-whitespace)))
   ;; `(whitespace-space ((t :background ,bg-whitespace :foreground ,fg-whitespace)))
   ;; `(whitespace-space-after-tab ((t :inherit fontaine-theme-subtle-magenta)))
   ;; `(whitespace-space-before-tab ((t :inherit fontaine-theme-subtle-cyan)))
   ;; `(whitespace-tab ((t :inherit fontaine-theme-subtle-green)))
   ;; `(whitespace-trailing ((t :inherit fontaine-theme-intense-red)))

;;;;; xref
   ;; `(xref-file-header ((t :inherit bold :foreground ,fg-special-cold)))
   ;; `(xref-line-number ((t :inherit shadow)))
   ;; `(xref-match ((t :inherit match)))

;;;;; yaml-mode
   ;; `(yaml-tab-face ((t :inherit fontaine-theme-intense-red)))

   )) ;; custom-theme-set-faces ;; let*
;;
(message "Loading fontaine-theme...done")

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'fontaine)


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; fontaine-theme.el ends here
