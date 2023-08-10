;;; fontaine-theme.el --- Dark on light theme -*-Emacs-Lisp-*-

;;         Copyright © 2016-2023  Tom Fontaine

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

;; Revision:    16-Nov-2016 added vhl/default-face
;;              04-Jan-2017 added material colors
;;              05-Jan-2017 converting from blue to Mint
;;              25-Apr-2017 added flat  colors
;;              19-Jun-2018 added multiple palettes
;;              02-Apr-2019 changed font-family selection method
;;                          added ‘Hack’ and made it the default font
;;                          added ‘Source Code Pro’
;;              04-Apr-2019 added ‘Cousine’
;;                          added ‘CamingoCode’
;;                          added ‘Fantasque’
;;              09-Jun-2019 added ‘Nord’ colors
;;                          added ‘Srcsery’ colors
;;              09-Jul-2019 added ‘Victor’ font
;;              21-Jan-2020 added ‘JetBrains’ font
;;              28-Jan-2020 added ‘Iosevka’ and ‘IosevkaSlab’ font
;;              23-Sep-2020 added ‘canva’ colors
;;                          cleaned up mint definitions
;;              05-Oct-2020 major overhaul
;;              20-Oct-2020 added variable pitch fonts
;;              26-Oct-2020 added ‘Segoe UI' font
;;              14-Jan-2021 fixed copyright
;;                          load colors programmatically
;;                          rainbow-delimiters use wcag hue progression
;;              09-Feb-2021 changed ‘variable-pitch’ font to Nimbus Sans
;;                          fixed broken font lookups
;;              29-Aug-2022 updated ‘sh-heredoc’ face
;;              12-Sep-2022 added ‘fontaine/heredoc’
;;              26-Oct-2022 added ‘eglot-highlight-symbol-face’
;;              08-Jun-2023 added ‘blamer’ faces
;;              09-Jun-2023 added ‘elpaca’ faces
;;              10-Aug-2023 clean up
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
                     (concat user-dir-home "elisp/themes/")))
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
       (anka            (car (member "Anka/Coder"                   font-family-list)))
       (bitstream       (car (member "Bitstream Vera Sans Mono"     font-family-list)))
       (camingo         (car (member "CamingoCode"                  font-family-list)))
       (consolas        (car (member "Consolas"                     font-family-list)))
       (courier         (car (member "Courier 10 Pitch"             font-family-list)))
       (courier-prime   (car (member "Courier Prime Code"           font-family-list)))
       (cousine         (car (member "Cousine"                      font-family-list)))
       (dm              (car (member "DM Mono"                      font-family-list)))
       (envy            (car (member "Envy Code R"                  font-family-list)))
       (fantasque       (car (member "Fantasque Sans Mono"          font-family-list)))
       (firacode        (car (member "Fira Code"                    font-family-list)))
       (hack            (car (member "Hack Nerd Font"               font-family-list)))
       (hermit          (car (member "Hermit"                       font-family-list)))
       (ia-writer       (car (member "iA Writer Mono S"             font-family-list)))
       (inconsolata     (car (member "Inconsolata"                  font-family-list)))
       (input           (car (member "Input Mono"                   font-family-list)))
       (intel-one       (car (member "IntelOne Mono"                font-family-list)))
       (iosevka         (car (member "Iosevka"                      font-family-list)))
       (jet             (car (member "JetBrains Mono"               font-family-list)))
       (julia           (car (member "JuliaMono"                    font-family-list)))
       (liberation      (car (member "Liberation Mono"              font-family-list)))
       (lotion          (car (member "Lotion"                       font-family-list)))
       (luculent        (car (member "Luculent"                     font-family-list)))
       (meslo           (car (member "Meslo LG S DZ"                font-family-list)))
       (monofoki        (car (member "Monofoki"                     font-family-list)))
       (monoid          (car (member "Monoid"                       font-family-list)))
       (recursive       (car (member "Recursive Mono Linear Static" font-family-list)))
       (source-code-pro (car (member "Source Code Pro"              font-family-list)))
       (space           (car (member "Space Mono"                   font-family-list)))
       (twilio          (car (member "Twilio Sans Mono Retina"      font-family-list)))
       (ubuntu          (car (member "Ubuntu Mono"                  font-family-list)))
       (victor          (car (member "Victor Mono"                  font-family-list)))

       (avenir-next (car (member "Avenir Next Rounded Pro" font-family-list)))
       (dejavu-sans (car (member "DejaVu Sans"             font-family-list)))
       (inter       (car (member "Inter V"                 font-family-list)))
       (nimbus-sans (car (member "Nimbus Sans"             font-family-list)))
       (noto-sans   (car (member "Noto Sans"               font-family-list)))
       (open-sans   (car (member "Open Sans"               font-family-list)))
       (roboto      (car (member "Roboto"                  font-family-list)))
       (segoe-ui    (car (member "Segoe UI"                font-family-list)))
       (source-sans (car (member "Source Sans Pro"         font-family-list)))

       (fontaine/fixed-pitch-family
        (or hack bitstream envy ubuntu fantasque
            anka julia cousine input recursive liberation monofoki consolas
            meslo twilio camingo dm source-code-pro iosevka courier-prime
            ia-writer monoid firacode
            hermit intel-one
            space
            luculent inconsolata lotion
            victor
            firacode
            courier))

       (fontaine/variable-pitch-family (or inter nimbus-sans roboto avenir-next noto-sans source-sans dejavu-sans open-sans segoe-ui))

       (fontaine/fixed-pitch-height    168)
       (fontaine/variable-pitch-height (/ (* 3 fontaine/fixed-pitch-height) 4))

       (mint-y/green   "#8fa876")
       (mint-y/green-2 "#b3c4a2")

       (mint-x/black "#2f2f2f")
       (mint-x/white "#cccccc")

       (ubuntu/bg-shell "#2d0922")

       ;; theme palette

       ;; theme palette foregrounds - minimum WCAG AA, preferably WCAG AAA

       (fontaine/black x11/black)
       (fontaine/red   x11/red)
       (fontaine/white x11/white)

       (fontaine/fg-black x11/black)
       (fontaine/fg-gray  nord/polar-night-nord-2)
       (fontaine/fg-white mint-x/white)

       (fontaine/fg-red          canva/red-pepper)
       (fontaine/fg-red-yellow   x11/dark-orange3)
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
       (fontaine/bg-bookmark        fontaine/bg-red-yellow)
       (fontaine/fg-bookmark        fontaine/bg-white)
       (fontaine/builtin            fontaine/fg-cyan)
       (fontaine/comment            fontaine/fg-red-dark)
       (fontaine/constant           fontaine/fg-green)
       (fontaine/current-line       fontaine/bg-white)
       (fontaine/cursor             fontaine/fg-black)
       (fontaine/bg-default         fontaine/bg-gray)
       (fontaine/fg-default         fontaine/fg-black)
       (fontaine/deleted            fontaine/fg-red)
       (fontaine/escape             fontaine/fg-blue-magenta)
       (fontaine/error              fontaine/fg-red)
       (fontaine/execute            fontaine/fg-magenta-red)
       (fontaine/function           fontaine/fg-blue)
       (fontaine/hash               fontaine/fg-magenta)
       (fontaine/heredoc            fontaine/fg-red-magenta)
       (fontaine/inactive-1         ibm/cool-gray-70)
       (fontaine/inactive-2         ibm/cool-gray-60)
       (fontaine/keyword            fontaine/fg-black)
       (fontaine/lazy-highlight     fontaine/bg-blue-cyan)
       (fontaine/line-number        fontaine/fg-gray)
       (fontaine/match              fontaine/bg-cyan)
       (fontaine/modified           fontaine/fg-blue-magenta)
       (fontaine/next-error         fontaine/bg-red)
       (fontaine/non-overridable    fontaine/fg-gray)
       (fontaine/bg-paren-mismatch  fontaine/bg-red)
       (fontaine/fg-paren-mismatch  fontaine/fg-white)
       (fontaine/bg-paren-no-match  fontaine/bg-yellow)
       (fontaine/fg-paren-no-match  fontaine/fg-red)
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
       (fontaine/bg-mode-line-active   fontaine/bg-black)
       (fontaine/fg-mode-line-active   fontaine/fg-white)
       (fontaine/bg-mode-line-inactive fontaine/bg-black)
       (fontaine/fg-mode-line-inactive fontaine/fg-white)

       (fontaine/bg-powerline-active0   material/light-green-300)
       (fontaine/bg-powerline-active1   material/light-green-200)
       (fontaine/bg-powerline-active2   fontaine/bg-black)

       (fontaine/fg-powerline-active0   fontaine/fg-black)
       (fontaine/fg-powerline-active1   fontaine/fg-black)
       (fontaine/fg-powerline-active2   fontaine/fg-mode-line-active)

       (fontaine/bg-powerline-inactive0 fontaine/bg-black)
       (fontaine/bg-powerline-inactive1 fontaine/inactive-1)
       (fontaine/bg-powerline-inactive2 fontaine/inactive-2)

       (fontaine/fg-powerline-inactive0 fontaine/fg-mode-line-inactive)
       (fontaine/fg-powerline-inactive1 fontaine/fg-white)
       (fontaine/fg-powerline-inactive2 fontaine/fg-white))

  (set-face-attribute 'default
                      nil
                      :background fontaine/bg-default
                      :family     fontaine/fixed-pitch-family
                      :height     fontaine/fixed-pitch-height)

  (set-face-attribute 'variable-pitch
                      nil
                      :family fontaine/variable-pitch-family
                      :height fontaine/variable-pitch-height)

  ;; (defface powerline-base `((t (:inherit mode-line :family variable-pitch :weight bold))) "" :group 'mode-line-faces)
  (defface powerline-base `((t (:inherit variable-pitch :weight bold))) "" :group 'mode-line-faces)

  (custom-theme-set-faces
   `fontaine
;; ;;;;; anzu
   `(anzu-mode-line ((t (:inherit minibuffer-prompt :family ,fontaine/variable-pitch-family :foreground ,fontaine/match :weight bold))))

;; ;;;;; blamer
   `(blamer-face                        ((t :foreground ,fontaine/comment    :slant italic)))
   `(blamer-pretty-border-face          ((t :foreground ,fontaine/fg-default :weight bold)))
   `(blamer-pretty-commit-message-face  ((t :inherit font-lock-comment-face)))
   `(blamer-pretty-meta-data-face       ((t :foreground ,fontaine/comment)))
   `(blamer-pretty-meta-keywords-face   ((t :foreground ,fontaine/prompt :weight bold)))

;; ;;;;; bm
   `(bm-fringe-persistent-face ((t :background ,fontaine/bg-bookmark :foreground ,fontaine/fg-bookmark )))

;;;;; compilation
   `(compilation-column-number ((t (:weight bold))))
   `(compilation-error         ((t :inherit error)))
   `(compilation-line-number   ((t (:weight bold))))
   `(compilation-warning       ((t :inherit warning)))

;;;;; cursor
   `(cursor ((t (:background ,fontaine/cursor))))

;;;;; comint
   `(comint-highlight-input  ((t (:weight bold))))
   `(comint-highlight-prompt ((t (:weight bold :foreground ,fontaine/prompt))))

;;;;; cperl
   `(cperl-array-face          ((t (:foreground ,fontaine/array           :weight bold))))
   `(cperl-hash-face           ((t (:foreground ,fontaine/hash            :weight bold))))
   `(cperl-nonoverridable-face ((t (:foreground ,fontaine/non-overridable :weight bold))))

;;;;; eglot
   `(eglot-highlight-symbol-face ((t (:weight bold :background ,fontaine/bg-magenta-red))))

;;;;; elpaca
   `(elpaca-blocked           ((t (:weight bold :foreground ,fontaine/fg-yellow))))
   `(elpaca-busy              ((t (:weight bold :foreground ,fontaine/fg-yellow-red))))
   `(elpaca-finished          ((t (:weight bold :foreground ,fontaine/fg-green-cyan))))
   `(elpaca-finished          ((t (:weight bold :foreground ,fontaine/fg-green-cyan))))
   `(elpaca-ui-marked-package ((t (:weight bold :foreground ,fontaine/fg-magenta))))

;;;;; error
   `(error ((t (:foreground ,fontaine/error :weight bold))))

;;;;; escape-glyph
   `(escape-glyph ((t :inherit bold-italic :foreground ,fontaine/escape)))

;;;;; file-name-shadow
   `(file-name-shadow ((t (:inherit shadow))))

;;;;; fixed-pitch
   `(fixed-pitch ((t (:inherit default))))

;;;;; font-lock
   `(font-lock-builtin-face           ((t (:foreground ,fontaine/builtin :weight bold))))
   `(font-lock-comment-face           ((t (:foreground ,fontaine/comment :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face          ((t (:foreground ,fontaine/constant :weight bold))))
   `(font-lock-doc-face               ((t (:foreground ,fontaine/comment  :weight bold :slant italic))))
   `(font-lock-function-call-face     ((t (:foreground ,fontaine/function))))
   `(font-lock-function-name-face     ((t (:foreground ,fontaine/function :weight bold))))
   `(font-lock-keyword-face           ((t (:foreground ,fontaine/keyword  :weight bold))))
   `(font-lock-negation-char-face     ((t (:weight bold))))
   `(font-lock-string-face            ((t (:foreground ,fontaine/string   :slant italic))))
   `(font-lock-type-face              ((t (:foreground ,fontaine/type     :weight bold))))
   `(font-lock-variable-name-face     ((t (:foreground ,fontaine/variable :weight bold))))
   `(font-lock-warning-face           ((t (:inherit warning))))

;;;;; fringe
   `(fringe  ((t (:background unspecified))))

;;;;; git-gutter
   `(git-gutter:added    ((t (:foreground ,fontaine/added    :weight bold))))
   `(git-gutter:deleted  ((t (:foreground ,fontaine/deleted  :weight bold))))
   `(git-gutter:modified ((t (:foreground ,fontaine/modified :weight bold))))

;;;;; highlight
   `(highlight ((t (:inherit match))))

;;;;; hl-line
   `(hl-line ((t (:background ,fontaine/current-line))))

;;;;; isearch
   `(isearch ((t (:inherit match))))

;;;;; italic
   `(italic         ((t (:slant italic)))) ;; === yes ===

;;;;; lazy-highlight
   `(lazy-highlight ((t (:background ,fontaine/lazy-highlight))))

;;;;; line-number
   `(line-number              ((t (:foreground ,fontaine/line-number))))
   `(line-number-current-line ((t (:foreground ,fontaine/line-number :background ,fontaine/current-line :weight bold))))

;;;;; match
   `(match ((t (:background ,fontaine/match))))

;;;;; minibuffer-prompt
   `(minibuffer-prompt ((t (:weight bold))))

;;;;; mode-line
   `(mode-line           ((t (:inherit variable-pitch :background ,fontaine/bg-mode-line-active   :foreground ,fontaine/fg-mode-line-active))))
   `(mode-line-inactive  ((t (:inherit variable-pitch :background ,fontaine/bg-mode-line-inactive :foreground ,fontaine/fg-mode-line-inactive))))

;; ;;;;; next-error
;;    `(next-error ((t (:background ,fontaine/bg-red :foreground ,fontaine/default-fg))))

;;;;; paren
   `(paren-face-match    ((t (:inherit match))))
   `(paren-face-mismatch ((t (:background ,fontaine/bg-paren-mismatch :foreground ,fontaine/fg-paren-mismatch))))
   `(paren-face-no-match ((t (:background ,fontaine/bg-paren-no-match :foreground ,fontaine/fg-paren-no-match))))

;;;;; powerline
   `(powerline-active0   ((t (:inherit powerline-base :background ,fontaine/bg-powerline-active0   :foreground ,fontaine/fg-powerline-active0))))
   `(powerline-active1   ((t (:inherit powerline-base :background ,fontaine/bg-powerline-active1   :foreground ,fontaine/fg-powerline-active1))))
   `(powerline-active2   ((t (:inherit powerline-base :background ,fontaine/bg-powerline-active2   :foreground ,fontaine/fg-powerline-active2))))

   `(powerline-inactive0 ((t (:inherit powerline-base :background ,fontaine/bg-powerline-inactive0 :foreground ,fontaine/fg-powerline-inactive0))))
   `(powerline-inactive1 ((t (:inherit powerline-base :background ,fontaine/bg-powerline-inactive1 :foreground ,fontaine/fg-powerline-inactive1))))
   `(powerline-inactive2 ((t (:inherit powerline-base :background ,fontaine/bg-powerline-inactive2 :foreground ,fontaine/fg-powerline-inactive2))))

;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:weight bold :foreground ,fontaine/rainbow-1))))
   `(rainbow-delimiters-depth-2-face ((t (:weight bold :foreground ,fontaine/rainbow-2))))
   `(rainbow-delimiters-depth-3-face ((t (:weight bold :foreground ,fontaine/rainbow-3))))
   `(rainbow-delimiters-depth-4-face ((t (:weight bold :foreground ,fontaine/rainbow-4))))
   `(rainbow-delimiters-depth-5-face ((t (:weight bold :foreground ,fontaine/rainbow-5))))
   `(rainbow-delimiters-depth-6-face ((t (:weight bold :foreground ,fontaine/rainbow-6))))
   `(rainbow-delimiters-depth-7-face ((t (:weight bold :foreground ,fontaine/rainbow-7))))
   `(rainbow-delimiters-depth-8-face ((t (:weight bold :foreground ,fontaine/rainbow-8))))
   `(rainbow-delimiters-depth-9-face ((t (:weight bold :foreground ,fontaine/rainbow-9))))

   `(rainbow-delimiters-mismatched-face ((t (:inherit paren-face-mismatch))))
   `(rainbow-delimiters-unmatched-face  ((t (:inherit paren-face-no-match))))

;;;;; rectangle-preview
   `(rectangle-preview ((t (:inherit region))))

;;;;; region
   `(region ((t (:background ,fontaine/region))))

;;;;; scroll-bar
   `(scroll-bar ((t (:background ,fontaine/fg-default :foreground ,fontaine/bg-default))))

;;;;; secondary-selection
   `(secondary-selection ((t (:background ,fontaine/secondary))))

;;;;; shell-script-mode
   `(sh-quoted-exec ((t :foreground ,fontaine/execute)))
   `(sh-heredoc     ((t :foreground ,fontaine/heredoc)))

;;;;; shadow
   `(shadow ((t (:foreground ,fontaine/shadow))))

;;;;; success
   `(success ((t (:foreground ,fontaine/success :weight bold))))

;;;;; tjf:tabline
   `(tjf:tabline/default          ((t (:inherit variable-pitch :height 0.8))))
   `(tjf:tabline/button           ((t (:inherit nil :family ,fontaine/fixed-pitch-family :background ,fontaine/fg-mode-line-active :foreground ,fontaine/fg-black :weight bold))))
   `(tjf:tabline/button-highlight ((t (:inherit tjf:tabline/highlight))))
   `(tjf:tabline/highlight        ((t (:background ,fontaine/bg-yellow-red :foreground ,fontaine/fg-default :weight bold))))
   `(tjf:tabline/modified         ((t (:inherit tjf:tabline/default :background ,fontaine/red    :foreground ,fontaine/white :weight bold))))
   `(tjf:tabline/sel-mod          ((t (:inherit tjf:tabline/default :background "xxx"            :foreground ,fontaine/red   :weight bold))))
   `(tjf:tabline/selected         ((t (:inherit tjf:tabline/default :background "xxx"            :foreground ,fontaine/black :weight bold))))
   `(tjf:tabline/unselected       ((t (:inherit tjf:tabline/default :background ,fontaine/shadow :foreground ,fontaine/fg-white))))

;;;;; trailing-whitespace
   `(trailing-whitespace ((t (:background ,fontaine/whitespace))))

;;;;; vhl/default-face
   `(vhl/default-face ((t (:background ,fontaine/volatile-highlight))))

;;;;; warning
   `(warning ((t (:foreground ,fontaine/warning :weight bold))))

;;;;; web-mode
   `(web-mode-json-key-face ((t (:weight bold))))

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
