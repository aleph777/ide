;;; fontaine-theme.el --- color definitions -*-lexical-binding: t-*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2026  Tom Fontaine
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
;;
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
;;              01-Jan-2024 added ‘Lilex’ font
;;                          changed scrollbar colors
;;                          Wayland adjustments
;;

;;
;;; Code:
(message "Loading fontaine-theme...")
(require 'tjf-fonts)

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

(let* ((theme/fixed-pitch-family    tjf:fonts/fixed)
       (theme/variable-pitch-family tjf:fonts/variable)

       (theme/fixed-pitch-height    (if is-wsl? 112 184))
       (theme/variable-pitch-height (/ (* 3 theme/fixed-pitch-height) 4))

       (mint-y/green   "#8fa876")
       (mint-y/green-2 "#b3c4a2")

       (mint-x/black "#2f2f2f")
       (mint-x/white "#cccccc")

       (ubuntu/bg-shell "#2d0922")

       (theme/black   x11/black)
       (theme/white   x11/white)
       (theme/red     x11/red)
       (theme/yellow  x11/yellow)
       (theme/green   x11/green)
       (theme/blue    x11/blue)
       (theme/cyan    x11/cyan)
       (theme/magenta x11/magenta)

       (theme/fg-black    theme/black)
       (theme/fg-white    theme/white)
       (theme/fg-white-ui mint-x/white)

       (theme/bg-black    theme/black)
       (theme/bg-black-ui mint-x/black)
       (theme/bg-white    theme/white)

       (theme/default     british/hint-of-pensive)

       ;; text-should be "white"
       ;;
       (theme/fg-gray         nord/polar-night-nord-2)
       (theme/fg-red          canva/red-pepper)
       (theme/fg-red-yellow   x11/dark-orange3)
       (theme/fg-yellow-red   canva/camouflage)
       (theme/fg-yellow       wcag/060)
       (theme/fg-yellow-green canva/forest-green)
       (theme/fg-green-yellow wcag/090)
       (theme/fg-green        material/green-900)
       (theme/fg-green-cyan   wcag/140)
       (theme/fg-cyan-green   ibm/teal-60)
       (theme/fg-cyan         dutch/turkish-aqua)
       (theme/fg-cyan-blue    french/forest-blues)
       (theme/fg-blue-cyan    material/blue-900)
       (theme/fg-blue         x11/blue2)
       (theme/fg-blue-magenta ryb/blue-purple)
       (theme/fg-magenta-blue ryb/purple)
       (theme/fg-magenta      wcag/300)
       (theme/fg-magenta-red  x11/maroon4)
       (theme/fg-red-magenta  ibm/magenta-70)

       (theme/fg-red-dark     x11/firebrick4)
       (theme/fg-blue-dark    dutch/20000-leagues-under-the-sea)

       ;; largely for ui highlighting. text should be "black.
       ;;
       (theme/bg-gray         material-ui/grey)
       (theme/bg-red          indian/georgia-peach)
       (theme/bg-red-yellow   chinese/bruschetta-tomato)
       (theme/bg-yellow-red   ryb/orange)
       (theme/bg-yellow       ryb/yellow)
       (theme/bg-yellow-green dutch/energos)
       (theme/bg-green-yellow x11/lawn-green)
       (theme/bg-green        x11/green2)
       (theme/bg-green-cyan   chinese/ufo-green)
       (theme/bg-cyan-green   american/light-greenish-blue)
       (theme/bg-cyan         turkish/electric-blue)
       (theme/bg-cyan-blue    turkish/neon-blue)
       (theme/bg-blue-cyan    ibm/ultramarine-20)
       (theme/bg-blue         american/shy-moment)
       (theme/bg-blue-magenta ibm/indigo-20)
       (theme/bg-magenta-blue x11/medium-orchid1)
       (theme/bg-magenta      x11/orchid1)
       (theme/bg-magenta-red  canadian/jigglypuff)
       (theme/bg-red-magenta  russian/rogue-pink)

       ;; theme palette

       (theme/added             theme/fg-green)
       (theme/array             theme/fg-blue-magenta)
       (theme/fg-bookmark       theme/bg-white)
       (theme/builtin           theme/fg-cyan)
       (theme/comment           theme/fg-red-dark)
       (theme/constant          theme/fg-green)
       (theme/current-line      theme/bg-white)
       (theme/cursor            theme/fg-black)
       (theme/fg-default        theme/fg-black)
       (theme/deleted           theme/fg-red)
       (theme/escape            theme/fg-blue-magenta)
       (theme/error             theme/fg-red)
       (theme/error-text        theme/white)
       (theme/execute           theme/fg-magenta-red)
       (theme/function          theme/fg-blue)
       (theme/hash              theme/fg-magenta)
       (theme/heredoc           theme/fg-red-magenta)
       (theme/inactive-1        ibm/cool-gray-70)
       (theme/inactive-2        ibm/cool-gray-60)
       (theme/keyword           theme/fg-black)
       (theme/line-number       theme/fg-gray)
       (theme/modified          theme/fg-blue-magenta)
       (theme/non-overridable   theme/fg-gray)
       (theme/fg-paren-mismatch theme/fg-white)
       (theme/fg-paren-no-match theme/fg-red)
       (theme/prompt            theme/fg-blue-cyan)
       (theme/rainbow-1         wcag/180)
       (theme/rainbow-2         wcag/220)
       (theme/rainbow-3         wcag/240)
       (theme/rainbow-4         wcag/280)
       (theme/rainbow-5         wcag/320)
       (theme/rainbow-6         wcag/020)
       (theme/rainbow-7         wcag/060)
       (theme/rainbow-8         wcag/100)
       (theme/rainbow-9         wcag/140)
       (theme/shadow            theme/fg-gray)
       (theme/string            theme/fg-yellow)
       (theme/success           theme/fg-green)
       (theme/type              theme/fg-blue-dark)
       (theme/variable          theme/fg-blue-cyan)
       (theme/warning           theme/fg-red-yellow)


        ;; theme elements

       (theme/bg-bookmark        theme/bg-red-yellow)
       (theme/bg-default         theme/bg-gray)
       (theme/error-field        theme/red)     ;; background for white text
       (theme/lazy-highlight     theme/bg-blue-cyan)
       (theme/match              theme/bg-cyan)
       (theme/next-error         theme/bg-red)
       (theme/bg-paren-mismatch  theme/bg-red)
       (theme/bg-paren-no-match  theme/bg-yellow)
       (theme/region             theme/bg-yellow)
       (theme/secondary          theme/bg-yellow-red)
       (theme/volatile-highlight theme/bg-red-yellow)
       (theme/whitespace         theme/bg-magenta)

       ;; theme elements: mode-line / powerline

       (theme/mode-line-box         x11/gray50)
       (theme/bg-mode-line-active   theme/bg-black)
       (theme/fg-mode-line-active   theme/fg-white)
       (theme/bg-mode-line-inactive theme/bg-black)
       (theme/fg-mode-line-inactive theme/fg-white)

       (theme/bg-powerline-active0   material/light-green-300)
       (theme/bg-powerline-active1   material/light-green-200)
       (theme/bg-powerline-active2   theme/bg-black)

       (theme/fg-powerline-active0   theme/fg-black)
       (theme/fg-powerline-active1   theme/fg-black)
       (theme/fg-powerline-active2   theme/fg-mode-line-active)

       (theme/bg-powerline-inactive0 theme/bg-black)
       (theme/bg-powerline-inactive1 theme/inactive-1)
       (theme/bg-powerline-inactive2 theme/inactive-2)

       (theme/fg-powerline-inactive0 theme/fg-mode-line-inactive)
       (theme/fg-powerline-inactive1 theme/fg-white)
       (theme/fg-powerline-inactive2 theme/fg-white))

  (set-face-attribute 'default
                      nil
                      :background theme/default
                      :family     theme/fixed-pitch-family
                      :height     theme/fixed-pitch-height)

  (set-face-attribute 'variable-pitch
                      nil
                      :family theme/variable-pitch-family
                      :height theme/variable-pitch-height)

  (defface powerline-base `((t (:inherit variable-pitch :weight bold))) "" :group 'mode-line-faces)

(custom-theme-set-faces
    `fontaine
;; ;;;;; anzu
   `(anzu-mode-line ((t (:inherit minibuffer-prompt :family ,theme/fixed-pitch-family :height ,theme/fixed-pitch-height :foreground ,theme/match :weight bold))))

;; ;;;;; blamer
   `(blamer-face                        ((t :foreground ,theme/comment    :slant italic)))
   `(blamer-pretty-border-face          ((t :foreground ,theme/fg-default :weight bold)))
   `(blamer-pretty-commit-message-face  ((t :inherit font-lock-comment-face)))
   `(blamer-pretty-meta-data-face       ((t :foreground ,theme/comment)))
   `(blamer-pretty-meta-keywords-face   ((t :foreground ,theme/prompt :weight bold)))

;; ;;;;; bm
   `(bm-fringe-persistent-face ((t :background ,theme/bg-bookmark :foreground ,theme/fg-bookmark )))

;;;;; compilation
   `(compilation-column-number ((t (:weight bold))))
   `(compilation-error         ((t :inherit error)))
   `(compilation-line-number   ((t (:weight bold))))
   `(compilation-warning       ((t :inherit warning)))

;;;;; cursor
   `(cursor ((t (:background ,theme/cursor))))

;;;;; comint
   `(comint-highlight-input  ((t (:weight bold))))
   `(comint-highlight-prompt ((t (:weight bold :foreground ,theme/prompt))))

;;;;; cperl
   `(cperl-array-face          ((t (:foreground ,theme/array           :weight bold))))
   `(cperl-hash-face           ((t (:foreground ,theme/hash            :weight bold))))
   `(cperl-nonoverridable-face ((t (:foreground ,theme/non-overridable :weight bold))))

;;;;; eglot
   `(eglot-highlight-symbol-face ((t (:weight bold :background ,theme/bg-magenta-red))))


;;;;; error
   `(error ((t (:foreground ,theme/error :weight bold))))

;;;;; escape-glyph
   `(escape-glyph ((t :inherit bold-italic :foreground ,theme/escape)))

;;;;; file-name-shadow
   `(file-name-shadow ((t (:inherit shadow))))

;;;;; fixed-pitch
   `(fixed-pitch ((t (:inherit default))))

;;;;; font-lock
   `(font-lock-builtin-face           ((t (:foreground ,theme/builtin :weight bold))))
   `(font-lock-comment-face           ((t (:foreground ,theme/comment :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face          ((t (:foreground ,theme/constant :weight bold))))
   `(font-lock-doc-face               ((t (:foreground ,theme/comment  :weight bold :slant italic))))
   `(font-lock-function-call-face     ((t (:foreground ,theme/function))))
   `(font-lock-function-name-face     ((t (:foreground ,theme/function :weight bold))))
   `(font-lock-keyword-face           ((t (:foreground ,theme/keyword  :weight bold))))
   `(font-lock-negation-char-face     ((t (:weight bold))))
   `(font-lock-string-face            ((t (:foreground ,theme/string   :weight bold :slant italic))))
   `(font-lock-type-face              ((t (:foreground ,theme/type     :weight bold))))
   `(font-lock-variable-name-face     ((t (:foreground ,theme/variable :weight bold))))
   `(font-lock-warning-face           ((t (:inherit warning))))

;;;;; fringe
   `(fringe  ((t (:background unspecified))))

;;;;; git-gutter
   `(git-gutter:added    ((t (:foreground ,theme/added    :weight bold))))
   `(git-gutter:deleted  ((t (:foreground ,theme/deleted  :weight bold))))
   `(git-gutter:modified ((t (:foreground ,theme/modified :weight bold))))

;;;;; highlight
   `(highlight ((t (:inherit match))))

;;;;; hl-line
   `(hl-line ((t (:background ,theme/current-line))))

;;;;; isearch
   `(isearch ((t (:inherit match))))

;;;;; italic
   `(italic         ((t (:slant italic)))) ;; === yes ===

;;;;; lazy-highlight
   `(lazy-highlight ((t (:background ,theme/lazy-highlight))))

;;;;; line-number
   `(line-number              ((t (:foreground ,theme/line-number))))
   `(line-number-current-line ((t (:foreground ,theme/line-number :background ,theme/current-line :weight bold))))

;;;;; match
   `(match ((t (:background ,theme/match))))

;;;;; minibuffer-prompt
   `(minibuffer-prompt ((t (:weight bold))))

;;;;; mode-line
   `(mode-line           ((t (:inherit variable-pitch :background ,theme/bg-mode-line-active   :foreground ,theme/fg-mode-line-active))))
   `(mode-line-inactive  ((t (:inherit variable-pitch :background ,theme/bg-mode-line-inactive :foreground ,theme/fg-mode-line-inactive))))

;;;;; next-error
;;    `(next-error ((t (:background ,theme/bg-red :foreground ,theme/default-fg))))

;;;;; paren
   `(paren-face-match    ((t (:inherit match))))
   `(paren-face-mismatch ((t (:background ,theme/bg-paren-mismatch :foreground ,theme/fg-paren-mismatch))))
   `(paren-face-no-match ((t (:background ,theme/bg-paren-no-match :foreground ,theme/fg-paren-no-match))))

;;;;; powerline
   `(powerline-active0   ((t (:inherit powerline-base :background ,theme/bg-powerline-active0   :foreground ,theme/fg-powerline-active0))))
   `(powerline-active1   ((t (:inherit powerline-base :background ,theme/bg-powerline-active1   :foreground ,theme/fg-powerline-active1))))
   `(powerline-active2   ((t (:inherit powerline-base :background ,theme/bg-powerline-active2   :foreground ,theme/fg-powerline-active2))))

   `(powerline-inactive0 ((t (:inherit powerline-base :background ,theme/bg-powerline-inactive0 :foreground ,theme/fg-powerline-inactive0))))
   `(powerline-inactive1 ((t (:inherit powerline-base :background ,theme/bg-powerline-inactive1 :foreground ,theme/fg-powerline-inactive1))))
   `(powerline-inactive2 ((t (:inherit powerline-base :background ,theme/bg-powerline-inactive2 :foreground ,theme/fg-powerline-inactive2))))

;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:weight bold :foreground ,theme/rainbow-1))))
   `(rainbow-delimiters-depth-2-face ((t (:weight bold :foreground ,theme/rainbow-2))))
   `(rainbow-delimiters-depth-3-face ((t (:weight bold :foreground ,theme/rainbow-3))))
   `(rainbow-delimiters-depth-4-face ((t (:weight bold :foreground ,theme/rainbow-4))))
   `(rainbow-delimiters-depth-5-face ((t (:weight bold :foreground ,theme/rainbow-5))))
   `(rainbow-delimiters-depth-6-face ((t (:weight bold :foreground ,theme/rainbow-6))))
   `(rainbow-delimiters-depth-7-face ((t (:weight bold :foreground ,theme/rainbow-7))))
   `(rainbow-delimiters-depth-8-face ((t (:weight bold :foreground ,theme/rainbow-8))))
   `(rainbow-delimiters-depth-9-face ((t (:weight bold :foreground ,theme/rainbow-9))))

   `(rainbow-delimiters-mismatched-face ((t (:inherit paren-face-mismatch))))
   `(rainbow-delimiters-unmatched-face  ((t (:inherit paren-face-no-match))))

;;;;; rectangle-preview
   `(rectangle-preview ((t (:inherit region))))

;;;;; region
   `(region ((t (:background ,theme/region))))

;;;;; scroll-bar
   `(scroll-bar ((t (:background ,theme/fg-black :foreground ,theme/fg-white))))

;;;;; secondary-selection
   `(secondary-selection ((t (:background ,theme/secondary))))

;;;;; shell-script-mode
   `(sh-quoted-exec ((t :foreground ,theme/execute)))
   `(sh-heredoc     ((t :foreground ,theme/heredoc)))

;;;;; shadow
   `(shadow ((t (:foreground ,theme/shadow))))

;;;;; success
   `(success ((t (:foreground ,theme/success :weight bold))))

;;;;; tab-bar
   `(tab-bar ((t (:inherit mode-line :family ,theme/fixed-pitch-family :weight bold :height 0.9))))

;;;;; tjf:tabline
   `(tjf:tabline/default          ((t (:inherit variable-pitch :height 0.8 :background ,theme/bg-black))))
   `(tjf:tabline/button           ((t (:inherit nil :family ,theme/fixed-pitch-family :background ,theme/fg-mode-line-active :foreground ,theme/fg-black :weight bold))))
   `(tjf:tabline/button-highlight ((t (:inherit tjf:tabline/highlight))))
   `(tjf:tabline/highlight        ((t (:background ,theme/bg-yellow-red :foreground ,theme/fg-default :weight bold))))
   `(tjf:tabline/modified         ((t (:inherit tjf:tabline/default :background ,theme/red    :foreground ,theme/white :weight bold))))
   ;; `(tjf:tabline/sel-mod          ((t (:inherit tjf:tabline/default :background "xxx"            :foreground ,theme/red   :weight bold))))
   ;; `(tjf:tabline/selected         ((t (:inherit tjf:tabline/default :background "xxx"            :foreground ,theme/black :weight bold))))
   `(tjf:tabline/sel-mod          ((t (:inherit tjf:tabline/default            :foreground ,theme/red   :weight bold))))
   `(tjf:tabline/selected         ((t (:inherit tjf:tabline/default            :foreground ,theme/black :weight bold))))
   `(tjf:tabline/unselected       ((t (:inherit tjf:tabline/default :background ,theme/shadow :foreground ,theme/fg-white))))

;;;;; trailing-whitespace
   `(trailing-whitespace ((t (:background ,theme/whitespace))))

;;;;; vhl/default-face
   `(vhl/default-face ((t (:background ,theme/volatile-highlight))))

;;;;; warning
   `(warning ((t (:foreground ,theme/warning :weight bold))))

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
