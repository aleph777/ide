;;; fontaine-theme.el --- Dark on light theme -*-Emacs-Lisp-*-

;;         Copyright © 2016-2019  Tom Fontaine

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
;;

;;; Code:

(message "Loading fontaine-theme...")

(deftheme fontaine)

(defconst themes-dir (concat user-dir-home "/elisp/themes"))
(defconst colors-dir (concat themes-dir "/colors"))

(load-file (concat colors-dir "/american.el"))
(load-file (concat colors-dir "/aussie.el"))
(load-file (concat colors-dir "/british.el"))
(load-file (concat colors-dir "/canadian.el"))
(load-file (concat colors-dir "/canva.el"))
(load-file (concat colors-dir "/chinese.el"))
(load-file (concat colors-dir "/color-blind.el"))
(load-file (concat colors-dir "/dutch.el"))
(load-file (concat colors-dir "/flat.el"))
(load-file (concat colors-dir "/french.el"))
(load-file (concat colors-dir "/german.el"))
(load-file (concat colors-dir "/ibm.el"))
(load-file (concat colors-dir "/indian.el"))
(load-file (concat colors-dir "/material.el"))
(load-file (concat colors-dir "/material-ui.el"))
(load-file (concat colors-dir "/nord.el"))
(load-file (concat colors-dir "/qualitative.el"))
(load-file (concat colors-dir "/russian.el"))
(load-file (concat colors-dir "/spanish.el"))
(load-file (concat colors-dir "/solar.el"))
(load-file (concat colors-dir "/srcsery.el"))
(load-file (concat colors-dir "/swedish.el"))
(load-file (concat colors-dir "/turkish.el"))
(load-file (concat colors-dir "/x11.el"))

;; (add-to-list 'default-frame-alist '(alpha . (92 . 90)))

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

       (camingo         (car (member "CamingoCode"      font-family-list)))
       (consolas        (car (member "Consolas"         font-family-list)))
       (courier         (car (member "Courier 10 Pitch" font-family-list)))
       (cousine         (car (member "Cousine"          font-family-list)))
       (deja-vu         (car (member "DejaVu Sans Mono" font-family-list)))
       (fantasque       (car (member "Fantasque"        font-family-list)))
       (iosevka         (car (member "Iosevka"          font-family-list)))
       (iosevka-slab    (car (member "Iosevka Slab"     font-family-list)))
       (victor          (car (member "Victor"           font-family-list)))
       (hack            (car (member "Hack"             font-family-list)))
       (jet             (car (member "Jet"              font-family-list)))
       (source-code-pro (car (member "Source Code Pro"  font-family-list)))

       (font-family (or hack deja-vu consolas source-code-pro jet iosevka iosevka-slab cousine camingo fantasque victor courier))

       (mint-y/green "#8fa876")
       (mint-x/black "#2f2f2f")
       (mint-x/white "#cccccc")

       (ubuntu/bg-shell "#2d0922")

       ;; theme palette

       (fontaine/fg-red     x11/red2)
       (fontaine/fg-brick   canva/scarlet)
       (fontaine/fg-orange  canva/seeds)
       (fontaine/fg-yellow  x11/yellow)
       (fontaine/fg-gold    ibm/yellow-50)
       (fontaine/fg-lime    canva/plants)
       (fontaine/fg-green   dutch/pixelated-grass)
       (fontaine/fg-teal    ibm/green-50)
       (fontaine/fg-cyan    canva/ocean)
       (fontaine/fg-blue    x11/blue2)
       (fontaine/fg-powder  canadian/bleu-de-france)
       (fontaine/fg-navy    dutch/20000-leagues-under-the-sea)
       (fontaine/fg-purple  x11/blue-violet)
       (fontaine/fg-magenta canva/cherry-red)
       (fontaine/fg-violet  color-blind/magenta-50)
       (fontaine/fg-brown   x11/saddle-brown)
       (fontaine/fg-black   x11/black)
       (fontaine/fg-white   mint-x/white)

       (fontaine/fg-gray-1 x11/gray50)
       (fontaine/fg-gray-2 canva/sleek-blue-black)
       (fontaine/fg-gray-3 russian/pencil-lead)

       (fontaine/bg-red     x11/red2)
       (fontaine/bg-orange  german/nyc-taxi)
       (fontaine/bg-yellow  x11/yellow)

       (fontaine/bg-green-1 mint-y/green)
       (fontaine/bg-green-2 material/light-green-600)
       (fontaine/bg-green-3 material/light-green-400)
       (fontaine/bg-green-4 aussie/pure-apple)
       (fontaine/bg-green-5 ibm/green-60)
       (fontaine/bg-green-6 qualitative/lime-20)

       (fontaine/bg-blue-1 french/azraq-blue)
       (fontaine/bg-blue-2 turkish/neon-blue)

       (fontaine/bg-gray-1  ibm/cool-gray-70)
       (fontaine/bg-gray-2  ibm/cool-gray-60)
       (fontaine/bg-gray-3  ibm/cool-gray-50)
       (fontaine/bg-gray-4  flat/asbestos)

       (fontaine/bg-black   mint-x/black)
       (fontaine/bg-white   x11/white)

       ;; theme elements

       (fontaine/added        fontaine/fg-green)
       (fontaine/array        fontaine/fg-purple)
       (fontaine/builtin      fontaine/fg-cyan)
       (fontaine/comment      fontaine/fg-brick)
       (fontaine/constant     fontaine/fg-green)
       (fontaine/current-line fontaine/bg-white)
       (fontaine/cursor       fontaine/fg-black)
       (fontaine/deleted      fontaine/fg-red)
       (fontaine/error        fontaine/fg-red)
       (fontaine/function     fontaine/fg-blue)
       (fontaine/hash         fontaine/fg-violet)
       (fontaine/keyword      fontaine/fg-black)

       (fontaine/line-number fontaine/fg-gray-2)

       (fontaine/mode-line-box         fontaine/fg-gray-1)
       (fontaine/bg-mode-line-active   fontaine/bg-black)
       (fontaine/fg-mode-line-active   fontaine/fg-white)
       (fontaine/bg-mode-line-inactive fontaine/bg-black)
       (fontaine/fg-mode-line-inactive fontaine/fg-white)

       (fontaine/modified        fontaine/fg-powder)
       (fontaine/non-overridable fontaine/fg-gray-3)

       (fontaine/bg-paren-mismatch fontaine/bg-red)
       (fontaine/fg-paren-mismatch fontaine/fg-white)
       (fontaine/bg-paren-no-match fontaine/bg-yellow)
       (fontaine/fg-paren-no-match fontaine/fg-red)

       (fontaine/bg-powerline-active1 fontaine/bg-mode-line-active)
       (fontaine/fg-powerline-active1 fontaine/fg-mode-line-active)
       (fontaine/bg-powerline-active2 fontaine/bg-green-2)
       (fontaine/fg-powerline-active2 fontaine/fg-black)
       (fontaine/bg-powerline-active3 fontaine/bg-green-3)
       (fontaine/fg-powerline-active3 fontaine/fg-black)

       (fontaine/bg-powerline-inactive1 fontaine/bg-mode-line-inactive)
       (fontaine/fg-powerline-inactive1 fontaine/fg-mode-line-inactive)
       (fontaine/bg-powerline-inactive2 fontaine/bg-gray-2)
       (fontaine/fg-powerline-inactive2 fontaine/fg-white)
       (fontaine/bg-powerline-inactive3 fontaine/bg-gray-3)
       (fontaine/fg-powerline-inactive3 fontaine/fg-white)

       (fontaine/match   fontaine/bg-blue-2)
       (fontaine/prompt  fontaine/fg-red)

       ;; ((((((((()))))))))
       (fontaine/rainbow-1 fontaine/fg-gold)
       (fontaine/rainbow-2 fontaine/fg-lime)
       (fontaine/rainbow-3 fontaine/fg-cyan)
       (fontaine/rainbow-4 fontaine/fg-teal)
       (fontaine/rainbow-5 fontaine/fg-powder)
       (fontaine/rainbow-6 fontaine/fg-navy)
       (fontaine/rainbow-7 fontaine/fg-purple)
       (fontaine/rainbow-8 fontaine/fg-magenta)
       (fontaine/rainbow-9 fontaine/fg-violet)

       (fontaine/bg-rainbow-mismatched fontaine/bg-yellow)
       (fontaine/fg-rainbow-mismatched fontaine/fg-red)
       (fontaine/bg-rainbow-unmatched  fontaine/bg-red)
       (fontaine/fg-rainbow-unmatched  fontaine/fg-yellow)

       (fontaine/region    fontaine/bg-yellow)
       (fontaine/secondary fontaine/bg-orange)
       (fontaine/string    fontaine/fg-brown)
       (fontaine/success   fontaine/fg-green)

       (fontaine/bg-tabbar-default           fontaine/bg-black)
       (fontaine/fg-tabbar-default           fontaine/fg-white)

       (fontaine/bg-tabbar-modified          fontaine/bg-red)
       (fontaine/fg-tabbar-modified          fontaine/fg-white)

       (fontaine/fg-tabbar-modified-selected fontaine/fg-red)

       (fontaine/fg-tabbar-selected          fontaine/fg-black)

       (fontaine/bg-tabbar-unselected        fontaine/bg-gray-1)

       (fontaine/type               fontaine/fg-navy)
       (fontaine/variable           fontaine/fg-powder)
       (fontaine/volatile-highlight fontaine/bg-blue-1)
       (fontaine/warning            fontaine/fg-orange)
       )

  (defface fontaine/variable-pitch '((t (:inherit variable-pitch :height 0.8))) "" :group 'font-lock-faces)

  (defface fontaine/mode-line-base '((t (:family "DejaVu Sans" :box (:line-width 1 :color "gray50")))) "" :group 'font-lock-faces)

  (defface tabbar-default `((t (:inherit fontaine/variable-pitch :background ,fontaine/bg-tabbar-default :foreground ,fontaine/fg-tabbar-default))) "" :group 'font-lock-faces)

  (defface tabbar-unselected `((t (:inherit tabbar-default :background ,fontaine/bg-tabbar-unselected))) "" :group 'font-lock-faces)

  (defface tabbar-modified `((t (:inherit tabbar-default :background ,fontaine/bg-tabbar-modified :foreground ,fontaine/fg-tabbar-modified))) "" :group 'font-lock-faces)

  (defface tabbar-selected `((t (:inherit tabbar-default :background "nil" :foreground ,fontaine/fg-tabbar-selected :weight bold))) "" :group 'font-lock-faces)


  (defface tabbar-selected-modified `((t (:inherit tabbar-selected :foreground ,fontaine/fg-tabbar-modified-selected))) "" :group 'font-lock-faces)

  (defface tabbar-button            `((t (:inherit tabbar-unselected))) "" :group 'font-lock-faces)

  (custom-theme-set-faces
   `fontaine

   ;; basic settings

   `(default ((t (:family ,font-family :height 120 :width normal))))
   ;;

   `(anzu-mode-line ((t (:inherit minibuffer-prompt :foreground ,fontaine/match :weight bold))))

   `(cursor ((t (:background ,fontaine/cursor))))

   `(comint-highlight-prompt ((t (:foreground ,fontaine/prompt :weight bold))))

   `(compilation-column-number ((t (:weight bold))))
   `(compilation-line-number   ((t (:weight bold))))
   `(compilation-warning       ((t (:foreground ,fontaine/warning :weight bold))))

   `(cperl-array-face          ((t (:foreground ,fontaine/array           :weight bold))))
   `(cperl-hash-face           ((t (:foreground ,fontaine/hash            :weight bold))))
   `(cperl-nonoverridable-face ((t (:foreground ,fontaine/non-overridable :weight bold))))

   `(error ((t (:foreground ,fontaine/error   :weight bold))))

   `(font-lock-builtin-face          ((t (:foreground ,fontaine/builtin  :weight bold))))
   `(font-lock-comment-face          ((t (:foreground ,fontaine/comment  :slant  italic))))
   `(font-lock-constant-face         ((t (:foreground ,fontaine/constant :weight bold))))
   `(font-lock-doc-face              ((t (:foreground ,fontaine/comment  :weight bold :slant italic))))
   `(font-lock-function-name-face    ((t (:foreground ,fontaine/function :weight bold))))
   `(font-lock-keyword-face          ((t (:foreground ,fontaine/keyword  :weight bold))))
   `(font-lock-string-face           ((t (:foreground ,fontaine/string   : slant italic))))
   `(font-lock-type-face             ((t (:foreground ,fontaine/type     :weight bold))))
   `(font-lock-variable-name-face    ((t (:foreground ,fontaine/variable :weight bold))))
   `(font-lock-warning-face          ((t (:foreground ,fontaine/warning  :weight bold))))

   `(fringe  ((t (:background nil))))

   `(git-gutter:added                ((t (:foreground ,fontaine/added    :weight bold))))
   `(git-gutter:deleted              ((t (:foreground ,fontaine/deleted  :weight bold))))
   `(git-gutter:modified             ((t (:foreground ,fontaine/modified :weight bold))))

   `(highlight ((t (:background ,fontaine/match))))

   `(hl-line ((t (:background ,fontaine/current-line))))

   `(isearch ((t (:background ,fontaine/match))))

   `(line-number              ((t (:foreground ,fontaine/line-number))))
   `(line-number-current-line ((t (:foreground ,fontaine/line-number :weight bold))))

   `(match ((t (:background ,fontaine/match))))

   `(mode-line           ((t (:inherit fontaine/mode-line-base :background ,fontaine/bg-mode-line-active   :foreground ,fontaine/bg-mode-line-active))))
   `(mode-line-inactive  ((t (:inherit fontaine/mode-line-base :background ,fontaine/bg-mode-line-inactive :foreground ,fontaine/bg-mode-line-inactive))))

   `(paren-face-match    ((t (:background ,fontaine/match))))
   `(paren-face-mismatch ((t (:background ,fontaine/bg-paren-mismatch :foreground ,fontaine/fg-paren-mismatch))))
   `(paren-face-no-match ((t (:background ,fontaine/bg-paren-no-match :foreground ,fontaine/fg-paren-no-match))))

   `(powerline-active1   ((t (:inherit fontaine/mode-line-base :background ,fontaine/bg-powerline-active1 :foreground ,fontaine/fg-powerline-active1 :weight bold))))
   `(powerline-active2   ((t (:inherit fontaine/mode-line-base :background ,fontaine/bg-powerline-active2 :foreground ,fontaine/fg-powerline-active2))))
   `(powerline-active3   ((t (:inherit fontaine/mode-line-base :background ,fontaine/bg-powerline-active3 :foreground ,fontaine/fg-powerline-active3))))

   `(powerline-inactive1 ((t (:inherit fontaine/mode-line-base :background ,fontaine/bg-powerline-inactive1 :foreground ,fontaine/fg-powerline-inactive1 :weight bold))))
   `(powerline-inactive2 ((t (:inherit fontaine/mode-line-base :background ,fontaine/bg-powerline-inactive2 :foreground ,fontaine/fg-powerline-inactive2))))
   `(powerline-inactive3 ((t (:inherit fontaine/mode-line-base :background ,fontaine/bg-powerline-inactive3 :foreground ,fontaine/fg-powerline-inactive3))))

   `(rainbow-delimiters-base-face    ((t (:inherit unspecified :weight bold))))
   `(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-1))))
   `(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-2))))
   `(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-3))))
   `(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-4))))
   `(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-5))))
   `(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-6))))
   `(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-7))))
   `(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-8))))
   `(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/rainbow-9))))

   `(rainbow-delimiters-mismatched-face ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/fg-rainbow-mismatched :background ,fontaine/bg-rainbow-mismatched))))
   `(rainbow-delimiters-unmatched-face  ((t (:inherit rainbow-delimiters-base-face :foreground ,fontaine/fg-rainbow-unmatched  :background ,fontaine/bg-rainbow-unmatched))))

   `(region ((t (:background ,fontaine/region))))

   `(secondary-selection ((t (:background ,fontaine/secondary))))

   `(success ((t (:foreground ,fontaine/success :weight bold))))

   `(vhl/default-face ((t (:background ,fontaine/volatile-highlight))))

   `(warning ((t (:foreground ,fontaine/warning :weight bold))))

   `(web-mode-json-key-face ((t (:weight bold))))
   ) ;; custom-theme-set-faces
  ) ;; let*
 ;;
(message "Loading fontaine-theme...done")
(provide-theme 'fontaine)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; fontaine-theme.el ends here
;; rainbow ((((((((()))))))))
