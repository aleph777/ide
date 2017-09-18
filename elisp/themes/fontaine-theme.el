;;; fontaine-theme.el --- Dark on light theme -*-Emacs-Lisp-*-

;;; Commentary:

;; Time-stamp: <25-Apr-2017 07:23:35 EDT, modified by Tom Fontaine>
;;
;; Author:      Tom Fontaine -- Copyright © 2016
;; Date:        19-Aug-2016
;;
;; Revision:    16-Nov-2016 Added vhl/default-face
;;              04-Jan-2017 Added material colors
;;              05-Jan-2017 Converting from blue to Mint
;;              25-Apr-2017 Added flat  colors
;;

;;; Code:

(message "Loading fontaine-theme...")

(deftheme fontaine
  "Shit-stained theme.")

;; Windows fonts
;;   Consolas
;;   Office Code Pro
;;   Office Code Pro D
;;   Source Code Pro
;;   Cousine
;;   Fantasque
;;   Courier New

(defface fontaine/mode-line-base '((t (:family "DejaVu Sans" :box (:line-width 1 :color "gray50")))) "Temp face." :group 'font-lock-faces)
(defface fontaine/variable-pitch '((t (:inherit variable-pitch :height 0.8))) "Temp face." :group 'font-lock-faces)

(let* ((font-family (if (eq system-type 'windows-nt) "Consolas" "DejaVu Sans Mono"))

       (solar/bg-base0    "#839496")
       (solar/bg-base1    "#93a1a1")
       (solar/bg-base2    "#eee8d5")
       (solar/bg-base3    "#fdf6e3")
       (solar/fg-base00   "#657b83")
       (solar/fg-base01   "#586e75")
       (solar/fg-base02   "#073642")
       (solar/fg-base03   "#002b36")
       (solar/fg-blue     "#268bd2")
       (solar/fg-cyan     "#2aa198")
       (solar/fg-green    "#859900")
       (solar/fg-magenta  "#d33682")
       (solar/fg-orange   "#cb4b16")
       (solar/fg-red      "#dc322f")
       (solar/fg-violet   "#6c71c4")
       (solar/fg-yellow   "#b58900")
       
       (flat/alizarin      "#e74c3c")
       (flat/amethyst      "#9b59b6")
       (flat/asbestos      "#7f8c8d")
       (flat/belize-hole   "#2980b9")
       (flat/carrot        "#e67e22")
       (flat/clouds        "#ecf0f1")
       (flat/concrete      "#95a5a6")
       (flat/emerald       "#2ecc71")
       (flat/green-sea     "#16a085")
       (flat/midnight-blue "#2c3e50")
       (flat/nephritis     "#27ae60")
       (flat/orange        "#f39c12")
       (flat/peter-river   "#3498db")
       (flat/pomegranate   "#c0392b")
       (flat/pumpkin       "#d35400")
       (flat/silver        "#bdc3c7")
       (flat/sunflower     "#f1c40f")
       (flat/turquoise     "#1abc9c")
       (flat/wet-asphalt   "#34495e")
       (flat/wisteria      "#8e44ad")

       (fontaine/blue-01  "#40bfff")
       (fontaine/blue-02  "#4f9ce4")

       (material/bg-amber-050  "#fff8e1")
       (material/bg-amber-100  "#ffecb3")
       (material/bg-amber-200  "#ffe082")
       (material/bg-amber-300  "#ffd54f")
       (material/bg-amber-400  "#ffca28")
       (material/bg-amber-500  "#ffc107")
       (material/bg-amber-600  "#ffb300")
       (material/bg-amber-700  "#ffa000")
       (material/bg-amber-800  "#ff8f00")
       (material/bg-amber-900  "#ff6f00")
       (material/bg-amber-A100 "#ffe57f")
       (material/bg-amber-A200 "#ffd740")
       (material/bg-amber-A400 "#ffc400")
       (material/bg-amber-A700 "#ffab00")
       (material/bg-blue-100  "#bbdefb")
       (material/bg-blue-200  "#90caf9")
       (material/bg-blue-300  "#64b5f6")
       (material/bg-blue-400  "#42a5f5")
       (material/bg-blue-50   "#e3f2fd")
       (material/bg-blue-A100 "#82b1ff")
       (material/bg-blue-grey-050 "#eceff1")
       (material/bg-blue-grey-100 "#cfd8dc")
       (material/bg-blue-grey-200 "#b0bec5")
       (material/bg-blue-grey-300 "#90a4ae")
       (material/bg-brown-050 "#efebe9")
       (material/bg-brown-100 "#d7ccc8")
       (material/bg-brown-200 "#bcaaa4")
       (material/bg-cyan-050  "#e0f7fa")
       (material/bg-cyan-100  "#b2ebf2")
       (material/bg-cyan-200  "#80deea")
       (material/bg-cyan-300  "#4dd0e1")
       (material/bg-cyan-400  "#26c6da")
       (material/bg-cyan-500  "#00bcd4")
       (material/bg-cyan-600  "#00acc1")
       (material/bg-cyan-A100 "#84ffff")
       (material/bg-cyan-A200 "#18ffff")
       (material/bg-cyan-A400 "#00e5ff")
       (material/bg-cyan-A700 "#00b8d4")
       (material/bg-deep-orange-050  "#fbe9e7")
       (material/bg-deep-orange-100  "#ffccbc")
       (material/bg-deep-orange-200  "#ffab91")
       (material/bg-deep-orange-300  "#ff8a65")
       (material/bg-deep-orange-400  "#ff7043")
       (material/bg-deep-orange-A100 "#ff9e80")
       (material/bg-deep-orange-A200 "#ff6e40")
       (material/bg-deep-purple-050  "#ede7f6")
       (material/bg-deep-purple-100  "#d1c4e9")
       (material/bg-deep-purple-200  "#b39ddb")
       (material/bg-deep-purple-A100 "#b388ff")
       (material/bg-green-050  "#e8f5e9")
       (material/bg-green-100  "#c8e6c9")
       (material/bg-green-200  "#a5d6a7")
       (material/bg-green-300  "#81c784")
       (material/bg-green-400  "#66bb6a")
       (material/bg-green-500  "#4caf50")
       (material/bg-green-A100 "#b9f6ca")
       (material/bg-green-A200 "#69f0ae")
       (material/bg-green-A400 "#00e676")
       (material/bg-green-A700 "#00c853")
       (material/bg-grey-050 "#fafafa")
       (material/bg-grey-100 "#f5f5f5")
       (material/bg-grey-200 "#eeeeee")
       (material/bg-grey-300 "#e0e0e0")
       (material/bg-grey-400 "#bdbdbd")
       (material/bg-grey-500 "#9e9e9e")
       (material/bg-inidigo-050  "#e8eaf6")
       (material/bg-inidigo-100  "#c5cae9")
       (material/bg-inidigo-200  "#9fa8da")
       (material/bg-inidigo-A100 "#8c9eff")
       (material/bg-light-blue-050  "#e1f5fe")
       (material/bg-light-blue-100  "#b3e5fc")
       (material/bg-light-blue-200  "#81d4fa")
       (material/bg-light-blue-300  "#4fc3f7")
       (material/bg-light-blue-400  "#29b6f6")
       (material/bg-light-blue-500  "#03a9f4")
       (material/bg-light-blue-A100 "#80d8ff")
       (material/bg-light-blue-A200 "#40c4ff")
       (material/bg-light-blue-A400 "#00b0ff")
       (material/bg-light-green-050  "#f1f8e9")
       (material/bg-light-green-100  "#dcedc8")
       (material/bg-light-green-200  "#c5e1a5")
       (material/bg-light-green-300  "#aed581")
       (material/bg-light-green-400  "#9ccc65")
       (material/bg-light-green-500  "#8bc34a")
       (material/bg-light-green-600  "#7cb342")
       (material/bg-light-green-A100 "#ccff90")
       (material/bg-light-green-A200 "#b2ff59")
       (material/bg-light-green-A400 "#76ff03")
       (material/bg-light-green-A700 "#64dd17")
       (material/bg-lime-050  "#f9fbe7")
       (material/bg-lime-100  "#f0f4c3")
       (material/bg-lime-200  "#e6ee9c")
       (material/bg-lime-300  "#dce775")
       (material/bg-lime-400  "#d4e157")
       (material/bg-lime-500  "#cddc39")
       (material/bg-lime-600  "#c0ca33")
       (material/bg-lime-700  "#afb42b")
       (material/bg-lime-800  "#9e9d24")
       (material/bg-lime-A100 "#f4ff81")
       (material/bg-lime-A200 "#eeff41")
       (material/bg-lime-A400 "#c6ff00")
       (material/bg-lime-A700 "#aeea00")
       (material/bg-orange-050  "#fff3e0")
       (material/bg-orange-100  "#ffe0b2")
       (material/bg-orange-200  "#ffcc80")
       (material/bg-orange-300  "#ffb74d")
       (material/bg-orange-400  "#ffa726")
       (material/bg-orange-500  "#ff9800")
       (material/bg-orange-600  "#fb8c00")
       (material/bg-orange-700  "#f57c00")
       (material/bg-orange-A100 "#ffd180")
       (material/bg-orange-A200 "#ffab40")
       (material/bg-orange-A400 "#ff9100")
       (material/bg-orange-A700 "#ff6d00")
       (material/bg-pink-050  "#fce4ec")
       (material/bg-pink-100  "#f8bbd0")
       (material/bg-pink-200  "#f48fb1")
       (material/bg-pink-A100 "#ff80ab")
       (material/bg-purple-050 "#f3e5f5")
       (material/bg-purple-100 "#e1bee7")
       (material/bg-purple-200 "#ce93d8")
       (material/bg-purple-A100 "#ea80fc")
       (material/bg-red-050  "#ffebee")
       (material/bg-red-100  "#ffcdd2")
       (material/bg-red-200  "#ef9a9a")
       (material/bg-red-300  "#e57373")
       (material/bg-red-A100 "#ff8a80")
       (material/bg-teal-050  "#e0f2f1")
       (material/bg-teal-100  "#b2dfdb")
       (material/bg-teal-200  "#80cbc4")
       (material/bg-teal-300  "#4db6ac")
       (material/bg-teal-400  "#26a69a")
       (material/bg-teal-A100 "#a7ffeb")
       (material/bg-teal-A200 "#64ffda")
       (material/bg-teal-A400 "#1de9b6")
       (material/bg-teal-A700 "#00bfa5")
       (material/bg-yellow-050  "#fffde7")
       (material/bg-yellow-100  "#fff9c4")
       (material/bg-yellow-200  "#fff59d")
       (material/bg-yellow-300  "#fff176")
       (material/bg-yellow-400  "#ffee58")
       (material/bg-yellow-500  "#ffeb3b")
       (material/bg-yellow-600  "#fdd835")
       (material/bg-yellow-700  "#fbc02d")
       (material/bg-yellow-800  "#f9a825")
       (material/bg-yellow-900  "#f57f17")
       (material/bg-yellow-A100 "#ffff8d")
       (material/bg-yellow-A200 "#ffff00")
       (material/bg-yellow-A400 "#ffea00")
       (material/bg-yellow-A700 "#ffd600")
       (material/fg-blue-500  "#2196f3")
       (material/fg-blue-600  "#1e88e5")
       (material/fg-blue-700  "#1976d2")
       (material/fg-blue-800  "#1565c0")
       (material/fg-blue-900  "#0d47a1")
       (material/fg-blue-A200 "#448aff")
       (material/fg-blue-A400 "#2979ff")
       (material/fg-blue-A700 "#2962ff")
       (material/fg-blue-grey-400 "#78909c")
       (material/fg-blue-grey-500 "#607d8b")
       (material/fg-blue-grey-600 "#546e7a")
       (material/fg-blue-grey-700 "#455a64")
       (material/fg-blue-grey-800 "#37474f")
       (material/fg-blue-grey-900 "#263238")
       (material/fg-brown-300 "#a1887f")
       (material/fg-brown-400 "#8d6e63")
       (material/fg-brown-500 "#795548")
       (material/fg-brown-600 "#6d4c41")
       (material/fg-brown-700 "#5d4037")
       (material/fg-brown-800 "#4e342e")
       (material/fg-brown-900 "#3e2723")
       (material/fg-cyan-700  "#0097a7")
       (material/fg-cyan-800  "#00838f")
       (material/fg-cyan-900  "#006064")
       (material/fg-deep-orange-500  "#ff5722")
       (material/fg-deep-orange-600  "#f4511e")
       (material/fg-deep-orange-700  "#e64a19")
       (material/fg-deep-orange-800  "#d84315")
       (material/fg-deep-orange-900  "#bf360c")
       (material/fg-deep-orange-A400 "#ff3d00")
       (material/fg-deep-orange-A700 "#dd2c00")
       (material/fg-deep-purple-300  "#9575cd")
       (material/fg-deep-purple-400  "#7e57c2")
       (material/fg-deep-purple-500  "#673ab7")
       (material/fg-deep-purple-600  "#5e35b1")
       (material/fg-deep-purple-700  "#512da8")
       (material/fg-deep-purple-800  "#4527a0")
       (material/fg-deep-purple-900  "#311b92")
       (material/fg-deep-purple-A200 "#7c4dff")
       (material/fg-deep-purple-A400 "#651fff")
       (material/fg-deep-purple-A700 "#6200ea")
       (material/fg-green-600  "#43a047")
       (material/fg-green-700  "#388e3c")
       (material/fg-green-800  "#2e7d32")
       (material/fg-green-900  "#1b5e20")
       (material/fg-grey-600 "#757575")
       (material/fg-grey-700 "#616161")
       (material/fg-grey-800 "#424242")
       (material/fg-grey-900 "#212121")
       (material/fg-inidigo-300  "#7986cb")
       (material/fg-inidigo-400  "#5c6bc0")
       (material/fg-inidigo-500  "#3f51b5")
       (material/fg-inidigo-600  "#3949ab")
       (material/fg-inidigo-700  "#303f9f")
       (material/fg-inidigo-800  "#283593")
       (material/fg-inidigo-900  "#1a237e")
       (material/fg-inidigo-A200 "#536dfe")
       (material/fg-inidigo-A400 "#3d5afe")
       (material/fg-inidigo-A700 "#304ffe")
       (material/fg-light-blue-600  "#039be5")
       (material/fg-light-blue-700  "#0288d1")
       (material/fg-light-blue-800  "#0277bd")
       (material/fg-light-blue-900  "#01579b")
       (material/fg-light-blue-A700 "#0091ea")
       (material/fg-light-green-700  "#689f38")
       (material/fg-light-green-800  "#558b2f")
       (material/fg-light-green-900  "#33691e")
       (material/fg-lime-900  "#827717")
       (material/fg-orange-800  "#ef6c00")
       (material/fg-orange-900  "#e65100")
       (material/fg-pink-300  "#f06292")
       (material/fg-pink-400  "#ec407a")
       (material/fg-pink-500  "#e91e63")
       (material/fg-pink-600  "#d81b60")
       (material/fg-pink-700  "#c2185b")
       (material/fg-pink-800  "#ad1457")
       (material/fg-pink-900  "#880e4f")
       (material/fg-pink-A200 "#ff4081")
       (material/fg-pink-A400 "#f50057")
       (material/fg-pink-A700 "#c51162")
       (material/fg-purple-300 "#ba68c8")
       (material/fg-purple-400 "#ab47bc")
       (material/fg-purple-500 "#9c27b0")
       (material/fg-purple-600 "#8e24aa")
       (material/fg-purple-700 "#7b1fa2")
       (material/fg-purple-800 "#6a1b9a")
       (material/fg-purple-900 "#4a148c")
       (material/fg-purple-A200 "#e040fb")
       (material/fg-purple-A400 "#d500f9")
       (material/fg-purple-A700 "#aa00ff")
       (material/fg-red-400  "#ef5350")
       (material/fg-red-500  "#f44336")
       (material/fg-red-600  "#e53935")
       (material/fg-red-700  "#d32f2f")
       (material/fg-red-800  "#c62828")
       (material/fg-red-900  "#b71c1c")
       (material/fg-red-A200 "#ff5252")
       (material/fg-red-A400 "#ff1744")
       (material/fg-red-A700 "#d50000")
       (material/fg-teal-500  "#009688")
       (material/fg-teal-600  "#00897b")
       (material/fg-teal-700  "#00796b")
       (material/fg-teal-800  "#00695c")
       (material/fg-teal-900  "#004d40")

       (fontaine/array      "purple")
       (fontaine/builtin    "cyan4")
       (fontaine/comment    "firebrick")
       (fontaine/constant   "green4")
       (fontaine/declare    "grey40")
       (fontaine/emphasized "yellow")
       (fontaine/error1     "red")
       (fontaine/function   "blue")
       (fontaine/hash       "violetred")
       (fontaine/keyword    "black")
       (fontaine/non-over   fontaine/declare)
       (fontaine/prompt     solar/fg-red)
       (fontaine/string     "saddlebrown")
       (fontaine/type       "blue3")
       (fontaine/variable   solar/fg-blue)
       (fontaine/warning    solar/fg-yellow)

       (fontaine/cursor    "black")
       (fontaine/inactive  "gray20")
       (fontaine/match     "gold2")
       (fontaine/region    "gold")
       (fontaine/secondary "gold3")
       (fontaine/separator "gray65")
       (fontaine/separator "gray65")

       (mint-y/green      "#8fa876")
       (mint-x/black      "#2f2f2f")
       (mint-x/white      "#cccccc"))

  (custom-theme-set-faces
   `fontaine
   `(default             ((t (:family ,font-family :height 120 :width normal))))
   ;;`(default             ((t (:family "Fantasque Sans Mono":height 120 :width normal))))
   ;;
   `(cursor              ((t (:background "black"))))
   `(region              ((t (:background ,fontaine/region))))
   `(secondary-selection ((t (:background ,fontaine/secondary))))
   ;;
   `(fringe              ((t (:background nil))))
   ;;
   `(success ((t (:foreground "ForestGreen" :weight bold))))
   `(warning ((t (:foreground ,material/bg-yellow-500 :weight bold))))
   ;;
   `(mode-line           ((t (:inherit fontaine/mode-line-base :background ,mint-x/black      :foreground ,mint-x/white))))
   `(mode-line-inactive  ((t (:inherit fontaine/mode-line-base :background ,fontaine/inactive :foreground ,mint-x/white))))
   
   `(powerline-active1   ((t (:inherit fontaine/mode-line-base :background ,mint-x/black      :foreground ,mint-x/white))))
   ;; `(powerline-active2   ((t (:inherit fontaine/mode-line-base :background ,fontaine/blue-02  :foreground "black"))))
   ;; `(powerline-active3   ((t (:inherit fontaine/mode-line-base :background ,fontaine/blue-01  :foreground "black"))))
   `(powerline-active2   ((t (:inherit fontaine/mode-line-base :background ,material/bg-light-green-600 :foreground "black"))))
   `(powerline-active3   ((t (:inherit fontaine/mode-line-base :background ,material/bg-light-green-400  :foreground "black"))))
   
   `(powerline-inactive1 ((t (:inherit fontaine/mode-line-base :background ,fontaine/inactive :foreground ,mint-x/white))))
   `(powerline-inactive2 ((t (:inherit fontaine/mode-line-base :background "grey40"           :foreground ,mint-x/white))))
   `(powerline-inactive3 ((t (:inherit fontaine/mode-line-base :background "grey45"           :foreground ,mint-x/white))))

   `(tabbar-default      ((t (:inherit fontaine/variable-pitch :background ,mint-x/black     :foreground ,mint-x/white))))
   `(tabbar-modified     ((t (:inherit fontaine/variable-pitch :background "red"             :foreground "white" :box (:line-width 1 :color "white" :style released-button)))))
   ;; `(tabbar-selected     ((t (:inherit fontaine/variable-pitch :background ,fontaine/blue-01 :foreground "black" :box (:line-width 1 :color "white" :style pressed-button)))))
   `(tabbar-selected     ((t (:inherit fontaine/variable-pitch :background ,material/bg-light-green-600 :foreground "black" :box (:line-width 1 :color "white" :style pressed-button)))))
   `(tabbar-separator    ((t (:foreground ,fontaine/separator :width normal))))
   ;;
   `(anzu-mode-line                  ((t (:inherit minibuffer-prompt :foreground ,fontaine/emphasized :weight bold))))

   `(clips-constant-face             ((t (:foreground ,solar/fg-blue))))
   `(clips-control-face              ((t (:foreground "cyan4"         :weight bold))))
   `(clips-declaration-face          ((t (:foreground "gray40"        :weight bold))))
   `(clips-function-face             ((t (:foreground ,solar/fg-green :weight bold))))
   `(clips-global-variable-face      ((t (:foreground "navy"))))
   `(clips-logical-face              ((t (:foreground ,solar/fg-cyan  :weight bold))))
   `(clips-object-match-face         ((t (:foreground "purple"        :weight bold))))
   `(clips-variable-face             ((t (:foreground "royalblue"))))
   `(clips-verb-face                 ((t (:foreground "green4"        :weight bold))))

   `(clips-log-activate-face         ((t (:foreground ,solar/fg-magenta :weight bold))))
   `(clips-log-deactivate-face       ((t (:foreground ,solar/fg-magenta))))
   `(clips-log-make-instance-face    ((t (:foreground ,solar/fg-blue :weight bold))))
   `(clips-log-unmake-instance-face  ((t (:foreground ,solar/fg-blue))))
   `(clips-log-fire-face             ((t (:foreground "blue3" :weight bold))))
   `(clips-log-assert-fact-face      ((t (:foreground ,solar/fg-cyan :weight bold))))
   `(clips-log-retract-fact-face     ((t (:foreground ,solar/fg-cyan))))
   `(clips-log-error-face            ((t (:foreground ,fontaine/error1 :weight bold))))
   `(clips-log-fatal-face            ((t (:foreground ,fontaine/error1 :background ,fontaine/emphasized :weight bold))))
   `(clips-log-pass-face             ((t (:foreground "ForestGreen" :weight bold))))

   `(comint-highlight-prompt         ((t (:foreground ,fontaine/prompt :weight bold))))

   `(compilation-column-number       ((t (:foreground "DodgerBlue4"     :weight bold))))
   `(compilation-line-number         ((t (:foreground "royal blue"      :weight bold))))
   `(compilation-warning             ((t (:foreground ,fontaine/warning :weight bold))))

   `(cperl-array-face                ((t (:foreground ,fontaine/array    :weight bold))))
   `(cperl-hash-face                 ((t (:foreground ,fontaine/hash     :weight bold))))
   `(cperl-nonoverridable-face       ((t (:foreground ,fontaine/non-over :weight bold))))

   `(font-lock-comment-face          ((t (:foreground ,fontaine/comment :slant italic))))
   `(font-lock-doc-face              ((t (:foreground ,fontaine/comment :slant italic :weight bold))))

   `(font-lock-string-face           ((t (:foreground ,fontaine/string    :slant italic))))
   `(font-lock-doc-string-face       ((t (:foreground ,fontaine/string    :slant italic))))

   `(font-lock-builtin-face          ((t (:foreground ,fontaine/builtin    :weight bold))))
   `(font-lock-constant-face         ((t (:foreground ,fontaine/constant   :weight bold))))
   `(font-lock-emphasized-face       ((t (:foreground ,fontaine/emphasized :weight bold))))
   `(font-lock-function-name-face    ((t (:foreground ,fontaine/function   :weight bold))))
   `(font-lock-keyword-face          ((t (:foreground ,fontaine/keyword    :weight bold))))
   `(font-lock-type-face             ((t (:foreground ,fontaine/type       :weight bold))))
   `(font-lock-variable-name-face    ((t (:foreground ,solar/fg-blue       :weight bold))))
   `(font-lock-warning-face          ((t (:foreground ,fontaine/warning    :weight bold))))
   ;; (font-lock-color-constant-face (,@fmt-none ,@fg-green))
   ;; (font-lock-comment-delimiter-face ; Comment ;;  (,@fmt-ital ,@fg-base01))
   ;; (font-lock-preprocessor-face (,@fmt-none ,@fg-orange)) ; PreProc
   ;; (font-lock-reference-face (,@fmt-none ,@fg-cyan))
   ;; (font-lock-negation-char-face (,@fmt-none ,@fg-red))
   ;; (font-lock-other-type-face (,@fmt-ital ,@fg-blue))
   ;; (font-lock-regexp-grouping-construct (,@fmt-none ,@fg-orange))
   ;; (font-lock-special-keyword-face (,@fmt-none ,@fg-red)) ; Special
   ;; (font-lock-exit-face (,@fmt-none ,@fg-red))
   ;; (font-lock-other-emphasized-face (,@fmt-bldi ,@fg-violet))
   ;; (font-lock-regexp-grouping-backslash (,@fmt-none ,@fg-yellow))

   ;; `(hes-escape-backslash-face ((t (:weight bold))))
   ;; `(hes-escape-face           ((t (:weight bold))))
   ;; `(hes-escape-sequence-face  ((t (:weight bold))))

   `(highlight                       ((t (:background ,fontaine/emphasized))))

   ;; `(highlight-operators-face        ((t (:foreground ,solar/fg-orange :weight bold))))

   `(hl-line                         ((t (:background "white"))))

   `(isearch                         ((t (:background ,fontaine/emphasized))))

   `(match                           ((t (:background ,fontaine/match))))

   `(paren-face-match                ((t (:background "turquoise"))))
   `(paren-face-mismatch             ((t (:background "yellow" :foreground "red"))))
   `(paren-face-no-match             ((t (:background "red"    :foreground "yellow"))))

   `(sh-heredoc                      ((t (:foreground "tan4"))))
   `(sh-heredoc-face                 ((t (:foreground "red4"))))
   `(sh-quoted-exec                  ((t (:foreground "magenta3" :slant italic))))

   `(vhl/default-face                ((t (:background "DeepSkyBlue"))))

   `(web-mode-json-context-face      ((t (:foreground ,material/fg-pink-500 :weight bold))))
   `(web-mode-json-key-face          ((t (:weight bold))))
   ))
 ;;
(message "Loading fontaine-theme...done")
(provide-theme 'fontaine)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; fontaine-theme.el ends here