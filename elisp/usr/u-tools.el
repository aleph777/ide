;;; u-tools.el --- Tools menu definition and associated functions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;              Copyright © 2016-2017 Tom Fontaine

;; Time-stamp: <18-Jan-2017 13:46:48 EST, modified by Tom Fontaine>
;;
;; Author:      Tom Fontaine
;; Date:        28-Feb-2016
;;

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

;; Revision: 18-Mar-2016 Added a few missing letters
;;           16-Jan-2017 Added "Check Language" to Tools menu
;;           17-Jan-2017 Added ‘*-unicode-name-at-point’
;;                       Added ‘copyright-update’
;;

;;; Code:

(message "Loading u-tools...")
(require 'u-flags)
;;
(defvar u-tools-menu
  '("Tools"
    ["Insert Unicode Character..." insert-char :enable (is-rw?) :key-sequence nil]
    ("Insert Symbol"
     ("Arrows"
      ["←	Leftwards Arrow"  (insert "←") :enable (is-rw?)]
      ["→	Rightwards Arrow" (insert "→") :enable (is-rw?)]
      ["↑	Upwards Arrow"    (insert "↑") :enable (is-rw?)]
      ["↓	Downwards Arrow"  (insert "↓") :enable (is-rw?)]
      ["↔	Left Right Arrow" (insert "↔") :enable (is-rw?)]
      ["↕	Up Down Arrow"    (insert "↕") :enable (is-rw?)]
      ["↗	North East Arrow" (insert "↗") :enable (is-rw?)]
      ["↖	North West Arrow" (insert "↖") :enable (is-rw?)]
      ["↘	South East Arrow" (insert "↘") :enable (is-rw?)]
      ["↙	South West Arrow" (insert "↙") :enable (is-rw?)]
      "---"
      ["⇇	Leftwards Paired Arrows"  (insert "⇇") :enable (is-rw?)]
      ["⇉	Rightwards Paired Arrows" (insert "⇉") :enable (is-rw?)]
      ["⇈	Upwards Paired Arrow"     (insert "⇈") :enable (is-rw?)]
      ["⇊	Downwards Paired Arrow"   (insert "⇊") :enable (is-rw?)]
      "---"
      ["⇆	Leftwards Arrow Over Rightwards Arrow"      (insert "⇆") :enable (is-rw?)]
      ["⇄	Rightwards Arrow Over Leftwards Arrow"      (insert "⇄") :enable (is-rw?)]
      ["⇅	Upwards Arrow Leftwards Of Downwards Arrow" (insert "⇅") :enable (is-rw?)]
      ["⇵	Downwards Arrow Leftwards of Upwards Arrow" (insert "⇵") :enable (is-rw?)]
      "---"
      ["⇐	Leftwards Double Arrow"  (insert "⇐") :enable (is-rw?)]
      ["⇒	Rightwards Double Arrow" (insert "⇒") :enable (is-rw?)]
      ["⇑	Upwards Double Arrow"    (insert "⇑") :enable (is-rw?)]
      ["⇓	Downwards Double Arrow"  (insert "⇓") :enable (is-rw?)]
      ["⇔	Left Right Double Arrow" (insert "⇔") :enable (is-rw?)]
      ["⇕	Up Down Double Arrow"    (insert "⇕") :enable (is-rw?)]
      "---"
      ["↻	Clockwise Open Circle Arrow"     (insert "↻") :enable (is-rw?)]
      ["↺	Anticlockwise Open Circle Arrow" (insert "↺") :enable (is-rw?)])
     ("Currency"
      ["֏	Armenian Dram Sign"                     (insert "֏") :enable (is-rw?)]
      ["₳	Austral Sign"				(insert "₳") :enable (is-rw?)]
      ["৳	Bengali Rupee Sign"                     (insert "৳") :enable (is-rw?)]
      ["¢	Cent Sign"                              (insert "¢") :enable (is-rw?)]
      ["₡	Colón Sign"                             (insert "₡") :enable (is-rw?)]
      ["₢	Cruzeiro Sign"                          (insert "₢") :enable (is-rw?)]
      ["¤	Currency Sign"                          (insert "¤") :enable (is-rw?)]
      ["₯	Drachma Sign"				(insert "₯") :enable (is-rw?)]
      ["₫	Dong Sign"                              (insert "₫") :enable (is-rw?)]
      ["€	Euro Sign"                              (insert "€") :enable (is-rw?)]
      ["₠	Euro-Currency Sign"                     (insert "₠") :enable (is-rw?)]
      ["₣	French Franc Sign"                      (insert "₣") :enable (is-rw?)]
      ["₰	German Penny Sign (Pfennig)"            (insert "₰") :enable (is-rw?)]
      ["₲	Guarani Sign"				(insert "₲") :enable (is-rw?)]
      ["૱	Gujarati Rupee Sign"                    (insert "૱") :enable (is-rw?)]
      ["₴	Hryvnia Sign"				(insert "₴") :enable (is-rw?)]
      ["₹	Indian Rupee Sign"                      (insert "₹") :enable (is-rw?)]
      ["¤	Khmer Currency Symbol Riel"             (insert "¤") :enable (is-rw?)]
      ["₭	Kip Sign"                               (insert "₭") :enable (is-rw?)]
      ["Ƒ	Latin Small Letter F With Hook (Florin)"(insert "Ƒ") :enable (is-rw?)]
      ["₤	Lira Sign"                              (insert "₤") :enable (is-rw?)]
      ["₥	Mill Sign"                              (insert "₥") :enable (is-rw?)]
      ["₦	Naira Sign"                             (insert "₦") :enable (is-rw?)]
      ["₪	New Sheqel Sign"                        (insert "₪") :enable (is-rw?)]
      ["₧	Peseta Sign"                            (insert "₧") :enable (is-rw?)]
      ["₱	Peso Sign"                              (insert "₱") :enable (is-rw?)]
      ["£	Pound Sign"                             (insert "£") :enable (is-rw?)]
      ["₽	Ruble Sign"                             (insert "₽") :enable (is-rw?)]
      ["₨	Rupee Sign"                             (insert "₨") :enable (is-rw?)]
      ["ℳ	Script Capital M (German Mark)"         (insert "ℳ") :enable (is-rw?)]
      ["௹	Tamil Rupee Sign"                       (insert "௹") :enable (is-rw?)]
      ["₸	Tenge Sign"                             (insert "₸") :enable (is-rw?)]
      ["฿	Thai Currency Symbol Baht"              (insert "฿") :enable (is-rw?)]
      ["₮	Tugrik Sign"                            (insert "₮") :enable (is-rw?)]
      ["₺	Turkish Lira Sign"                      (insert "₺") :enable (is-rw?)]
      ["₩	Won Sign"                               (insert "₩") :enable (is-rw?)]
      ["¥	Yen Sign"                               (insert "¥") :enable (is-rw?)])
     ("Greek Symbols"
      ["Δ	Greek Capital Letter Delta" (insert "Δ") :enable (is-rw?)]
      ["Γ	Greek Capital Letter Gamma" (insert "Γ") :enable (is-rw?)]
      ["Ω	Greek Capital Letter Omega" (insert "Ω") :enable (is-rw?)]
      ["Φ	Greek Capital Letter Phi"   (insert "Φ") :enable (is-rw?)]
      ["Π	Greek Capital Letter Pi"    (insert "Π") :enable (is-rw?)]
      ["Ψ	Greek Capital Letter Psi"   (insert "Ψ") :enable (is-rw?)]
      ["Σ	Greek Capital Letter Sigma" (insert "∑") :enable (is-rw?)]
      ["Θ	Greek Capital Letter Theta" (insert "Θ") :enable (is-rw?)]
      "---"
      ["α	Greek Small Letter Alpha"   (insert "α") :enable (is-rw?)]
      ["β	Greek Small Letter Beta"    (insert "β") :enable (is-rw?)]
      ["γ	Greek Small Letter Gamma"   (insert "γ") :enable (is-rw?)]
      ["λ	Greek Small Letter Lambda"  (insert "λ") :enable (is-rw?)]
      ["μ	Greek Small Letter Mu"      (insert "μ") :enable (is-rw?)]
      ["ω	Greek Small Letter Omega"   (insert "ω") :enable (is-rw?)]
      ["φ	Greek Small Letter Phi"     (insert "φ") :enable (is-rw?)]
      ["π	Greek Small Letter Pi"      (insert "π") :enable (is-rw?)]
      ["ψ	Greek Small Letter Psi"     (insert "ψ") :enable (is-rw?)]
      ["σ	Greek Small Letter Sigma"   (insert "σ") :enable (is-rw?)]
      ["τ	Greek Small Letter Tau"     (insert "τ") :enable (is-rw?)]
      ["θ	Greek Small Letter Theta"   (insert "θ") :enable (is-rw?)]
      )
     ("Intellectual Property"
      ["ⓟ	Circled Latin Small Letter P (Sound)" (insert "ⓟ") :enable (is-rw?)]
      ["©	Copyright Sign"                       (insert "©") :enable (is-rw?)]
      ["®	Registered Sign"                      (insert "®") :enable (is-rw?)]
      ["℠	Service Mark"                         (insert "℠") :enable (is-rw?)]
      ["™	Trade Mark Sign"                      (insert "™") :enable (is-rw?)])
     ("Latin-1 Capital Letters"
      ["À	Latin Capital Letter A with grave"      (insert "À") :enable (is-rw?)]
      ["Á	Latin Capital letter A with acute"      (insert "Á") :enable (is-rw?)]
      ["Â	Latin Capital letter A with circumflex" (insert "Â") :enable (is-rw?)]
      ["Ã	Latin Capital letter A with tilde"      (insert "Ã") :enable (is-rw?)]
      ["Ä	Latin Capital letter A with diaeresis"  (insert "Ä") :enable (is-rw?)]
      ["Å	Latin Capital letter A with ring above" (insert "Å") :enable (is-rw?)]
      ["Æ	Latin Capital letter AE"                (insert "Æ") :enable (is-rw?)]
      ["Ç	Latin Capital letter C with cedilla"    (insert "Ç") :enable (is-rw?)]
      ["È	Latin Capital letter E with grave"      (insert "È") :enable (is-rw?)]
      ["É	Latin Capital letter E with acute"      (insert "É") :enable (is-rw?)]
      ["Ê	Latin Capital letter E with circumflex" (insert "Ê") :enable (is-rw?)]
      ["Ë	Latin Capital letter E with diaeresis"  (insert "Ë") :enable (is-rw?)]
      ["Ì	Latin Capital letter I with grave"      (insert "Ì") :enable (is-rw?)]
      ["Í	Latin Capital letter I with acute"      (insert "Í") :enable (is-rw?)]
      ["Î	Latin Capital letter I with circumflex" (insert "Î") :enable (is-rw?)]
      ["Ï	Latin Capital letter I with diaeresis"  (insert "Ï") :enable (is-rw?)]
      ["Ð	Latin Capital letter Eth"               (insert "Ð") :enable (is-rw?)]
      ["Ñ	Latin Capital letter N with tilde"      (insert "Ñ") :enable (is-rw?)]
      ["Ò	Latin Capital letter O with grave"      (insert "Ò") :enable (is-rw?)]
      ["Ó	Latin Capital letter O with acute"      (insert "Ó") :enable (is-rw?)]
      ["Ô	Latin Capital letter O with circumflex" (insert "Ô") :enable (is-rw?)]
      ["Õ	Latin Capital letter O with tilde"      (insert "Õ") :enable (is-rw?)]
      ["Ö	Latin Capital letter O with diaeresis"  (insert "Ö") :enable (is-rw?)]
      ["Ø	Latin Capital letter O with stroke"     (insert "Ø") :enable (is-rw?)]
      ["Ù	Latin Capital letter U with grave"      (insert "Ù") :enable (is-rw?)]
      ["Ú	Latin Capital letter U with acute"      (insert "Ú") :enable (is-rw?)]
      ["Û	Latin Capital Letter U with circumflex" (insert "Û") :enable (is-rw?)]
      ["Ü	Latin Capital Letter U with diaeresis"  (insert "Ü") :enable (is-rw?)]
      ["Ý	Latin Capital Letter Y with acute"      (insert "Ý") :enable (is-rw?)]
      ["Þ	Latin Capital Letter Thorn"             (insert "Þ") :enable (is-rw?)]
      )
     ("Latin-1 Small Letters"
      ["à 	Latin Small Letter A with grave"      (insert "à") :enable (is-rw?)]
      ["á 	Latin Small Letter A with acute"      (insert "á") :enable (is-rw?)]
      ["â 	Latin Small Letter A with circumflex" (insert "â") :enable (is-rw?)]
      ["ã 	Latin Small Letter A with tilde"      (insert "ã") :enable (is-rw?)]
      ["ä 	Latin Small Letter A with diaeresis"  (insert "ä") :enable (is-rw?)]
      ["å 	Latin Small Letter A with ring above" (insert "å") :enable (is-rw?)]
      ["æ 	Latin Small Letter AE"                (insert "æ") :enable (is-rw?)]
      ["ç 	Latin Small Letter C with cedilla"    (insert "ç") :enable (is-rw?)]
      ["è 	Latin Small Letter E with grave"      (insert "è") :enable (is-rw?)]
      ["é 	Latin Small Letter E with acute"      (insert "é") :enable (is-rw?)]
      ["ê 	Latin Small Letter E with circumflex" (insert "ê") :enable (is-rw?)]
      ["ë 	Latin Small Letter E with diaeresis"  (insert "ë") :enable (is-rw?)]
      ["ì 	Latin Small Letter I with grave"      (insert "ì") :enable (is-rw?)]
      ["í 	Latin Small Letter I with acute"      (insert "í") :enable (is-rw?)]
      ["î 	Latin Small Letter I with circumflex" (insert "î") :enable (is-rw?)]
      ["ï 	Latin Small Letter I with diaeresis"  (insert "ï") :enable (is-rw?)]
      ["ð 	Latin Small Letter Eth"               (insert "ð") :enable (is-rw?)]
      ["ñ 	Latin Small Letter N with tilde"      (insert "ñ") :enable (is-rw?)]
      ["ò 	Latin Small Letter O with grave"      (insert "ò") :enable (is-rw?)]
      ["ó 	Latin Small Letter O with acute"      (insert "ó") :enable (is-rw?)]
      ["ô 	Latin Small Letter O with circumflex" (insert "ô") :enable (is-rw?)]
      ["õ 	Latin Small Letter O with tilde"      (insert "õ") :enable (is-rw?)]
      ["ö 	Latin Small Letter O with diaeresis"  (insert "ö") :enable (is-rw?)]
      ["ø 	Latin Small Letter O with stroke"     (insert "ø") :enable (is-rw?)]
      ["ù 	Latin Small Letter U with grave"      (insert "ù") :enable (is-rw?)]
      ["ú 	Latin Small Letter U with acute"      (insert "ú") :enable (is-rw?)]
      ["û 	Latin Small Letter U with circumflex" (insert "û") :enable (is-rw?)]
      ["ü 	Latin Small Letter U with diaeresis"  (insert "ü") :enable (is-rw?)]
      ["ý 	Latin Small Letter Y with acute"      (insert "ý") :enable (is-rw?)]
      ["þ 	Latin Small Letter Thorn"             (insert "þ") :enable (is-rw?)]
      ["ÿ	Latin Small Letter Y with diaeresis"  (insert "ÿ") :enable (is-rw?)]
      ["ž	Latin Small Letter Z with caron"      (insert "ž") :enable (is-rw?)]
      )
     ("Math"
      ["≈	Almost Equal To"                (insert "≈") :enable (is-rw?)]
      ["⊕	Circled Plus (Exclusive OR)"    (insert "⊕") :enable (is-rw?)]
      ["⊗	Circled Times (Tensor Product)" (insert "⊗") :enable (is-rw?)]
      ["∮	Contour Integral"               (insert "∮") :enable (is-rw?)]
      ["∛	Cube Root"                      (insert "∛") :enable (is-rw?)]
      ["÷	Division Sign"                  (insert "÷") :enable (is-rw?)]
      ["∬	Double Integral"                (insert "∬") :enable (is-rw?)]
      ["″	Double Prime"                   (insert "″") :enable (is-rw?)]
      ["∅	Empty Set"                      (insert "∅") :enable (is-rw?)]
      ["∀	For All"                        (insert "∀") :enable (is-rw?)]
      ["≥	Greater-Than or Equal To"       (insert "≥") :enable (is-rw?)]
      ["≡	Identical To"                   (insert "≡") :enable (is-rw?)]
      ["∞	Infinity"                       (insert "∞") :enable (is-rw?)]
      ["∫	Integral"                       (insert "∫") :enable (is-rw?)]
      ["∩	Intersection"                   (insert "∩") :enable (is-rw?)]
      ["≤	Less-Than or Equal To"          (insert "≤") :enable (is-rw?)]
      ["×	Multiplication Sign"            (insert "×") :enable (is-rw?)]
      ["∧	Logical AND"                    (insert "∧") :enable (is-rw?)]
      ["∨	Logical OR"                     (insert "∨") :enable (is-rw?)]
      ["≠	Not Equal To"                   (insert "≠") :enable (is-rw?)]
      ["¬	Not Sign"                       (insert "¬") :enable (is-rw?)]
      ["∥	Parallel To"                    (insert "∥") :enable (is-rw?)]
      ["∦	Not Parallel To"                (insert "∦") :enable (is-rw?)]
      ["∂	Partial Differential"           (insert "∂") :enable (is-rw?)]
      ["′	Prime"                          (insert "′") :enable (is-rw?)]
      ["∝	Proportional To"                (insert "∝") :enable (is-rw?)]
      ["‴	Triple Prime"                   (insert "‴") :enable (is-rw?)]
      ["±	Plus-Minus Sign"                (insert "±") :enable (is-rw?)]
      ["∓	Minus-or-Plus Sign"             (insert "∓") :enable (is-rw?)]
      ["√	Square Root"                    (insert "√") :enable (is-rw?)]
      ["∄	There Does Not Exist"           (insert "∄") :enable (is-rw?)]
      ["∃	There Exists"                   (insert "∃") :enable (is-rw?)]
      ["∪	Union"                          (insert "∪") :enable (is-rw?)])
     ("Punctuation"
      ["¦	Broken Bar"                                 (insert "¦") :enable (is-rw?)]
      ["‸	Caret"                                      (insert "‸") :enable (is-rw?)]
      ["⁁	Caret Insertion Point"                      (insert "⁁") :enable (is-rw?)]
      ["‼	Double Exclamation Mark"                    (insert "‼") :enable (is-rw?)]
      ["‗	Double Low Line"                            (insert "‗") :enable (is-rw?)]
      ["„	Double Low-9 Quotation Mark"                (insert "„") :enable (is-rw?)]
      ["—	Em Dash"                                    (insert "—") :enable (is-rw?)]
      ["–	En Dash"                                    (insert "–") :enable (is-rw?)]
      ["⁄	Fraction Slash"                             (insert "⁄") :enable (is-rw?)]
      ["―	Horizontal Bar"                             (insert "―") :enable (is-rw?)]
      ["…	Horizontal Ellipsis"                        (insert "…") :enable (is-rw?)]
      ["“	Left Double Quotation Mark"                 (insert "“") :enable (is-rw?)]
      ["‘	Left Single Quotation Mark"                 (insert "‘") :enable (is-rw?)]
      ["«	Left-pointing Double Angle Quotation Mark"  (insert "«") :enable (is-rw?)]
      ["¯	Macron"                                     (insert "¯") :enable (is-rw?)]
      ["·	Middle dot"                                 (insert "·") :enable (is-rw?)]
      ["‾	Overline"                                   (insert "‾") :enable (is-rw?)]
      ["‰	Per Mille Sign"                             (insert "‰") :enable (is-rw?)]
      ["‱	Per Ten Thousand Sign"                      (insert "‱") :enable (is-rw?)]
      ["”	Right Double Quotation Mark"                (insert "”") :enable (is-rw?)]
      ["’	Right Single Quotation Mark"                (insert "’") :enable (is-rw?)]
      ["»	Right-pointing Double-Angle Quotation Mark" (insert "»") :enable (is-rw?)]
      ["‛	Single High-Reversed-9 Quotation Mark"      (insert "‛") :enable (is-rw?)]
      ["‚	Single Low-9 Quotation Mark"                (insert "‚") :enable (is-rw?)]
      ["⁊	Tironian et sign"                           (insert "⁊") :enable (is-rw?)])
     ("Subscripts"
      ["₀	Subscript Zero"                 (insert "₀") :enable (is-rw?)]
      ["₁	Subscript One"                  (insert "₁") :enable (is-rw?)]
      ["₂	Subscript Two"                  (insert "₂") :enable (is-rw?)]
      ["₃	Subscript Three"                (insert "₃") :enable (is-rw?)]
      ["₄	Subscript Four"                 (insert "₄") :enable (is-rw?)]
      ["₅	Subscript Five"                 (insert "₅") :enable (is-rw?)]
      ["₆	Subscript Six"                  (insert "₆") :enable (is-rw?)]
      ["₇	Subscript Seven"                (insert "₇") :enable (is-rw?)]
      ["₈	Subscript Eight"                (insert "₈") :enable (is-rw?)]
      ["₉	Subscript Nine"                 (insert "₉") :enable (is-rw?)]
      ["ₙ	Latin Subscript Small Letter N" (insert "ₙ") :enable (is-rw?)])
     ("Superscripts"
      ["⁰	Superscript Zero"                 (insert "⁰") :enable (is-rw?)]
      ["¹	Superscript One"                  (insert "¹") :enable (is-rw?)]
      ["²	Superscript Two"                  (insert "²") :enable (is-rw?)]
      ["³	Superscript Three"                (insert "³") :enable (is-rw?)]
      ["⁴	Superscript Four"                 (insert "⁴") :enable (is-rw?)]
      ["⁵	Superscript Five"                 (insert "⁵") :enable (is-rw?)]
      ["⁶	Superscript Six"                  (insert "⁶") :enable (is-rw?)]
      ["⁷	Superscript Seven"                (insert "⁷") :enable (is-rw?)]
      ["⁸	Superscript Eight"                (insert "⁸") :enable (is-rw?)]
      ["⁹	Superscript Nine"                 (insert "⁹") :enable (is-rw?)]
      ["ⁿ	Latin Superscript Small Letter n" (insert "ⁿ") :enable (is-rw?)]
      ["⁻	Superscript Minus"                (insert "⁻") :enable (is-rw?)])
     ("Typography"
;;¹      ["א	Hebrew Letter Alef"            (insert "א") :enable (is-rw?)]
      ["★	Black Star"                    (insert "★") :enable (is-rw?)]
      ["•	Bullet"                        (insert "•") :enable (is-rw?)]
      ["†	Dagger"                        (insert "†") :enable (is-rw?)]
      ["°	Degree"                        (insert "°") :enable (is-rw?)]
      ["″	Double Prime (Ditto)"          (insert "″") :enable (is-rw?)]
      ["‡	Double Dagger"                 (insert "‡") :enable (is-rw?)]
      ["ª	Feminine Ordinal Indicator"    (insert "ª") :enable (is-rw?)]
      ["¡	Inverted Exclamation Mark"     (insert "¡") :enable (is-rw?)]
      ["¿	Inverted Question Mark"        (insert "¿") :enable (is-rw?)]
      ["ʌ	Latin Small Letter Turned V"   (insert "ʌ") :enable (is-rw?)]
      ["Ʌ	Latin Capital Letter Turned V" (insert "Ʌ") :enable (is-rw?)]
      ["º	Masculine Ordinal Indicator"   (insert "º") :enable (is-rw?)]
      ["№	Numero Sign"                   (insert "№") :enable (is-rw?)]
      ["¶	Pilcrow Sign"                  (insert "¶") :enable (is-rw?)]
      ["⁋	Reversed Pilcrow Sign"         (insert "⁋") :enable (is-rw?)]
      ["§	Section Sign"                  (insert "§") :enable (is-rw?)]
      ["¨	Spacing Diaresis"              (insert "¨") :enable (is-rw?)]
      ))
    ["Show Unicode Name"       show-unicode-name-at-point t]
    "---"
    ["Insert ‘Cut Here’ Start" insert-chs       :enable (is-rw?)]
    ["Insert ‘Cut Here’ End"   insert-che       :enable (is-rw?)]
    ["Update Copyright"        copyright-update :enable (is-rw?)]
    "---"
    ["Complete Symbol"        completion-at-point                :enable (is-rw?)]
    ["Complete Word Fragment" ispell-complete-word-interior-frag :enable (is-rw?)]
    ["Complete Word Spelling" ispell-complete-word               :enable (is-rw?)]
    "---"
    ["Initialize Perl Mode"   convert-to-perl :visible (visible-convert-to-perl?) :key-sequence nil]
    "---"
    ["Adaptive Fill" filladapt-mode :style toggle :selected filladapt-mode]
    ["Enriched Text" enriched-mode  :style toggle :selected enriched-mode :enable (enable-enriched-mode?)]
    "---"
    ["Auto-update This File" auto-revert-mode      :style toggle :selected auto-revert-mode]
    ["Tail Update This File" auto-revert-tail-mode :style toggle :selected auto-revert-tail-mode]
    "---"
    ["Check Language" langtool-check :enable (is-rw?)]
    ("Spelling"
     ["Flyspell Mode" flyspell-mode :style toggle :selected flyspell-mode :enable (is-rw?)]
     "---"
     ["Spell Check Word"   ispell-word   :active t]
     ["Spell Check Buffer" ispell-buffer :active t]
     ["Spell Check Region" ispell-region :active mark-active])
    ))

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (interactive)
  (while list
    (print (car list))
    (setq list (cdr list))))

(defun current-line ()
  "Return the vertical position of point
in the selected window.  Top line is 0.
Counts each text line only once, even if it wraps."
  (+ (count-lines (window-start) (point))
     (if (= (current-column) 0) 1 0)
     -1))

(defun get-random-value (list)
  "Return a random value from input LIST."
  (eval (nth (random (length list)) list)))

(defun get-unicode-name-at-point ()
  "Get the unicode name of the character at point."
  (interactive "P")
  (let ((char-code (elt (thing-at-point 'char) 0))
        name)
    (setq name (get-char-code-property char-code 'name))
    (when (or (not name)
              (= ?< (elt name 0)))
      (setq name (get-char-code-property char-code 'old-name)))
    name))

(defsubst show-unicode-name-at-point ()
  "Get the unicode name of the character at point."
  (interactive)
  (message "%s" (get-unicode-name-at-point)))
;;
(message "Loading u-tools...done")
(provide 'u-tools)

;;; u-tools.el ends here
