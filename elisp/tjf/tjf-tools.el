;;; tjf-tools.el --- Tools menu definition and associated functions -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2021 Tom Fontaine

;; Author: Tom Fontaine
;; Date:   28-Feb-2016

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
;;           17-Apr-2018 Added ‘magit-status’
;;           31-May-2018 Added ‘powerthesaurus-lookup-word’
;;           14-Jun-2018 Added ‘declutter’
;;           06-Jul-2018 Added ‘paradox-list-packages’
;;           10-Jul-2019 Completed Greek alphabet
;;           19-Jul-2019 Added ‘open-new-shell’
;;           03-Feb-2021 ‘tjf’ overhaul
;;

;;; Code:

(message "Loading tjf-tools...")
(require 'tjf-flags)

;;
(defun tjf:tools/current-line ()
  "Return the line number of the current line in the selected window."
  (+ (count-lines (window-start) (point))
     (if (= (current-column) 0) 1 0)
     -1))

(defun tjf:tools/get-random-value (list)
  "Return a random value from input LIST."
  (eval (nth (random (length list)) list)))

(defun tjf:tools/get-unicode-name-at-point ()
  "Get the unicode name of the character at point."
  (interactive "P")
  (let* ((char-code (elt (thing-at-point 'char) 0))
         (name       (get-char-code-property char-code 'name)))
    ;; (setq name (get-char-code-property char-code 'name))
    (when (or (not name)
              (= ?< (elt name 0)))
      (setq name (get-char-code-property char-code 'old-name)))
    name))

(defun tjf:tools/open-new-shell ()
  "Open a new shell buffer."
  (let* ((shell-number 1)
         (shell-buffer-name "*shell <1>*"))
    (while (not (equal nil (get-buffer shell-buffer-name)))
      (setq shell-number (1+ shell-number)
            shell-buffer-name (format "*shell <%d>*" shell-number)))
    (shell (get-buffer-create shell-buffer-name))))

(defun tjf:tools/print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (interactive)
  (while list
    (print (car list))
    (setq list (cdr list))))

(defun tjf:tools/show-unicode-name-at-point ()
  "Get the unicode name of the character at point."
  (interactive)
  (message "%s" (tjf:tools/get-unicode-name-at-point)))

(defvar tjf:tools/menu
  '("Tools"
    ["Insert Unicode Character..." insert-char :enable (tjf:flags/enable-write?) :key-sequence nil]
    ("Insert Symbol"
     ("Arrows"
      ["←	Leftwards Arrow"  (insert "←") :enable (tjf:flags/enable-write?)]
      ["→	Rightwards Arrow" (insert "→") :enable (tjf:flags/enable-write?)]
      ["↑	Upwards Arrow"    (insert "↑") :enable (tjf:flags/enable-write?)]
      ["↓	Downwards Arrow"  (insert "↓") :enable (tjf:flags/enable-write?)]
      ["↔	Left Right Arrow" (insert "↔") :enable (tjf:flags/enable-write?)]
      ["↕	Up Down Arrow"    (insert "↕") :enable (tjf:flags/enable-write?)]
      ["↗	North East Arrow" (insert "↗") :enable (tjf:flags/enable-write?)]
      ["↖	North West Arrow" (insert "↖") :enable (tjf:flags/enable-write?)]
      ["↘	South East Arrow" (insert "↘") :enable (tjf:flags/enable-write?)]
      ["↙	South West Arrow" (insert "↙") :enable (tjf:flags/enable-write?)]
      "---"
      ["⇇	Leftwards Paired Arrows"  (insert "⇇") :enable (tjf:flags/enable-write?)]
      ["⇉	Rightwards Paired Arrows" (insert "⇉") :enable (tjf:flags/enable-write?)]
      ["⇈	Upwards Paired Arrow"     (insert "⇈") :enable (tjf:flags/enable-write?)]
      ["⇊	Downwards Paired Arrow"   (insert "⇊") :enable (tjf:flags/enable-write?)]
      "---"
      ["⇆	Leftwards Arrow Over Rightwards Arrow"      (insert "⇆") :enable (tjf:flags/enable-write?)]
      ["⇄	Rightwards Arrow Over Leftwards Arrow"      (insert "⇄") :enable (tjf:flags/enable-write?)]
      ["⇅	Upwards Arrow Leftwards Of Downwards Arrow" (insert "⇅") :enable (tjf:flags/enable-write?)]
      ["⇵	Downwards Arrow Leftwards of Upwards Arrow" (insert "⇵") :enable (tjf:flags/enable-write?)]
      "---"
      ["⇐	Leftwards Double Arrow"  (insert "⇐") :enable (tjf:flags/enable-write?)]
      ["⇒	Rightwards Double Arrow" (insert "⇒") :enable (tjf:flags/enable-write?)]
      ["⇑	Upwards Double Arrow"    (insert "⇑") :enable (tjf:flags/enable-write?)]
      ["⇓	Downwards Double Arrow"  (insert "⇓") :enable (tjf:flags/enable-write?)]
      ["⇔	Left Right Double Arrow" (insert "⇔") :enable (tjf:flags/enable-write?)]
      ["⇕	Up Down Double Arrow"    (insert "⇕") :enable (tjf:flags/enable-write?)]
      "---"
      ["↻	Clockwise Open Circle Arrow"     (insert "↻") :enable (tjf:flags/enable-write?)]
      ["↺	Anticlockwise Open Circle Arrow" (insert "↺") :enable (tjf:flags/enable-write?)])
     ("Currency"
      ["֏	Armenian Dram Sign"                     (insert "֏") :enable (tjf:flags/enable-write?)]
      ["₳	Austral Sign"				(insert "₳") :enable (tjf:flags/enable-write?)]
      ["৳	Bengali Rupee Sign"                     (insert "৳") :enable (tjf:flags/enable-write?)]
      ["¢	Cent Sign"                              (insert "¢") :enable (tjf:flags/enable-write?)]
      ["₡	Colón Sign"                             (insert "₡") :enable (tjf:flags/enable-write?)]
      ["₢	Cruzeiro Sign"                          (insert "₢") :enable (tjf:flags/enable-write?)]
      ["¤	Currency Sign"                          (insert "¤") :enable (tjf:flags/enable-write?)]
      ["₯	Drachma Sign"				(insert "₯") :enable (tjf:flags/enable-write?)]
      ["₫	Dong Sign"                              (insert "₫") :enable (tjf:flags/enable-write?)]
      ["€	Euro Sign"                              (insert "€") :enable (tjf:flags/enable-write?)]
      ["₠	Euro-Currency Sign"                     (insert "₠") :enable (tjf:flags/enable-write?)]
      ["₣	French Franc Sign"                      (insert "₣") :enable (tjf:flags/enable-write?)]
      ["₰	German Penny Sign (Pfennig)"            (insert "₰") :enable (tjf:flags/enable-write?)]
      ["₲	Guarani Sign"				(insert "₲") :enable (tjf:flags/enable-write?)]
      ["૱	Gujarati Rupee Sign"                    (insert "૱") :enable (tjf:flags/enable-write?)]
      ["₴	Hryvnia Sign"				(insert "₴") :enable (tjf:flags/enable-write?)]
      ["₹	Indian Rupee Sign"                      (insert "₹") :enable (tjf:flags/enable-write?)]
      ["¤	Khmer Currency Symbol Riel"             (insert "¤") :enable (tjf:flags/enable-write?)]
      ["₭	Kip Sign"                               (insert "₭") :enable (tjf:flags/enable-write?)]
      ["Ƒ	Latin Small Letter F With Hook (Florin)"(insert "Ƒ") :enable (tjf:flags/enable-write?)]
      ["₤	Lira Sign"                              (insert "₤") :enable (tjf:flags/enable-write?)]
      ["₥	Mill Sign"                              (insert "₥") :enable (tjf:flags/enable-write?)]
      ["₦	Naira Sign"                             (insert "₦") :enable (tjf:flags/enable-write?)]
      ["₪	New Sheqel Sign"                        (insert "₪") :enable (tjf:flags/enable-write?)]
      ["₧	Peseta Sign"                            (insert "₧") :enable (tjf:flags/enable-write?)]
      ["₱	Peso Sign"                              (insert "₱") :enable (tjf:flags/enable-write?)]
      ["£	Pound Sign"                             (insert "£") :enable (tjf:flags/enable-write?)]
      ["₽	Ruble Sign"                             (insert "₽") :enable (tjf:flags/enable-write?)]
      ["₨	Rupee Sign"                             (insert "₨") :enable (tjf:flags/enable-write?)]
      ["ℳ	Script Capital M (German Mark)"         (insert "ℳ") :enable (tjf:flags/enable-write?)]
      ["௹	Tamil Rupee Sign"                       (insert "௹") :enable (tjf:flags/enable-write?)]
      ["₸	Tenge Sign"                             (insert "₸") :enable (tjf:flags/enable-write?)]
      ["฿	Thai Currency Symbol Baht"              (insert "฿") :enable (tjf:flags/enable-write?)]
      ["₮	Tugrik Sign"                            (insert "₮") :enable (tjf:flags/enable-write?)]
      ["₺	Turkish Lira Sign"                      (insert "₺") :enable (tjf:flags/enable-write?)]
      ["₩	Won Sign"                               (insert "₩") :enable (tjf:flags/enable-write?)]
      ["¥	Yen Sign"                               (insert "¥") :enable (tjf:flags/enable-write?)])
     ("Greek Symbols"
      ["Α Greek Capital Letter Alpha"   (insert "Α") :enable (tjf:flags/enable-write?)]
      ["Β Greek Capital Letter Beta"    (insert "Β") :enable (tjf:flags/enable-write?)]
      ["Γ Greek Capital Letter Gamma"   (insert "Γ") :enable (tjf:flags/enable-write?)]
      ["Δ Greek Capital Letter Delta"   (insert "Δ") :enable (tjf:flags/enable-write?)]
      ["Γ Greek Capital Letter Gamma"   (insert "Γ") :enable (tjf:flags/enable-write?)]
      ["Ε Greek Capital Letter Epsilon" (insert "Ε") :enable (tjf:flags/enable-write?)]
      ["Ζ Greek Capital Letter Zeta"    (insert "Ζ") :enable (tjf:flags/enable-write?)]
      ["Η Greek Capital Letter Eta"     (insert "Η") :enable (tjf:flags/enable-write?)]
      ["Θ Greek Capital Letter Theta"   (insert "Θ") :enable (tjf:flags/enable-write?)]
      ["Ι Greek Capital Letter Iota"    (insert "Ι") :enable (tjf:flags/enable-write?)]
      ["Κ Greek Capital Letter Kappa"   (insert "Κ") :enable (tjf:flags/enable-write?)]
      ["Λ Greek Capital Letter Lambda"  (insert "Λ") :enable (tjf:flags/enable-write?)]
      ["Μ Greek Capital Letter Mu"      (insert "Μ") :enable (tjf:flags/enable-write?)]
      ["Ν Greek Capital Letter Nu"      (insert "Ν") :enable (tjf:flags/enable-write?)]
      ["Ξ Greek Capital Letter Xi"      (insert "Ξ") :enable (tjf:flags/enable-write?)]
      ["Ο Greek Capital Letter Omicron" (insert "Ο") :enable (tjf:flags/enable-write?)]
      ["Π Greek Capital Letter Pi"      (insert "Π") :enable (tjf:flags/enable-write?)]
      ["Σ Greek Capital Letter Sigma"   (insert "∑") :enable (tjf:flags/enable-write?)]
      ["Ρ Greek Capital Letter Rho"     (insert "Ρ") :enable (tjf:flags/enable-write?)]
      ["Σ Greek Capital Letter Sigma"   (insert "Σ") :enable (tjf:flags/enable-write?)]
      ["Τ Greek Capital Letter Tau"     (insert "Τ") :enable (tjf:flags/enable-write?)]
      ["Υ Greek Capital Letter Upsilon" (insert "Υ") :enable (tjf:flags/enable-write?)]
      ["Φ Greek Capital Letter Phi"     (insert "Φ") :enable (tjf:flags/enable-write?)]
      ["Χ Greek Capital Letter Chi"     (insert "Χ") :enable (tjf:flags/enable-write?)]
      ["Ψ Greek Capital Letter Psi"     (insert "Ψ") :enable (tjf:flags/enable-write?)]
      ["Ω Greek Capital Letter Omega"   (insert "Ω") :enable (tjf:flags/enable-write?)]
      "---"
      ["α	Greek Small Letter Alpha"   (insert "α") :enable (tjf:flags/enable-write?)]
      ["β	Greek Small Letter Beta"    (insert "β") :enable (tjf:flags/enable-write?)]
      ["γ	Greek Small Letter Gamma"   (insert "γ") :enable (tjf:flags/enable-write?)]
      ["δ       Greek Small Letter Delta"   (insert "δ") :enable (tjf:flags/enable-write?)]
      ["ε       Greek Small Letter Epsilon" (insert "ε") :enable (tjf:flags/enable-write?)]
      ["ζ       Greek Small Letter Zeta"    (insert "ζ") :enable (tjf:flags/enable-write?)]
      ["η       Greek Small Letter Eta"     (insert "η") :enable (tjf:flags/enable-write?)]
      ["θ	Greek Small Letter Theta"   (insert "θ") :enable (tjf:flags/enable-write?)]
      ["ι       Greek Small Letter Iota"    (insert "ι") :enable (tjf:flags/enable-write?)]
      ["κ       Greek Small Letter Kappa"   (insert "κ") :enable (tjf:flags/enable-write?)]
      ["λ	Greek Small Letter Lambda"  (insert "λ") :enable (tjf:flags/enable-write?)]
      ["μ	Greek Small Letter Mu"      (insert "μ") :enable (tjf:flags/enable-write?)]
      ["ν       Greek Small Letter Nu"      (insert "ν") :enable (tjf:flags/enable-write?)]
      ["ξ       Greek Small Letter Xi"      (insert "ξ") :enable (tjf:flags/enable-write?)]
      ["ο       Greek Small Letter Omicron" (insert "ο") :enable (tjf:flags/enable-write?)]
      ["π	Greek Small Letter Pi"      (insert "π") :enable (tjf:flags/enable-write?)]
      ["ρ       Greek Small Letter Rho"     (insert "ρ") :enable (tjf:flags/enable-write?)]
      ["σ	Greek Small Letter Sigma"   (insert "σ") :enable (tjf:flags/enable-write?)]
      ["τ	Greek Small Letter Tau"     (insert "τ") :enable (tjf:flags/enable-write?)]
      ["υ       Greek Small Letter Upsilon" (insert "υ") :enable (tjf:flags/enable-write?)]
      ["φ	Greek Small Letter Phi"     (insert "φ") :enable (tjf:flags/enable-write?)]
      ["χ       Greek Small Letter Chi"     (insert "χ") :enable (tjf:flags/enable-write?)]
      ["ψ	Greek Small Letter Psi"     (insert "ψ") :enable (tjf:flags/enable-write?)]
      ["ω	Greek Small Letter Omega"   (insert "ω") :enable (tjf:flags/enable-write?)]
      )
     ("Intellectual Property"
      ["ⓟ	Circled Latin Small Letter P (Sound)" (insert "ⓟ") :enable (tjf:flags/enable-write?)]
      ["©	Copyright Sign"                       (insert "©") :enable (tjf:flags/enable-write?)]
      ["®	Registered Sign"                      (insert "®") :enable (tjf:flags/enable-write?)]
      ["℠	Service Mark"                         (insert "℠") :enable (tjf:flags/enable-write?)]
      ["™	Trade Mark Sign"                      (insert "™") :enable (tjf:flags/enable-write?)])
     ("Latin-1 Capital Letters"
      ["À	Latin Capital Letter A with grave"      (insert "À") :enable (tjf:flags/enable-write?)]
      ["Á	Latin Capital letter A with acute"      (insert "Á") :enable (tjf:flags/enable-write?)]
      ["Â	Latin Capital letter A with circumflex" (insert "Â") :enable (tjf:flags/enable-write?)]
      ["Ã	Latin Capital letter A with tilde"      (insert "Ã") :enable (tjf:flags/enable-write?)]
      ["Ä	Latin Capital letter A with diaeresis"  (insert "Ä") :enable (tjf:flags/enable-write?)]
      ["Å	Latin Capital letter A with ring above" (insert "Å") :enable (tjf:flags/enable-write?)]
      ["Æ	Latin Capital letter AE"                (insert "Æ") :enable (tjf:flags/enable-write?)]
      ["Ç	Latin Capital letter C with cedilla"    (insert "Ç") :enable (tjf:flags/enable-write?)]
      ["È	Latin Capital letter E with grave"      (insert "È") :enable (tjf:flags/enable-write?)]
      ["É	Latin Capital letter E with acute"      (insert "É") :enable (tjf:flags/enable-write?)]
      ["Ê	Latin Capital letter E with circumflex" (insert "Ê") :enable (tjf:flags/enable-write?)]
      ["Ë	Latin Capital letter E with diaeresis"  (insert "Ë") :enable (tjf:flags/enable-write?)]
      ["Ì	Latin Capital letter I with grave"      (insert "Ì") :enable (tjf:flags/enable-write?)]
      ["Í	Latin Capital letter I with acute"      (insert "Í") :enable (tjf:flags/enable-write?)]
      ["Î	Latin Capital letter I with circumflex" (insert "Î") :enable (tjf:flags/enable-write?)]
      ["Ï	Latin Capital letter I with diaeresis"  (insert "Ï") :enable (tjf:flags/enable-write?)]
      ["Ð	Latin Capital letter Eth"               (insert "Ð") :enable (tjf:flags/enable-write?)]
      ["Ñ	Latin Capital letter N with tilde"      (insert "Ñ") :enable (tjf:flags/enable-write?)]
      ["Ò	Latin Capital letter O with grave"      (insert "Ò") :enable (tjf:flags/enable-write?)]
      ["Ó	Latin Capital letter O with acute"      (insert "Ó") :enable (tjf:flags/enable-write?)]
      ["Ô	Latin Capital letter O with circumflex" (insert "Ô") :enable (tjf:flags/enable-write?)]
      ["Õ	Latin Capital letter O with tilde"      (insert "Õ") :enable (tjf:flags/enable-write?)]
      ["Ö	Latin Capital letter O with diaeresis"  (insert "Ö") :enable (tjf:flags/enable-write?)]
      ["Ø	Latin Capital letter O with stroke"     (insert "Ø") :enable (tjf:flags/enable-write?)]
      ["Ù	Latin Capital letter U with grave"      (insert "Ù") :enable (tjf:flags/enable-write?)]
      ["Ú	Latin Capital letter U with acute"      (insert "Ú") :enable (tjf:flags/enable-write?)]
      ["Û	Latin Capital Letter U with circumflex" (insert "Û") :enable (tjf:flags/enable-write?)]
      ["Ü	Latin Capital Letter U with diaeresis"  (insert "Ü") :enable (tjf:flags/enable-write?)]
      ["Ý	Latin Capital Letter Y with acute"      (insert "Ý") :enable (tjf:flags/enable-write?)]
      ["Þ	Latin Capital Letter Thorn"             (insert "Þ") :enable (tjf:flags/enable-write?)]
      )
     ("Latin-1 Small Letters"
      ["à 	Latin Small Letter A with grave"      (insert "à") :enable (tjf:flags/enable-write?)]
      ["á 	Latin Small Letter A with acute"      (insert "á") :enable (tjf:flags/enable-write?)]
      ["â 	Latin Small Letter A with circumflex" (insert "â") :enable (tjf:flags/enable-write?)]
      ["ã 	Latin Small Letter A with tilde"      (insert "ã") :enable (tjf:flags/enable-write?)]
      ["ä 	Latin Small Letter A with diaeresis"  (insert "ä") :enable (tjf:flags/enable-write?)]
      ["å 	Latin Small Letter A with ring above" (insert "å") :enable (tjf:flags/enable-write?)]
      ["æ 	Latin Small Letter AE"                (insert "æ") :enable (tjf:flags/enable-write?)]
      ["ç 	Latin Small Letter C with cedilla"    (insert "ç") :enable (tjf:flags/enable-write?)]
      ["è 	Latin Small Letter E with grave"      (insert "è") :enable (tjf:flags/enable-write?)]
      ["é 	Latin Small Letter E with acute"      (insert "é") :enable (tjf:flags/enable-write?)]
      ["ê 	Latin Small Letter E with circumflex" (insert "ê") :enable (tjf:flags/enable-write?)]
      ["ë 	Latin Small Letter E with diaeresis"  (insert "ë") :enable (tjf:flags/enable-write?)]
      ["ì 	Latin Small Letter I with grave"      (insert "ì") :enable (tjf:flags/enable-write?)]
      ["í 	Latin Small Letter I with acute"      (insert "í") :enable (tjf:flags/enable-write?)]
      ["î 	Latin Small Letter I with circumflex" (insert "î") :enable (tjf:flags/enable-write?)]
      ["ï 	Latin Small Letter I with diaeresis"  (insert "ï") :enable (tjf:flags/enable-write?)]
      ["ð 	Latin Small Letter Eth"               (insert "ð") :enable (tjf:flags/enable-write?)]
      ["ñ 	Latin Small Letter N with tilde"      (insert "ñ") :enable (tjf:flags/enable-write?)]
      ["ò 	Latin Small Letter O with grave"      (insert "ò") :enable (tjf:flags/enable-write?)]
      ["ó 	Latin Small Letter O with acute"      (insert "ó") :enable (tjf:flags/enable-write?)]
      ["ô 	Latin Small Letter O with circumflex" (insert "ô") :enable (tjf:flags/enable-write?)]
      ["õ 	Latin Small Letter O with tilde"      (insert "õ") :enable (tjf:flags/enable-write?)]
      ["ö 	Latin Small Letter O with diaeresis"  (insert "ö") :enable (tjf:flags/enable-write?)]
      ["ø 	Latin Small Letter O with stroke"     (insert "ø") :enable (tjf:flags/enable-write?)]
      ["ù 	Latin Small Letter U with grave"      (insert "ù") :enable (tjf:flags/enable-write?)]
      ["ú 	Latin Small Letter U with acute"      (insert "ú") :enable (tjf:flags/enable-write?)]
      ["û 	Latin Small Letter U with circumflex" (insert "û") :enable (tjf:flags/enable-write?)]
      ["ü 	Latin Small Letter U with diaeresis"  (insert "ü") :enable (tjf:flags/enable-write?)]
      ["ý 	Latin Small Letter Y with acute"      (insert "ý") :enable (tjf:flags/enable-write?)]
      ["þ 	Latin Small Letter Thorn"             (insert "þ") :enable (tjf:flags/enable-write?)]
      ["ÿ	Latin Small Letter Y with diaeresis"  (insert "ÿ") :enable (tjf:flags/enable-write?)]
      ["ž	Latin Small Letter Z with caron"      (insert "ž") :enable (tjf:flags/enable-write?)]
      )
     ("Math"
      ["≈	Almost Equal To"                (insert "≈") :enable (tjf:flags/enable-write?)]
      ["⊕	Circled Plus (Exclusive OR)"    (insert "⊕") :enable (tjf:flags/enable-write?)]
      ["⊗	Circled Times (Tensor Product)" (insert "⊗") :enable (tjf:flags/enable-write?)]
      ["∮	Contour Integral"               (insert "∮") :enable (tjf:flags/enable-write?)]
      ["∛	Cube Root"                      (insert "∛") :enable (tjf:flags/enable-write?)]
      ["÷	Division Sign"                  (insert "÷") :enable (tjf:flags/enable-write?)]
      ["∬	Double Integral"                (insert "∬") :enable (tjf:flags/enable-write?)]
      ["″	Double Prime"                   (insert "″") :enable (tjf:flags/enable-write?)]
      ["∅	Empty Set"                      (insert "∅") :enable (tjf:flags/enable-write?)]
      ["∀	For All"                        (insert "∀") :enable (tjf:flags/enable-write?)]
      ["≥	Greater-Than or Equal To"       (insert "≥") :enable (tjf:flags/enable-write?)]
      ["≡	Identical To"                   (insert "≡") :enable (tjf:flags/enable-write?)]
      ["∞	Infinity"                       (insert "∞") :enable (tjf:flags/enable-write?)]
      ["∫	Integral"                       (insert "∫") :enable (tjf:flags/enable-write?)]
      ["∩	Intersection"                   (insert "∩") :enable (tjf:flags/enable-write?)]
      ["≤	Less-Than or Equal To"          (insert "≤") :enable (tjf:flags/enable-write?)]
      ["×	Multiplication Sign"            (insert "×") :enable (tjf:flags/enable-write?)]
      ["∧	Logical AND"                    (insert "∧") :enable (tjf:flags/enable-write?)]
      ["∨	Logical OR"                     (insert "∨") :enable (tjf:flags/enable-write?)]
      ["≠	Not Equal To"                   (insert "≠") :enable (tjf:flags/enable-write?)]
      ["¬	Not Sign"                       (insert "¬") :enable (tjf:flags/enable-write?)]
      ["∥	Parallel To"                    (insert "∥") :enable (tjf:flags/enable-write?)]
      ["∦	Not Parallel To"                (insert "∦") :enable (tjf:flags/enable-write?)]
      ["∂	Partial Differential"           (insert "∂") :enable (tjf:flags/enable-write?)]
      ["′	Prime"                          (insert "′") :enable (tjf:flags/enable-write?)]
      ["∝	Proportional To"                (insert "∝") :enable (tjf:flags/enable-write?)]
      ["‴	Triple Prime"                   (insert "‴") :enable (tjf:flags/enable-write?)]
      ["±	Plus-Minus Sign"                (insert "±") :enable (tjf:flags/enable-write?)]
      ["∓	Minus-or-Plus Sign"             (insert "∓") :enable (tjf:flags/enable-write?)]
      ["√	Square Root"                    (insert "√") :enable (tjf:flags/enable-write?)]
      ["∄	There Does Not Exist"           (insert "∄") :enable (tjf:flags/enable-write?)]
      ["∃	There Exists"                   (insert "∃") :enable (tjf:flags/enable-write?)]
      ["∪	Union"                          (insert "∪") :enable (tjf:flags/enable-write?)])
     ("Punctuation"
      ["¦	Broken Bar"                                 (insert "¦") :enable (tjf:flags/enable-write?)]
      ["‸	Caret"                                      (insert "‸") :enable (tjf:flags/enable-write?)]
      ["⁁	Caret Insertion Point"                      (insert "⁁") :enable (tjf:flags/enable-write?)]
      ["‼	Double Exclamation Mark"                    (insert "‼") :enable (tjf:flags/enable-write?)]
      ["‗	Double Low Line"                            (insert "‗") :enable (tjf:flags/enable-write?)]
      ["„	Double Low-9 Quotation Mark"                (insert "„") :enable (tjf:flags/enable-write?)]
      ["—	Em Dash"                                    (insert "—") :enable (tjf:flags/enable-write?)]
      ["–	En Dash"                                    (insert "–") :enable (tjf:flags/enable-write?)]
      ["⁄	Fraction Slash"                             (insert "⁄") :enable (tjf:flags/enable-write?)]
      ["―	Horizontal Bar"                             (insert "―") :enable (tjf:flags/enable-write?)]
      ["…	Horizontal Ellipsis"                        (insert "…") :enable (tjf:flags/enable-write?)]
      ["“	Left Double Quotation Mark"                 (insert "“") :enable (tjf:flags/enable-write?)]
      ["‘	Left Single Quotation Mark"                 (insert "‘") :enable (tjf:flags/enable-write?)]
      ["«	Left-pointing Double Angle Quotation Mark"  (insert "«") :enable (tjf:flags/enable-write?)]
      ["¯	Macron"                                     (insert "¯") :enable (tjf:flags/enable-write?)]
      ["·	Middle dot"                                 (insert "·") :enable (tjf:flags/enable-write?)]
      ["‾	Overline"                                   (insert "‾") :enable (tjf:flags/enable-write?)]
      ["‰	Per Mille Sign"                             (insert "‰") :enable (tjf:flags/enable-write?)]
      ["‱	Per Ten Thousand Sign"                      (insert "‱") :enable (tjf:flags/enable-write?)]
      ["”	Right Double Quotation Mark"                (insert "”") :enable (tjf:flags/enable-write?)]
      ["’	Right Single Quotation Mark"                (insert "’") :enable (tjf:flags/enable-write?)]
      ["»	Right-pointing Double-Angle Quotation Mark" (insert "»") :enable (tjf:flags/enable-write?)]
      ["‛	Single High-Reversed-9 Quotation Mark"      (insert "‛") :enable (tjf:flags/enable-write?)]
      ["‚	Single Low-9 Quotation Mark"                (insert "‚") :enable (tjf:flags/enable-write?)]
      ["⁊	Tironian et sign"                           (insert "⁊") :enable (tjf:flags/enable-write?)])
     ("Subscripts"
      ["₀	Subscript Zero"                 (insert "₀") :enable (tjf:flags/enable-write?)]
      ["₁	Subscript One"                  (insert "₁") :enable (tjf:flags/enable-write?)]
      ["₂	Subscript Two"                  (insert "₂") :enable (tjf:flags/enable-write?)]
      ["₃	Subscript Three"                (insert "₃") :enable (tjf:flags/enable-write?)]
      ["₄	Subscript Four"                 (insert "₄") :enable (tjf:flags/enable-write?)]
      ["₅	Subscript Five"                 (insert "₅") :enable (tjf:flags/enable-write?)]
      ["₆	Subscript Six"                  (insert "₆") :enable (tjf:flags/enable-write?)]
      ["₇	Subscript Seven"                (insert "₇") :enable (tjf:flags/enable-write?)]
      ["₈	Subscript Eight"                (insert "₈") :enable (tjf:flags/enable-write?)]
      ["₉	Subscript Nine"                 (insert "₉") :enable (tjf:flags/enable-write?)]
      ["ₙ	Latin Subscript Small Letter N" (insert "ₙ") :enable (tjf:flags/enable-write?)])
     ("Superscripts"
      ["⁰	Superscript Zero"                 (insert "⁰") :enable (tjf:flags/enable-write?)]
      ["¹	Superscript One"                  (insert "¹") :enable (tjf:flags/enable-write?)]
      ["²	Superscript Two"                  (insert "²") :enable (tjf:flags/enable-write?)]
      ["³	Superscript Three"                (insert "³") :enable (tjf:flags/enable-write?)]
      ["⁴	Superscript Four"                 (insert "⁴") :enable (tjf:flags/enable-write?)]
      ["⁵	Superscript Five"                 (insert "⁵") :enable (tjf:flags/enable-write?)]
      ["⁶	Superscript Six"                  (insert "⁶") :enable (tjf:flags/enable-write?)]
      ["⁷	Superscript Seven"                (insert "⁷") :enable (tjf:flags/enable-write?)]
      ["⁸	Superscript Eight"                (insert "⁸") :enable (tjf:flags/enable-write?)]
      ["⁹	Superscript Nine"                 (insert "⁹") :enable (tjf:flags/enable-write?)]
      ["ⁿ	Latin Superscript Small Letter n" (insert "ⁿ") :enable (tjf:flags/enable-write?)]
      ["⁻	Superscript Minus"                (insert "⁻") :enable (tjf:flags/enable-write?)])
     ("Typography"
;;¹      ["א	Hebrew Letter Alef"            (insert "א") :enable (tjf:flags/enable-write?)]
      ["★	Black Star"                    (insert "★") :enable (tjf:flags/enable-write?)]
      ["•	Bullet"                        (insert "•") :enable (tjf:flags/enable-write?)]
      ["†	Dagger"                        (insert "†") :enable (tjf:flags/enable-write?)]
      ["°	Degree"                        (insert "°") :enable (tjf:flags/enable-write?)]
      ["″	Double Prime (Ditto)"          (insert "″") :enable (tjf:flags/enable-write?)]
      ["‡	Double Dagger"                 (insert "‡") :enable (tjf:flags/enable-write?)]
      ["ª	Feminine Ordinal Indicator"    (insert "ª") :enable (tjf:flags/enable-write?)]
      ["¡	Inverted Exclamation Mark"     (insert "¡") :enable (tjf:flags/enable-write?)]
      ["¿	Inverted Question Mark"        (insert "¿") :enable (tjf:flags/enable-write?)]
      ["ʌ	Latin Small Letter Turned V"   (insert "ʌ") :enable (tjf:flags/enable-write?)]
      ["Ʌ	Latin Capital Letter Turned V" (insert "Ʌ") :enable (tjf:flags/enable-write?)]
      ["º	Masculine Ordinal Indicator"   (insert "º") :enable (tjf:flags/enable-write?)]
      ["№	Numero Sign"                   (insert "№") :enable (tjf:flags/enable-write?)]
      ["¶	Pilcrow Sign"                  (insert "¶") :enable (tjf:flags/enable-write?)]
      ["⁋	Reversed Pilcrow Sign"         (insert "⁋") :enable (tjf:flags/enable-write?)]
      ["§	Section Sign"                  (insert "§") :enable (tjf:flags/enable-write?)]
      ["¨	Spacing Diaresis"              (insert "¨") :enable (tjf:flags/enable-write?)]
      ))
    ["Show Unicode Name" show-unicode-name-at-point t]
    "---"
    ["Git Status" magit-status]
    "---"
    ["Check Language"             langtool-check             :enable (tjf:flags/enable-write?)]
    ["Insert Synonym..."          powerthesaurus-lookup-word :enable (tjf:flags/enable-write?)]
    ["Look up word definition..." sdcv-search                :active t]
    "---"
    ["Insert ‘Cut Here’ Start" insert-chs       :enable (tjf:flags/enable-write?)]
    ["Insert ‘Cut Here’ End"   insert-che       :enable (tjf:flags/enable-write?)]
    ["Update Copyright"        copyright-update :enable (tjf:flags/enable-write?)]
    "---"
    ["Read from URL..." declutter :active t]
    "---"
    ["List Packages"    paradox-list-packages :active t]
    "---"
    ["Complete Symbol"        completion-at-point                :enable (tjf:flags/enable-write?)]
    ["Complete Word Fragment" ispell-complete-word-interior-frag :enable (tjf:flags/enable-write?)]
    ["Complete Word Spelling" ispell-complete-word               :enable (tjf:flags/enable-write?)]
    "---"
    ["Initialize Perl Mode"   tjf:perl/convert :visible (tjf:flags/visible-convert-to-perl?) :key-sequence nil]
    "---"
    ["Adaptive Fill" filladapt-mode :style toggle :selected filladapt-mode]
    ["Enriched Text" enriched-mode  :style toggle :selected enriched-mode :enable (tjf:flags/enable-enriched-mode?)]
    "---"
    ["Auto-update This File" auto-revert-mode      :style toggle :selected auto-revert-mode]
    ["Tail Update This File" auto-revert-tail-mode :style toggle :selected auto-revert-tail-mode]
    ("Spelling"
     ["Flyspell Mode" flyspell-mode :style toggle :selected flyspell-mode :enable (tjf:flags/enable-write?)]
     "---"
     ["Spell Check Word"   ispell-word   :active t]
     ["Spell Check Buffer" ispell-buffer :active t]
     ["Spell Check Region" ispell-region :active mark-active])
    ))

;;
(message "Loading tjf-tools...done")
(provide 'tjf-tools)

;;; tjf-tools.el ends here
