;;; tjf-fonts.el --- font definitions -*-lexical-binding: t-*- ;; -*-Emacs-Lisp-*-

;;         Copyright © 2016-2026  Tom Fontaine

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

;; Does this file belong in elisp/themes?

;;; Code:

;;
(defvar tjf:fonts/fixed    nil "Preferred available monospace/fixed-width font.")
(defvar tjf:fonts/variable nil "Preferred available variable width font.")

(let* ((recursive-mono "Recursive Mono Static Beta 1.019")

       (font-family-list (font-family-list))
       (anka              (car (member "Anka/Coder"               font-family-list)))
       (bitstream         (car (member "Bitstream Vera Sans Mono" font-family-list)))
       (camingo           (car (member "CamingoCode"              font-family-list)))
       (consolas          (car (member "Consolas"                 font-family-list)))
       (courier           (car (member "Courier 10 Pitch"         font-family-list)))
       (courier-prime     (car (member "Courier Prime Code"       font-family-list)))
       (cousine           (car (member "Cousine"                  font-family-list)))
       (dm                (car (member "DM Mono"                  font-family-list)))
       (envy              (car (member "Envy Code R"              font-family-list)))
       (fantasque         (car (member "Fantasque Sans Mono"      font-family-list)))
       (firacode          (car (member "Fira Code"                font-family-list)))
       (go-mono           (car (member "Go Mono"                  font-family-list)))
       (hack              (car (member "Hack"                     font-family-list)))
       (hack-nerd         (car (member "Hack Nerd Font"           font-family-list)))
       (hermit            (car (member "Hermit"                   font-family-list)))
       (ia-writer         (car (member "iA Writer Mono S"         font-family-list)))
       (inconsolata       (car (member "Inconsolata"              font-family-list)))
       (input             (car (member "Input Mono"               font-family-list)))
       (intel-one         (car (member "IntelOne Mono"            font-family-list)))
       (iosevka           (car (member "Iosevka"                  font-family-list)))
       (jet               (car (member "JetBrains Mono"           font-family-list)))
       (julia             (car (member "JuliaMono"                font-family-list)))
       (liberation        (car (member "Liberation Mono"          font-family-list)))
       (lilex             (car (member "Lilex"                    font-family-list)))
       (lotion            (car (member "Lotion"                   font-family-list)))
       (luculent          (car (member "Luculent"                 font-family-list)))
       (meslo             (car (member "Meslo LG S DZ"            font-family-list)))
       (monaspace-argon   (car (member "Monaspace Argon"          font-family-list)))
       (monaspace-krypton (car (member "Monaspace Krypton"        font-family-list)))
       (monaspace-radon   (car (member "Monaspace Radon"          font-family-list)))
       (monofoki          (car (member "Monofoki"                 font-family-list)))
       (monoid            (car (member "Monoid"                   font-family-list)))
       (recursive         (car (member recursive-mono             font-family-list)))
       (source-code-pro   (car (member "Source Code Pro"          font-family-list)))
       (space             (car (member "Space Mono"               font-family-list)))
       (twilio            (car (member "Twilio Sans Mono Retina"  font-family-list)))
       (ubuntu            (car (member "Ubuntu Mono"              font-family-list)))
       (victor            (car (member "Victor Mono"              font-family-list)))

       (avenir-next           (car (member "Avenir Next Rounded Pro" font-family-list)))
       (dejavu-sans           (car (member "DejaVu Sans"             font-family-list)))
       (go                    (car (member "Go"                      font-family-list)))
       (inter                 (car (member "Inter V"                 font-family-list)))
       (monaspace-argon-var   (car (member "Monaspace Argon"         font-family-list)))
       (monaspace-krypton-var (car (member "Monaspace Krypton"       font-family-list)))
       (monaspace-neon-var    (car (member "Monaspace Neon"          font-family-list)))
       (monaspace-radon-var   (car (member "Monaspace Radon"         font-family-list)))
       (monaspace-xenon-var   (car (member "Monaspace Xenon"         font-family-list)))
       (nimbus-sans           (car (member "Nimbus Sans"             font-family-list)))
       (noto-sans             (car (member "Noto Sans"               font-family-list)))
       (open-sans             (car (member "Open Sans"               font-family-list)))
       (roboto                (car (member "Roboto"                  font-family-list)))
       (segoe-ui              (car (member "Segoe UI"                font-family-list)))
       (source-sans           (car (member "Source Sans Pro"         font-family-list)))

       (fixed-font
        (or hack bitstream envy ubuntu fantasque
            anka lilex julia cousine input recursive liberation monofoki consolas
            meslo twilio camingo dm source-code-pro iosevka courier-prime
            ia-writer monoid firacode
            hermit intel-one
            go-mono monaspace-argon monaspace-krypton monaspace-radon
            space
            luculent inconsolata lotion
            victor
            firacode
            courier))

       (variable-font
        (or inter nimbus-sans roboto go avenir-next noto-sans source-sans
            monaspace-argon-var monaspace-krypton-var monaspace-neon-var
            monaspace-radon-var monaspace-xenon-var dejavu-sans open-sans
            segoe-ui)))

  (setq tjf:fonts/fixed    fixed-font)
  (setq tjf:fonts/variable variable-font))

(provide 'tjf-fonts)
;;; tjf-fonts.el ends here
