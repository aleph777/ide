# Ascii --- Provides named ASCII characters as constants and a hash -*-Perl-*-

#         Copyright © 2015-2023 Tom Fontaine

# Author: Tom Fontaine
# Date:   25-Mar-2015

# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software",
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# Except as contained in this notice, the name(s of the above copyright
# holders shall not be used in advertising or otherwise to promote the sale,
# use or other dealings in this Software without prior written authorization.

# The software is provided "As Is", without warranty of any kind, express or
# implied, including but not limited to the warranties of merchantability,
# fitness for a particular purpose and noninfringement. In no event shall
# the authors or copyright holders be liable for any claim, damages or other
# liability, whether in an action of contract, tort or otherwise, arising
# from, out of or in connection with the software or the use or other
# dealings in the software.

#
# Revision: 26-Mar-2015 Added %char;
#           18-Mar-2016 Added chr 128 .. 255
#           14-Jun-2023 use Modern::Perl
#
# Usage:
#             use Ascii;
#               .
#               .
#               .
#             use constant BACKSLASH_BACKSLASH => Ascii::BACKSLASH x 2;
#
#               or
#
#             my $bs = Ascii::BACKSLASH;
#
#               also
#
#             print "==> $Ascii::char{'QUOTE'}\n"
#

# Code:

package Ascii;

use Carp;
use Exporter 'import';
use Modern::Perl;

our @ASCII_CHARNAMES;

our @EXPORT_OK = @ASCII_CHARNAMES;

use constant NUL => chr(0);
use constant SOH => chr(1);
use constant STX => chr(2);
use constant ETX => chr(3);
use constant EOT => chr(4);
use constant ENQ => chr(5);
use constant ACK => chr(6);
use constant BEL => chr(7);
use constant BS  => chr(8);
use constant HT  => chr(9);
use constant LF  => chr(10);
use constant VT  => chr(11);
use constant FF  => chr(12);
use constant CR  => chr(13);
use constant SO  => chr(14);
use constant SI  => chr(15);
use constant DLE => chr(16);
use constant DC1 => chr(17);
use constant DC2 => chr(18);
use constant DC3 => chr(19);
use constant DC4 => chr(20);
use constant NAK => chr(21);
use constant SYN => chr(22);
use constant ETB => chr(23);
use constant CAN => chr(24);
use constant EM  => chr(25);
use constant SUB => chr(26);
use constant ESC => chr(27);
use constant FS  => chr(28);
use constant GS  => chr(29);
use constant RS  => chr(30);
use constant US  => chr(31);

use constant DOUBLE_QUOTES => chr(34);
use constant SINGLE_QUOTE  => chr(39);
use constant QUOTE         => chr(39);
use constant BACKSLASH     => chr(92);
use constant DEL           => chr(127); # 

use constant EURO_SIGN => chr(128); # €

use constant SINGLE_LOW_9_QUOTATION_MARK               => chr(130); # ‚
use constant LATIN_SMALL_LETTER_F_WITH_HOOK            => chr(131); # Ƒ
use constant DOUBLE_LOW_9_QUOTATION_MARK               => chr(132); # „
use constant HORIZONTAL_ELLIPSIS                       => chr(133); # …
use constant DAGGER                                    => chr(134); # †
use constant DOUBLE_DAGGER                             => chr(135); # ‡
use constant MODIFIER_LETTER_CIRCUMFLEX_ACCENT         => chr(136); # ˆ
use constant PER_MILLE_SIGN                            => chr(137); # ‰
use constant LATIN_CAPITAL_LETTER_S_WITH_CARON         => chr(138); # Š
use constant SINGLE_LEFT_POINTING_ANGLE_QUOTATION_MARK => chr(139); # ‹
use constant LATIN_CAPITAL_LIGATURE_OE                 => chr(140); # Œ

use constant LATIN_CAPITAL_LETTER_Z_WITH_CARON => chr(141); # Ž

use constant LEFT_SINGLE_QUOTATION_MARK                 => chr(145); # ‘
use constant RIGHT_SINGLE_QUOTATION_MARK                => chr(146); # ’
use constant LEFT_DOUBLE_QUOTATION_MARK                 => chr(147); # “
use constant RIGHT_DOUBLE_QUOTATION_MARK                => chr(148); # ”
use constant BULLET                                     => chr(149); # •
use constant EN_DASH                                    => chr(150); # –
use constant EM_DASH                                    => chr(151); # —
use constant SMALL_TILDE                                => chr(152); # ˜
use constant TRADE_MARK_SIGN                            => chr(153); # ®
use constant LATIN_SMALL_LETTER_S_WITH_CARON            => chr(154); # š
use constant SINGLE_RIGHT_POINTING_ANGLE_QUOTATION_MARK => chr(155); # ›
use constant LATIN_SMALL_LIGATURE_OE                    => chr(156); # œ

use constant LATIN_SMALL_LETTER_Z_WITH_CARON        => chr(158); # ž
use constant LATIN_CAPITAL_Y_WITH_DIAERESIS         => chr(159); # Ÿ
use constant NON_BREAKING_SPACE                     => chr(160); #
use constant INVERTED_EXCLAMATION_MARK              => chr(161); # ¡
use constant CENT_SIGN                              => chr(162); # ¢
use constant POUND_SIGN                             => chr(163); # £
use constant CURRENCY_SIGN                          => chr(164); # ¤
use constant YEN_SIGN                               => chr(165); # ¥
use constant BROKEN_VERTICAL_BAR                    => chr(166); # ¦
use constant SECTION_SIGN                           => chr(167); # §
use constant SPACING_DIAERESIS                      => chr(168); # ¨
use constant COPYRIGHT_SIGN                         => chr(169); # ©
use constant FEMININE_ORDINAL_INDICATOR             => chr(170); # ª
use constant LEFT_DOUBLE_ANGLE_QUOTES               => chr(171); # «
use constant NOT_SIGN                               => chr(172); # ¬
use constant SOFT_HYPHEN                            => chr(173); #
use constant REGISTERED_TRADE_MARK_SIGN             => chr(174); # ®
use constant SPACING_MACRON                         => chr(175); # ¯
use constant DEGREE_SIGN                            => chr(176); # °
use constant PLUS_OR_MINUS_SIGN                     => chr(177); # ±
use constant PLUS_MINUS_SIGN                        => chr(177); # ±
use constant SUPERSCRIPT_TWO                        => chr(178); # ²
use constant SUPERSCRIPT_THREE                      => chr(179); # ³
use constant ACUTE_ACCENT                           => chr(180); # ´
use constant MICRO_SIGN                             => chr(181); # µ
use constant PILCROW_SIGN                           => chr(182); # ¶
use constant MIDDLE_DOT                             => chr(183); # ·
use constant SPACING_CEDILLA                        => chr(184); # ¸
use constant SUPERSCRIPT_ONE                        => chr(185); # ¹
use constant MASCULINE_ORDINAL_INDICATOR            => chr(186); # º
use constant RIGHT_DOUBLE_ANGLE_QUOTES              => chr(187); # »
use constant FRACTION_ONE_QUARTER                   => chr(188); # ¼
use constant FRACTION_ONE_HALF                      => chr(189); # ½
use constant FRACTION_THREE_QUARTERS                => chr(190); # ¾
use constant INVERTED_QUESTION_MARK                 => chr(191); # ¿
use constant LATIN_CAPITAL_LETTER_A_WITH_GRAVE      => chr(192); # À
use constant LATIN_CAPITAL_LETTER_A_WITH_ACUTE      => chr(193); # Á
use constant LATIN_CAPITAL_LETTER_A_WITH_CIRCUMFLEX => chr(194); # Â
use constant LATIN_CAPITAL_LETTER_A_WITH_TILDE      => chr(195); # Ã
use constant LATIN_CAPITAL_LETTER_A_WITH_DIAERESIS  => chr(196); # Ä
use constant LATIN_CAPITAL_LETTER_A_WITH_RING_ABOVE => chr(197); # Å
use constant LATIN_CAPITAL_LETTER_AE                => chr(198); # Æ
use constant LATIN_CAPITAL_LETTER_C_WITH_CEDILLA    => chr(199); # Ç
use constant LATIN_CAPITAL_LETTER_E_WITH_GRAVE      => chr(200); # È
use constant LATIN_CAPITAL_LETTER_E_WITH_ACUTE      => chr(201); # É
use constant LATIN_CAPITAL_LETTER_E_WITH_CIRCUMFLEX => chr(202); # Ê
use constant LATIN_CAPITAL_LETTER_E_WITH_DIAERESIS  => chr(203); # Ë
use constant LATIN_CAPITAL_LETTER_I_WITH_GRAVE      => chr(204); # Ì
use constant LATIN_CAPITAL_LETTER_I_WITH_ACUTE      => chr(205); # Í
use constant LATIN_CAPITAL_LETTER_I_WITH_CIRCUMFLEX => chr(206); # Î
use constant LATIN_CAPITAL_LETTER_I_WITH_DIAERESIS  => chr(207); # Ï
use constant LATIN_CAPITAL_LETTER_ETH               => chr(208); # Ð
use constant LATIN_CAPITAL_LETTER_N_WITH_TILDE      => chr(209); # Ñ
use constant LATIN_CAPITAL_LETTER_O_WITH_GRAVE      => chr(210); # Ò
use constant LATIN_CAPITAL_LETTER_O_WITH_ACUTE      => chr(211); # Ó
use constant LATIN_CAPITAL_LETTER_O_WITH_CIRCUMFLEX => chr(212); # Ô
use constant LATIN_CAPITAL_LETTER_O_WITH_TILDE      => chr(213); # Õ
use constant LATIN_CAPITAL_LETTER_O_WITH_DIAERESIS  => chr(214); # Ö
use constant MULTIPLICATION_SIGN                    => chr(215); # ×
use constant LATIN_CAPITAL_LETTER_O_WITH_SLASH      => chr(216); # Ø
use constant LATIN_CAPITAL_LETTER_U_WITH_GRAVE      => chr(217); # Ù
use constant LATIN_CAPITAL_LETTER_U_WITH_ACUTE      => chr(218); # Ú
use constant LATIN_CAPITAL_LETTER_U_WITH_CIRCUMFLEX => chr(219); # Û
use constant LATIN_CAPITAL_LETTER_U_WITH_DIAERESIS  => chr(220); # Ü
use constant LATIN_CAPITAL_LETTER_Y_WITH_ACUTE      => chr(221); # Ý
use constant LATIN_CAPITAL_LETTER_THORN             => chr(222); # Þ
use constant LATIN_SMALL_LETTER_SHARP_S             => chr(223); # ß
use constant LATIN_SMALL_LETTER_A_WITH_GRAVE        => chr(224); # à
use constant LATIN_SMALL_LETTER_A_WITH_ACUTE        => chr(225); # á
use constant LATIN_SMALL_LETTER_A_WITH_CIRCUMFLEX   => chr(226); # â
use constant LATIN_SMALL_LETTER_A_WITH_TILDE        => chr(227); # ã
use constant LATIN_SMALL_LETTER_A_WITH_DIAERESIS    => chr(228); # ä
use constant LATIN_SMALL_LETTER_A_WITH_RING_ABOVE   => chr(229); # å
use constant LATIN_SMALL_LETTER_AE                  => chr(230); # æ
use constant LATIN_SMALL_LETTER_C_WITH_CEDILLA      => chr(231); # ç
use constant LATIN_SMALL_LETTER_E_WITH_GRAVE        => chr(232); # è
use constant LATIN_SMALL_LETTER_E_WITH_ACUTE        => chr(233); # é
use constant LATIN_SMALL_LETTER_E_WITH_CIRCUMFLEX   => chr(234); # ê
use constant LATIN_SMALL_LETTER_E_WITH_DIAERESIS    => chr(235); # ë
use constant LATIN_SMALL_LETTER_I_WITH_GRAVE        => chr(236); # ì
use constant LATIN_SMALL_LETTER_I_WITH_ACUTE        => chr(237); # í
use constant LATIN_SMALL_LETTER_I_WITH_CIRCUMFLEX   => chr(238); # î
use constant LATIN_SMALL_LETTER_I_WITH_DIAERESIS    => chr(239); # ï
use constant LATIN_SMALL_LETTER_ETH                 => chr(240); # ð
use constant LATIN_SMALL_LETTER_N_WITH_TILDE        => chr(241); # ñ
use constant LATIN_SMALL_LETTER_O_WITH_GRAVE        => chr(242); # ò
use constant LATIN_SMALL_LETTER_O_WITH_ACUTE        => chr(243); # ó
use constant LATIN_SMALL_LETTER_O_WITH_CIRCUMFLEX   => chr(244); # ô
use constant LATIN_SMALL_LETTER_O_WITH_TILDE        => chr(245); # õ
use constant LATIN_SMALL_LETTER_O_WITH_DIAERESIS    => chr(246); # ö
use constant DIVISION_SIGN                          => chr(247); # ÷
use constant LATIN_SMALL_LETTER_O_WITH_SLASH        => chr(248); # ø
use constant LATIN_SMALL_LETTER_U_WITH_GRAVE        => chr(249); # ù
use constant LATIN_SMALL_LETTER_U_WITH_ACUTE        => chr(250); # ú
use constant LATIN_SMALL_LETTER_U_WITH_CIRCUMFLEX   => chr(251); # û
use constant LATIN_SMALL_LETTER_U_WITH_DIAERESIS    => chr(252); # ü
use constant LATIN_SMALL_LETTER_Y_WITH_ACUTE        => chr(253); # ý
use constant LATIN_SMALL_LETTER_THORN               => chr(254); # þ
use constant LATIN_SMALL_LETTER_Y_WITH_DIAERESIS    => chr(255); # ÿ

BEGIN
{
  our %char;

  @ASCII_CHARNAMES = qw(NUL SOH STX ETX EOT ENQ ACK BEL BS HT LF VT FF CR SO SI DLE DC1 DC2 DC3 DC4
                        NAK SYN ETB CAN EM SUB ESC FS GS RS US SINGLE_QUOTE QUOTE BACKSLASH DEL);

  my @v = (NUL, SOH, STX, ETX, EOT, ENQ, ACK, BEL, BS, HT, LF, VT, FF, CR, SO, SI, DLE,
           DC1, DC2, DC3, DC4, NAK, SYN, ETB, CAN, EM, SUB, ESC, FS, GS, RS, US,
           SINGLE_QUOTE, QUOTE, BACKSLASH, DEL,);

  @Ascii::char{@ASCII_CHARNAMES} = @v;
}

1;
