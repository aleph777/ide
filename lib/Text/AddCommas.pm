# Text::AddCommas --- Adds commas to input number -*-Perl-*-

#         Copyright © 2012-2019 Tom Fontaine

# Author: Tom Fontaine
# Date:   26-Sep-2012

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
# Revision: 13-Apr-2015 Added Scalar::Util::looks_like_number check
#
package Text::AddCommas;

require 5.004;
use Exporter 'import';
use Carp;
use strict;

use Scalar::Util qw(looks_like_number);

our @EXPORT_OK = qw(addCommas);

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

# BEGIN
# {
# }

# END
# {
# }

sub addCommas
{
  my $number = shift;

  my $__ME__ = (caller(0))[3];

  die $__ME__,": $number is not a number!!!\n" unless looks_like_number($number);

  return length($number) < 5 ? $number : join ',',map { scalar reverse } reverse grep /^\d/,split /(\d{3})/,reverse($number);
}

1;
