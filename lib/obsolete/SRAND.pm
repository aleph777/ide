# Math::SRAND --- DEPRECATED -*-Perl-*-

#         Copyright © 2008-2021 Tom Fontaine

# Author: Tom Fontaine
# Date:   22-May-2008

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
# Revision: 14-Apr-2011 added /dev/urandom support
#           13-Apr-2015 DEPRECATED (use Math::Random::Secure)
#
package Math::SRAND;

require 5.004;
use Carp;
use strict;

BEGIN
{
  my $x = unpack '%L*',qx(ps axww | gzip -f);
  my $y = unpack '%L*',qx(head -1 /dev/urandom);
  my $s = int 10000000*sqrt($x)*sqrt($y);

  srand($s);
}

1;
