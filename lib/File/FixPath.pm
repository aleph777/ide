# File::FixPath --- Fixes problems in pathnames (e.g. spaces) by backslashing -*-Perl-*-

#         Copyright © 2015-2021 Tom Fontaine

# Author: Tom Fontaine
# Date:   31-Mar-2015

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
# Revision:
#
package File::FixPath;

require 5.008;
use Exporter 'import';
use Carp;
use strict;

our @EXPORT_OK = qw(fixpath);

use Ascii qw(BACKSLASH);

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

use constant BACKSLASH_SPACE => join '',BACKSLASH,' ';

my $bs2 = BACKSLASH x 2;

# BEGIN
# {
# }

# END
# {
# }

sub fixpath
{
  my $path = shift @_;
  my $refp = ref($path);

  if($refp eq 'ARRAY')
  {
    s/(?<!$bs2) /BACKSLASH_SPACE/eg for @{$path};
  }
  elsif($refp eq 'SCALAR')
  {
    ${$path} =~ s/(?<!$bs2) /BACKSLASH_SPACE/eg;
  }
  else
  {
    my $__ME__ = (caller(0))[3];

    die $__ME__,": invalid reference type [$refp]!!!\n",;
  }
}

1;
