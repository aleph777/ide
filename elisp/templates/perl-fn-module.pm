# <<<PACKAGE>>> --- [description] -*-Perl-*-

#         Copyright © <<<YEAR>>> <<<AUTHOR>>>

# Author: <<<AUTHOR>>>
# Date:   <<<DATE>>>

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

# Code:

package <<<PACKAGE>>>;

use Exporter 'import';
use Carp;
use Modern::Perl;

our @EXPORT_OK = qw();

# use Foo::Bar;

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

# BEGIN
# {
# }

# END
# {
# }

sub Foo
{
  my ($bar) = @_;

  my $_SELF_ = join '::',_ME_,(caller(0))[3];

  return $bar;
}

1;
