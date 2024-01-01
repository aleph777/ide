# Text::HexString --- Stringifies a hex number in chunks of 4 bytes using a delimiter -*-Perl-*-

#         Copyright © 2012-2024 Tom Fontaine

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
#                e.g. "89ABCDEF" => "89AB CDEF"
#                e.g. "89ABCDEF" => "89AB-CDEF"
#
# Revision: 14-Jun-2023 use Modern::Perl
#

# Code:

package Text::HexString;

use Carp;
use Modern::Perl;

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

our $AUTOLOAD;

my @AREF = qw();
my @HREF = qw();

my %fields = (number    => undef,
              text      => undef,
              delimiter => ' ',
             );

my $__ME__ = join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

# BEGIN
# {
# }

# END
# {
# }

sub new
{
  my $proto = shift;
  my $class = ref($proto) || $proto;
  my $this  = {_permitted => \%fields,%fields,};

  bless $this,$class;

  @{$this}{@AREF,@HREF}= ((map { [] } @AREF),(map { {} } @HREF));

  my %parm = @_;

  @{$this}{keys %parm} = values %parm;

  return $this;
}

sub AUTOLOAD
{
  my $this = shift;
  my $type = ref($this) or croak "$this is not an object";
  my $name = $AUTOLOAD;

  $name =~ s/.*://;

  return if $name eq "DESTROY";

  croak "Can't access `$name' field in class $type" unless exists $this->{'_permitted'}->{$name};

  return @_ ? $this->{$name} = shift : $this->{$name};
}

sub configure
{
  my $this = shift;
  my %parm = @_;

  @{$this}{keys %parm} = values %parm;
}

sub get
{
  my $this = shift;
  my %parm = @_;

  my $_SELF_ = join '::',_ME_,(caller(0))[3];

  my $number    = exists $parm{number}    ? $parm{number}    : $this->{number};
  my $delimiter = exists $parm{delimiter} ? $parm{delimiter} : $this->{delimiter};

  return $this->{text} = $number if length($number) < 5;

  $this->{text} = join $delimiter,map { join '',0 x (4-length),uc reverse } reverse grep !/^$/,split /([[:xdigit:]]{4})/,reverse $number;

  return $this->{text};
}

1;
