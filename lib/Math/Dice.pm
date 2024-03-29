# Math::Dice --- Throws N dice of X number of sides -*-Perl-*-

#         Copyright © 2008-2024 Tom Fontaine

# Author: Tom Fontaine
# Date:   18-May-2008

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
# Revision: 20-Apr-2015 use Math::Random::Secure
#           12-Feb-2021 added getDistribution
#           14-Jun-2023 use Modern::Perl
#

# Code:

package Math::Dice;

require Exporter;
use Carp;
use Modern::Perl;

use List::Util qw(sum);
use Math::Random::Secure qw(irand);
use v5.10;

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

our $AUTOLOAD;

my @AREF = qw(contents dice);
my @HREF = qw();

my %fields = (N     => 1,
              sides => 6,

              contents => undef,
              dice     => undef,
             );

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

  my $n     = exists $parm{N}     ? $parm{N}     : $this->{N};
  my $sides = exists $parm{sides} ? $parm{sides} : $this->{sides};

  die "$_SELF_: N ($n) must be positive!!!\n"           if $n     < 1;
  die "$_SELF_: sides ($sides) must be at least 2!!!\n" if $sides < 2;

  #my @sides = (1 .. $sides);

  return sum(map { 1 + irand $sides } (1 .. $n));
}


sub getDistribution
{
  my $this = shift;
  my %parm = @_;

  my $__ME__ = (caller(0))[3];

  my $contents = exists $parm{contents} ? $parm{contents} : $this->{contents};
  my $dice     = exists $parm{dice}     ? $parm{dice}     : $this->{dice};
  my $n        = exists $parm{N}        ? $parm{N}        : $this->{N};

  # 'dice' should be 'die' but oh well

  my @tmp = @{$dice};

  for (1 .. $n-1)
  {
    for(0 .. $#tmp)
    {
      my $d = shift @tmp;

      push @tmp,map { join ',',$d,$_ } @{$dice};
    }
  }
  @{$contents} = map { sum(split ',') } @tmp;
}

1;
