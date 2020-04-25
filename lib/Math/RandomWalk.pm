# Math::RandomWalk --- Implements a random walk object -*-Perl-*-

#         Copyright © 2017-2020 Tom Fontaine

# Author: Tom Fontaine
# Date:   14-Aug-2015

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
package Math::RandomWalk;

require 5.008;
use Carp;
use strict;

use Math::Random::Secure qw(irand);

# use constant FOO => 'BAR';

our $AUTOLOAD;

my @AREF = qw(contents);
my @HREF = qw();

my %fields = (contents => undef,

              start => undef,
              range => undef,
              delta => undef,
              min   => undef,
              max   => undef,
              count => undef,
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

  @{$this}{@AREF} = map { [] } @AREF;
  @{$this}{@HREF} = map { {} } @HREF;

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

  croak "Can't access `$name' field in class $type" unless exists $this->{_permitted}->{$name};

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

  my $__ME__ = (caller(0))[3];

  my $contents = exists $parm{contents} ? $parm{contents} : $this->{contents};
  my $start    = exists $parm{start}    ? $parm{start}    : $this->{start};
  my $range    = exists $parm{range}    ? $parm{range}    : $this->{range};
  my $delta    = exists $parm{delta}    ? $parm{delta}    : $this->{delta};
  my $min      = exists $parm{min}      ? $parm{min}      : $this->{min};
  my $max      = exists $parm{max}      ? $parm{max}      : $this->{max};
  my $count    = exists $parm{count}    ? $parm{count}    : $this->{count};

  @{$contents} = ();

  die $__ME__,": no range defined!!!\n" unless defined $range;

  $delta = 0 unless defined $delta;

  my $n = defined $start ? $start : irand($max - $min) + $min;

  for(1 .. $count)
  {
    push @{$contents},$n;

    my $x  = irand($range) - $delta;
    my $nn = $n + $x;

    if((defined $max && $nn > $max) || (defined $min && $nn < $min))
    {
      $n -= $x;
    }
    else
    {
      $n = $nn;
    }
  }
}

1;
