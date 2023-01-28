# Math::Statistics --- Statistics object -*-Perl-*-

#         Copyright © 2008-2023 Tom Fontaine

# Author: Tom Fontaine
# Date:   07-Feb-2008

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
# Revision: 18-Feb-2021 Major overhaul
#
package Math::Stats;

use Carp;
use strict;
use v5.10;

use List::Util qw(any first max product sum);

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

our $AUTOLOAD;

my @AREF = qw(data data1 histogram);
my @HREF = qw();

my %fields = (data  => undef,
              data1 => undef,

              maximum => undef,
              minimum => undef,
              range   => undef,

              mean     => undef,
              stdev    => undef,
              variance => undef,

              harmonicMean  => undef,
              geometricMean => undef,

              trim        => undef,
              trimmedMean => undef,

              correlation => undef,

              histogram  => undef,
              binMinimum => undef,
              binMaximum => undef,
              binWidth   => undef,
              mode       => undef,
             );

my $threshold = 0.000001;

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

  my $data      = exists $parm{data}       ? $parm{data}       : $this->{data};
  my $data1     = exists $parm{data1}      ? $parm{data1}      : $this->{data1};
  my $trim      = exists $parm{trim}       ? $parm{trim}       : $this->{trim};
  my $histogram = exists $parm{histogram}  ? $parm{histogram}  : $this->{histogram};

  my @data = sort { $a <=> $b } @{$data};

  die $__ME__,': not enough data!!!',"\n" unless @data >= 2;

  $this->{minimum} = $data[0];
  $this->{maximum} = $data[-1];

  $this->{minIndex} = first { equals(@{$data}[$_],$this->{minimum}) } 0 .. $#data;
  $this->{maxIndex} = first { equals(@{$data}[$_],$this->{maximum}) } 0 .. $#data;

  $this->{sum}    = sum @data;
  $this->{range}  = $this->{maximum} - $this->{minimum};
  $this->{mean}   = $this->{sum}/@data;

  $this->{variance} = sum([map { ($_ - $this->{mean})**2 } @data])/(@data - 1);
  $this->{stdev}    = sqrt($this->{variance});

  my $mid = @data/2;

  $this->{median} = @data % 2 == 0 ? ($data[$mid-1] + $data[$mid])/2 : $data[$mid];

  if(defined $trim && $trim > 0 && $trim < 0.45)
  {
    my $trmin = int @data*$trim;
    my $trmax = int @data*(1 - $trim);
    my $trrng = $trmax - $trmin;

    die $__ME__,': not enough data for trimmed mean!!!',"\n" if $trrng == 0;

    $this->{trimmedMean} = sum(@data[$trmin .. $trmax])/$trrng;
  }
  if(any { $_ == 0 } @data)
  {
    $this->{geometricMean} = 0;
    $this->{harmonicMean}  = 0;
  }
  else
  {
    $this->{geometricMean} = product(@data) ** (1/@data);
    $this->{harmonicMean}  = @data/sum(map { 1/$_ } @data);
  }
  if(defined $data1 && @{$data1} == @{$data})
  {
    my $meanx = $this->{mean};
    my $meany = sum(@{$data1})/@data;

    my @x = map { $_ - $meanx } @{$data};
    my @y = map { $_ - $meany } @{$data1};

    my $xx = sum([map { $_**2 } @x]);
    my $yy = sum([map { $_**2 } @y]);
    my $xy = sum([map { $x[$_]*$y[$_] } (0 .. $#x)]);

    $this->{correlation} = $xy/sqrt($xx*$yy);
  }
  my $binMinimum = exists $parm{binMinimum} ? $parm{binMinimum} : $this->{binMinimum};
  my $binMaximum = exists $parm{binMaximum} ? $parm{binMaximum} : $this->{binMaximum};
  my $binWidth   = exists $parm{binWidth}   ? $parm{binWidth}   : $this->{binWidth};

  $binMinimum = int($this->{minimum}) if not defined $binMinimum || $this->{minimum} < $binMinimum;
  $binMaximum = int($this->{maximum}) if not defined $binMaximum || $this->{maximum} > $binMaximum;
  $binWidth   = 1 if not defined $binWidth || $binWidth == 0;

  $this->{binMinimum} = $binMinimum;
  $this->{binMaximum} = $binMaximum;
  $this->{binWidth}   = $binWidth;

  my $binCount = int(($binMaximum - $binMinimum)/$binWidth) + 1;

  @{$histogram} = (0) x $binCount;

  $histogram->[int(($_ - $binMinimum)/$binWidth)]++ for @data;

  $this->{mode} = max(@{$histogram})*$binWidth + $binMinimum;
}

sub equals
{
  my ($x1,$x2) = @_;

  return abs($x1 - $x2) < $threshold;
}

1;
