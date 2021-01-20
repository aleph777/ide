# Math::Statistics --- [description] -*-Perl-*-

#         Copyright © 2008-2021 Tom Fontaine

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
# Revision:
#
package Math::Statistics;

require 5.006;
use Carp;
use strict;
use List::Util qw(sum first);

# use constant FOO => 'BAR';

our $AUTOLOAD;

my @AREF = qw(Data CorrData Histogram);
my @HREF = qw(Frequency);

my %fields = (Data               => undef,
              CorrData           => undef,
              Histogram          => undef,
              Frequency          => undef,

              LowerTrim          => 0,
              UpperTrim          => 0,

              Partitions         => 2,
              BinWidth           => 1,

              Min                => undef,
              Max                => undef,
              MinIndex           => undef,
              MaxIndex           => undef,

              Sum                => undef,
              Range              => undef,
              Mean               => undef,
              Median             => undef,
              Variance           => undef,
              StandardDeviation  => undef,
              Correlation        => undef,
              TrimmedMean        => undef,
              GeometricMean      => undef,
              HarmonicMean       => undef,
              Mode               => undef,
              MinBin             => undef,
              MaxBin             => undef,
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
  my $self = shift;
  my %parm = @_;

  my $data = exists $parm{Data}       ? $parm{Data}       : $self->{Data};
  my $freq = exists $parm{Frequency}  ? $parm{Frequency}  : $self->{Frequency};
  my $hist = exists $parm{Histogram}  ? $parm{Histogram}  : $self->{Histogram};
  my $corr = exists $parm{CorrData}   ? $parm{CorrData}   : $self->{CorrData};
  my $utrm = exists $parm{UpperTrim}  ? $parm{UpperTrim}  : $self->{UpperTrim};
  my $ltrm = exists $parm{LowerTrim}  ? $parm{LowerTrim}  : $self->{LowerTrim};
  my $part = exists $parm{Partitions} ? $parm{Partitions} : $self->{Partitions};

  unless(@{$data} > 1)
  {
    if(@{$data})
    {
      print STDERR "Math::Statistics::get: not enough data\n";
    }
    else
    {
      print STDERR "Math::Statistics::get: empty data set\n";
    }
    return;
  }
  my @data = sort { $a <=> $b } @{$data};

  $self->{Min} = $data[0];
  $self->{Max} = $data[-1];

  $self->{MinIndex} = first { $data->[$_] == $self->{Min} } 0 .. $#data;
  $self->{MaxIndex} = first { $data->[$_] == $self->{Max} } 0 .. $#data;

  $self->{Sum}    = sum @data;
  $self->{Range}  = $self->{Max} - $self->{Min};
  $self->{Mean}   = $self->{Sum}/@data;
  $self->{Median} = ($self->{Max} + $self->{Min})/2;

  $self->{Variance}          = sum([map { ($_ - $self->{Mean})**2 } @{$data}])/(@{$data} - 1);
  $self->{StandardDeviation} = sqrt($self->{Variance});

  if(@{$corr})
  {
    my $meanx = $self->{Mean};
    my $meany = average($corr);

    my @x = map { $_ - $meanx } @{$data};
    my @y = map { $_ - $meany } @{$corr};

    my $xx = sum([map { $_**2 } @x]);
    my $yy = sum([map { $_**2 } @y]);
    my $xy = sum([map { $x[$_]*$y[$_] } (0 .. $#x)]);

    $self->{Correlation} = $xy/sqrt($xx*$yy);
  }
  if($utrm || $ltrm)
  {
    $self->{TrimmedMean} = average([@data[int($ltrm*@data) .. $#data-int($utrm*@data)]]);
  }
  unless(grep { $_ == 0 } @{$data})
  {
    my $gm  = 1;
    my $exp = 1/@{$data};

    for(@{$data}) { $gm *= $_**$exp; }

    $self->{GeometricMean} = $gm;

    my $hs = sum([map { 1/$_ } @{$data}]);

    $self->{HarmonicMean} = @{$data}/$hs;
  }
  my %count;

  for(@{$data}) { $count{$_} = exists $count{$_} ? $count{$_} + 1 : 1; }

  $self->{Mode} = (sort { $count{$b} <=> $count{$a} } keys %count)[0];

  my $interval = ($self->{Max} - $self->{Min})/$part;
  my $iterate  = $self->{Min};

  $freq->{$iterate} = 0 while ($iterate += $interval) <= $self->{Max};

  my @bkeys = sort { $a <=> $b } keys %{$freq};

  for(@{$data})
  {
    foreach my $key (@bkeys)
    {
      if($_ <= $key)
      {
        $freq->{$key}++;
        last;
      }
    }
  }
  my $minbin = exists $parm{MinBin}    ? $parm{MinBin}    : $self->{MinBin};
  my $maxbin = exists $parm{MaxBin}    ? $parm{MaxBin}    : $self->{MaxBin};
  my $width  = exists $parm{BinWidth}  ? $parm{BinWidth}  : $self->{BinWidth};

  $minbin = $self->{Min} if not defined $minbin || $self->{Min} < $minbin;
  $maxbin = $self->{Max} if not defined $maxbin || $self->{Max} > $maxbin;

  $minbin = int($minbin/$width)*$width;
  $maxbin = int($maxbin/$width)*$width;

  $self->{MinBin} = $minbin;
  $self->{MaxBin} = $maxbin;

  @{$hist} = (0) x (1+int(($maxbin - $minbin)/$width));

  #  for(@{$data}) { $hist->[int(($_ - $minbin)/$width)]++; }
  $hist->[int(($_ - $minbin)/$width)]++ for @{$data};
}

1;
