# Math::Genetic --- Differential Evolution Genetic Algorithm -*-Perl-*-

#         Copyright © 2000-2021 Tom Fontaine

# Author: Tom Fontaine
# Date:   02-Feb-2000

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
# Revision: 14-May-2008 Moved to Math::Genetic
#                       Revised random seed
#                       Removed reseed
#                       use strict
#           22-May-2008 use Math::SRAND
#           13-Apr-2015 use Math::Random::Secure
#
package Math::Genetic;

require 5.006;
use Carp;
use strict;
use Math::Random::Secure qw(irand rand);
use List::Util qw(sum);

# use constant FOO => 'BAR';

our $AUTOLOAD;

my @AREF = qw(Vectors ResumeVectors);
my @HREF = qw();

my %fields = (Vectors             => undef,
              ResumeVectors       => undef,
              Keys                => undef,
              Specification       => undef,
              CostFunction        => undef,
              CostFormat          => '%7.4f',
              NP_Scale            => 5,
              CrossoverRatio      => 0.9,
              WeightDifferential  => 0.7,
              MutationRate        => undef,
              MinDiff             => 0.00005,
              MaxGenerations      => undef,
              Print               => 1,
              Dump                => undef,
              HUP                 => 0,
              USR1                => 0,
              USR2                => 0,
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
sub solve
{
  my $this = shift;
  my %parm = @_;

  my $vectors = exists $parm{Vectors}             ? $parm{Vectors}             : $this->{Vectors};
  my $resume  = exists $parm{ResumeVectors}       ? $parm{ResumeVectors}       : $this->{ResumeVectors};
  my $vkeys   = exists $parm{Keys}                ? $parm{Keys}                : $this->{Keys};
  my $data    = exists $parm{Data}                ? $parm{Data}                : $this->{Data};
  my $spec    = exists $parm{Specification}       ? $parm{Specification}       : $this->{Specification};
  my $costfun = exists $parm{CostFunction}        ? $parm{CostFunction}        : $this->{CostFunction};
  my $costfmt = exists $parm{CostFormat}          ? $parm{CostFormat}          : $this->{CostFormat};
  my $npscale = exists $parm{NP_Scale}            ? $parm{NP_Scale}            : $this->{NP_Scale};
  my $cr      = exists $parm{CrossoverRatio}      ? $parm{CrossoverRatio}      : $this->{CrossoverRatio};
  my $wd      = exists $parm{WeightDifferential}  ? $parm{WeightDifferential}  : $this->{WeightDifferential};
  my $mu      = exists $parm{MutationRate}        ? $parm{MutationRate}        : $this->{MutationRate};
  my $diff    = exists $parm{MinDiff}             ? $parm{MinDiff}             : $this->{MinDiff};
  my $maxgen  = exists $parm{MaxGenerations}      ? $parm{MaxGenerations}      : $this->{MaxGenerations};
  my $prnt    = exists $parm{Print}               ? $parm{Print}               : $this->{Print};
  my $dump    = exists $parm{Dump}                ? $parm{Dump}                : $this->{Dump};

  $mu = @{$vkeys}**-2 unless defined $mu;

  my @trial_vectors;

  my $i = 0;

  my $fmt = join ' ','%3d.',(map { $spec->{$_}{FMT} } @{$vkeys}),'--',($costfmt) x 10;

  for(@{$vkeys}) { $spec->{$_}{RNG} = $spec->{$_}{MAX} - $spec->{$_}{MIN}; }

  initializeVectors($vectors,$vkeys,$spec,$npscale*@{$vkeys},$resume);
  &{$costfun}($vectors);

  $dump->Put(Contents => [$i,map { $_->{TEXT} } sort { $a->{COST} <=> $b->{COST} } @{$vectors}]) if defined $dump;

  while(1)
  {
    if($this->{USR1}) # currently unused -- was reseed
    {
      $this->{USR1} = 0;
    }
    makeTrialVectors($vectors,$vkeys,$spec,$cr,$wd,$mu,\@trial_vectors);
    &{$costfun}(\@trial_vectors,$data);

    @{$vectors} = sort { $a->{COST} <=> $b->{COST} }
      map { $vectors->[$_]{COST} < $trial_vectors[$_]{COST} ? $vectors->[$_] : $trial_vectors[$_] } (0 .. $#{$vectors});

    printf "$fmt\n",$i,@{$vectors->[0]}{@{$vkeys}},map { $vectors->[$_]{COST} } (0 .. 4,-5 .. -1) if defined $prnt && $prnt;

    $dump->Put(Contents => [$i,map { $_->{TEXT} } @{$vectors}]) if defined $dump;

    if($this->{USR2})
    {
      irradiate($vectors,$vkeys,$spec);

      $this->{USR2} = 0;
    }
    last if ($vectors->[-1]{COST} - $vectors->[0]{COST}) <= $diff || (defined $maxgen && $i == $maxgen) || $this->{HUP};
  }
}
sub initializeVectors
{
  my ($vectors,$vkeys,$spec,$np,$resume) = @_;

  my $sum;

  if(defined $resume)
  {
    push @{$vectors},@{$resume};

    $np -= @{$resume};
  }
  for my $i (1 .. $np)
  {
    my $h = {map { $_ => rand($spec->{$_}{RNG}) + $spec->{$_}{MIN} } @{$vkeys}};

    for(@{$vkeys})
    {
      if(defined $spec->{$_}{INT} && $spec->{$_}{INT})
      {
        $h->{$_} = int($h->{$_} + 0.5);
      }
      elsif(defined $spec->{$_}{PRC} && $spec->{$_}{PRC})
      {
        $h->{$_} = int($spec->{$_}{PRC}*$h->{$_} + 0.5)/$spec->{$_}{PRC};
      }
    }
    if(defined $spec->{NORMALIZE} && $spec->{NORMALIZE})
    {
      $sum = sum(@{$h}{@{$vkeys}});

      for(@{$vkeys}) { $h->{$_} /= $sum; }
    }
    push @{$vectors},$h;
  }
}
sub makeTrialVectors
{
  my ($vectors,$vkeys,$spec,$crossover_ratio,$weight_differential,$mutation,$trial_vectors) = @_;

  my ($a,$b,$c);

  my $key;
  my $sum;

  my $r;

  @{$trial_vectors} = ();

  for my $i (0 .. $#{$vectors})
  {
    ($a,$b,$c) = ($i) x 3;

    $a = irand(@{$vectors}) until $a ne $i;
    $b = irand(@{$vectors}) until $b ne $i && $b ne $a;
    $c = irand(@{$vectors}) until $c ne $i && $c ne $b && $c ne $a;

    $key = $vkeys->[irand(@{$vkeys})];

    my $h = {};

    for(@{$vkeys})
    {
      if($mutation > ($r = rand) || $crossover_ratio > $r || $_ eq $key)
      {
        $h->{$_} = $mutation > $r ? rand($spec->{$_}{RNG}) + $spec->{$_}{MIN} :
                                    $vectors->[$a]{$_} + $weight_differential*($vectors->[$b]{$_} - $vectors->[$c]{$_});

        if(defined $spec->{$_}{INT} && $spec->{$_}{INT})
        {
          $h->{$_} = int($h->{$_} + 0.5);
        }
        elsif(defined $spec->{$_}{PRC} && $spec->{$_}{PRC})
        {
          $h->{$_} = int($spec->{$_}{PRC}*$h->{$_} + 0.5)/$spec->{$_}{PRC};
        }
        if($h->{$_} > $spec->{$_}{MAX})
        {
          $h->{$_} -= abs($spec->{$_}{RNG});
        }
        elsif($h->{$_} < $spec->{$_}{MIN})
        {
          $h->{$_} += abs($spec->{$_}{RNG});
        }
      }
      else
      {
        $h->{$_} = $vectors->[$i]{$_};
      }
    }
    if(defined $spec->{NORMALIZE} && $spec->{NORMALIZE})
    {
      $sum = sum(@{$h}{@{$vkeys}});

      for(@{$vkeys}) { $h->{$_} /= $sum; }
    }
    push @{$trial_vectors},$h;
  }
}
sub irradiate
{
  my ($vectors,$vkeys,$spec) = @_;

  my $key;
  my $sum;

  for(1 .. $#{$vectors})
  {
    $key = $vkeys->[irand(@{$vkeys})];

    $_->{$key} = rand($spec->{$key}{RNG}) + $spec->{$key}{MIN};

    if(defined $spec->{$key}{INT} && $spec->{$key}{INT})
    {
      $_->{$key} = int($_->{$key} + 0.5);
    }
    elsif(defined $spec->{$key}{PRC} && $spec->{$key}{PRC})
    {
      $_->{$key} = int($spec->{$key}{PRC}*$_->{$key} + 0.5)/$spec->{$key}{PRC};
    }
    if(defined $spec->{NORMALIZE} && $spec->{NORMALIZE})
    {
      $sum = sum(@{$_}{@{$vkeys}});

      foreach $key (@{$vkeys}) { $_->{$key} /= $sum; }
    }
  }
}

1;
