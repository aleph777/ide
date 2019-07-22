#!/usr/bin/perl -w    # -*-Perl-*-

use Math::Random::Secure;
use List::Util qw(sum);
use File::IO;
use strict;

use constant MAX_GENERATIONS => 1000;
use constant DELTA           => 0.97;
use constant CROSSOVER       => 0.5;

local $| = 1;

my @towns = 'A' .. 'Z';
my @pairs = map { [$_,$_+1] } 0 .. $#towns-1;

my $np    = 5*@towns;
my $alpha = 0.2;

my %distance;
my %route;
my @population;

my $io = File::IO->new(chomp => 1);

$io->get(path => $ARGV[0]);

for(@{$io->contents})
{
  my ($t1,$t2,$d) = split ',';

  $distance{$t1}{$t2} = $d;
}
for(1 .. $np)
{
  my @tmp = @towns;

  my $h = {ROUTE => []};

  push @{$h->{ROUTE}},splice @tmp,int(rand @tmp),1 while @tmp;
  push @population,$h;

  $h->{COST}  = sum map { $distance{$h->{ROUTE}[$_]}{$h->{ROUTE}[$_+1]} } 0 .. $#towns-1;
  $h->{INDEX} = {map { $h->{ROUTE}[$_] => $_ } 0 .. $#{$h->{ROUTE}}};
}
@population = sort { $a->{COST} <=> $b->{COST} } @population;

print "    0: ",join(' ',map { $population[$_]{COST} } 0 .. 9)," | ",join(' ',map { $population[$_]{COST} } -10 .. -1),"\n";

for my $gen (1 .. MAX_GENERATIONS)
{
  getNextGeneration($gen);

  printf "%5d: %s | %s\n",$gen,join(' ',map { $population[$_]{COST} } 0 .. 9),join(' ',map { $population[$_]{COST} } -10 .. -1),"\n";

  last if int($population[0]{COST} - $population[-1]{COST}) == 0;
  $alpha *= DELTA;
}
print "\n";
print join(' ',map { sprintf '%2s',$_ } @{$population[-1]{ROUTE}}),"\n";
print join(' ',map { sprintf '%2d',$_ } map { $population[-1]{INDEX}{$_} } @{$population[-1]{ROUTE}}),"\n";

sub swapPair
{
  my ($route,$index,$p0,$p1,$j0,$i0,$j1,$i1) = @_;

  my ($x0,$x1) = ($index->{$j0},$index->{$j1}); # indices for overwritten towns

  if($j0 eq $i0)
  {
    $route->[$p1] = $j1;
    $index->{$j1} = $p1;

    $route->[$x1] = $i1;
    $index->{$i1} = $x1;
  }
  elsif($j1 eq $i1)
  {
    $route->[$p0] = $j0;
    $index->{$j0} = $p0;

    $route->[$x0] = $i0;
    $index->{$i0} = $x0;
  }
  elsif ($j0 eq $i1 && $j1 eq $i0)
  {
  }
  elsif($j0 eq $i1)
  {
    $route->[$p0] = $j0;
    $index->{$j0} = $p0;
    $route->[$p1] = $j1;
    $index->{$j1} = $p1;

    $route->[$x1] = $i0;
    $index->{$i0} = $x1;
  }
  elsif($j1 eq $i0)
  {
    $route->[$p0] = $j0;
    $index->{$j0} = $p0;
    $route->[$p1] = $j1;
    $index->{$j1} = $p1;

    $route->[$x0] = $i1;
    $index->{$i1} = $x0;
  }
  else
  {
    $route->[$p0] = $j0;
    $index->{$j0} = $p0;
    $route->[$p1] = $j1;
    $index->{$j1} = $p1;

    $route->[$x0] = $i0;
    $index->{$i0} = $x0;
    $route->[$x1] = $i1;
    $index->{$i1} = $x1;
  }
}

sub getNextGeneration
{
  my $gen = shift;

  my @tmp = @population;

  for my $i (1 .. $#tmp)
  {
    my $pi = $tmp[$i];
    my $j  = int(rand $i);
    my $pj = $population[$j];

    my $pairs = int(CROSSOVER*rand @towns) + 1;

    my @p = @pairs;

    for(1 .. $pairs)
    {
      my $pair = splice @p,int(rand @p),1; # indices of towns moving from j

      my ($j0,$j1) = @{$pj->{ROUTE}}[@{$pair}]; # town pair moving from j to i
      my ($i0,$i1) = @{$pi->{ROUTE}}[@{$pair}]; # town pair in i being overwritten

      next if $j0 eq $i0 && $j1 eq $i1; # nothing to swap

      swapPair($pi->{ROUTE},$pi->{INDEX},@{$pair},$j0,$i0,$j1,$i1);

      die "FAIL: @{$pi->{ROUTE}}\n" if grep { $pi->{ROUTE}[$_] eq $pi->{ROUTE}[$_+1] } 0 .. $#towns-1;
    }
    #
    # Random walk
    #
    for(1 .. int($alpha*@towns))
    {
      my $n1 = int(rand @towns);
      my $n2 = int(rand @towns);

      $n2 = int(rand @towns) while $n2 == $n1;

      my $t1 = $pi->{ROUTE}[$n1];
      my $t2 = $pi->{ROUTE}[$n2];

      ($pi->{ROUTE}[$n1],$pi->{ROUTE}[$n2]) = ($pi->{ROUTE}[$n2],$pi->{ROUTE}[$n1]);
      ($pi->{INDEX}{$t1},$pi->{INDEX}{$t2}) = ($pi->{INDEX}{$t2},$pi->{INDEX}{$t1});
    }
    $pi->{COST} = sum map { $distance{$pi->{ROUTE}[$_]}{$pi->{ROUTE}[$_+1]} } 0 .. $#towns-1;
  }
  @population = sort { $a->{COST} <=> $b->{COST} } @tmp;
}

