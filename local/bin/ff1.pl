#!/usr/bin/perl -w    # -*-Perl-*-

#use Math::Random::Secure;
use List::Util qw(sum);
use strict;

use constant MAX_GENERATIONS => 1000;
use constant DELTA           => 0.97;

srand(8675309);

local $| = 1;

my @towns = 'A' .. 'Z';
my @pairs = map { [$_,$_+1] } 0 .. $#towns-1;

my $np    = 5*@towns;
my $alpha = 0.2;

my %distance;
my %route;
my @population;

for my $i (0 .. $#towns)
{
  for my $j ($i .. $#towns)
  {
    next if $i == $j;

    $distance{$towns[$i]}{$towns[$j]} = $distance{$towns[$j]}{$towns[$i]} = int(rand 250) + 1;
  }
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

#  # print STDERR "@{$population[-1]{ROUTE}}\n";
#  # print STDERR join(' ',map { $population[-1]{INDEX}{$_} } @{$population[-1]{ROUTE}}),"\n";

  printf "%5d: %s | %s\n",$gen,join(' ',map { $population[$_]{COST} } 0 .. 9),join(' ',map { $population[$_]{COST} } -10 .. -1),"\n";

  last if int($population[0]{COST} - $population[-1]{COST}) == 0;
  $alpha *= DELTA;
}
print "\n";
print join(' ',map { sprintf '%2s',$_ } @{$population[-1]{ROUTE}}),"\n";
print join(' ',map { sprintf '%2d',$_ } map { $population[-1]{INDEX}{$_} } @{$population[-1]{ROUTE}}),"\n";

sub getNextGeneration
{
  my $gen = shift;

  my @tmp = @population;

  for my $i (1 .. $#tmp)
  {
    my $pi = $tmp[$i];

    my $cost  = $pi->{COST};
    my @route = @{$pi->{ROUTE}};
    my %index = map { $pi->{INDEX}{$_} } @route;

    for my $j (0 .. $i-1)
    {
      my $pj = $population[$j];

      if($pj->{COST} < $pi->{COST})
      {
        my $pairs = int(rand 5) + 1;

        my @p = @pairs;

        for(1 .. $pairs)
        {
          my $pair = splice @p,int(rand @p),1; # indices of towns moving from j

          my ($j0,$j1) = @{$pj->{ROUTE}}[@{$pair}]; # town pair moving from j to i
          my ($i0,$i1) = @{$pi->{ROUTE}}[@{$pair}]; # town pair in i being overwritten

          next if $j0 eq $i0 && $j1 eq $i1; # nothing to swap

          swapPair($pi->{ROUTE},$pi->{INDEX},@{$pair},$j0,$i0,$j1,$i1);
          # print STDERR '+' x 80,"\n";

          die "FAIL: @{$pi->{ROUTE}}\n" if grep { $pi->{ROUTE}[$_] eq $pi->{ROUTE}[$_+1] } 0 .. $#towns-1;
        }
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
  }
  @population = sort { $a->{COST} <=> $b->{COST} } @tmp;
}

sub swapPair
{
  my ($route,$index,$p0,$p1,$j0,$i0,$j1,$i1) = @_;

  my ($x0,$x1) = ($index->{$j0},$index->{$j1}); # indices for overwritten towns
  #
  # Move route pair from j to i
  #
  # $route->[$p0] = $j0;
  # $index->{$j0} = $p0;
  # $route->[$p1] = $j1;
  # $index->{$j1} = $p1;

  if($j0 eq $i0)
  {
    # print STDERR "=====> j0 eq i0\n";
    # print STDERR "$j0 => $p0 ($i0 => $x0)\n";
    # print STDERR "$j1 => $p1 ($i1 => $x1)\n";
    # print STDERR join(' ',map { sprintf '%2s',$_ } @{$route}),"\n";
    # print STDERR join(' ',map { sprintf '%2d',$_ } @{$index}{@{$route}}),"\n\n";
    $route->[$p1] = $j1;
    $index->{$j1} = $p1;

    $route->[$x1] = $i1;
    $index->{$i1} = $x1;
    # print STDERR join(' ',map { sprintf '%2s',$_ } @{$route}),"\n";
    # print STDERR join(' ',map { sprintf '%2d',$_ } @{$index}{@{$route}}),"\n";
  }
  elsif($j1 eq $i1)
  {
    # print STDERR "=====> j1 eq i1\n";
    # print STDERR "$j0 => $p0 ($i0 => $x0)\n";
    # print STDERR "$j1 => $p1 ($i1 => $x1)\n";
    # print STDERR join(' ',map { sprintf '%2s',$_ } @{$route}),"\n";
    # print STDERR join(' ',map { sprintf '%2d',$_ } @{$index}{@{$route}}),"\n\n";
    $route->[$p0] = $j0;
    $index->{$j0} = $p0;

    $route->[$x0] = $i0;
    $index->{$i0} = $x0;
    # print STDERR join(' ',map { sprintf '%2s',$_ } @{$route}),"\n";
    # print STDERR join(' ',map { sprintf '%2d',$_ } @{$index}{@{$route}}),"\n";
  }
  elsif ($j0 eq $i1 && $j1 eq $i0)
  {
    $route->[$p0] = $j0;
    $index->{$j0} = $p0;
    $route->[$p1] = $j1;
    $index->{$j1} = $p1;
  }
  elsif($j0 eq $i1)
  {
    # print STDERR "=====> j0 eq i1\n";
    # print STDERR "$j0 => $p0 ($i0 => $x0)\n";
    # print STDERR "$j1 => $p1 ($i1 => $x1)\n";
    # print STDERR join(' ',map { sprintf '%2s',$_ } @{$route}),"\n";
    # print STDERR join(' ',map { sprintf '%2d',$_ } @{$index}{@{$route}}),"\n\n";

    $route->[$p0] = $j0;
    $index->{$j0} = $p0;
    $route->[$p1] = $j1;
    $index->{$j1} = $p1;

    $route->[$x1] = $i0;
    $index->{$i0} = $x1;
    # print STDERR join(' ',map { sprintf '%2s',$_ } @{$route}),"\n";
    # print STDERR join(' ',map { sprintf '%2d',$_ } @{$index}{@{$route}}),"\n";
  }
  elsif($j1 eq $i0)
  {
    # print STDERR "=====> j1 eq i0\n";
    # print STDERR "$j0 => $p0 ($i0 => $x0)\n";
    # print STDERR "$j1 => $p1 ($i1 => $x1)\n";
    # print STDERR join(' ',map { sprintf '%2s',$_ } @{$route}),"\n";
    # print STDERR join(' ',map { sprintf '%2d',$_ } @{$index}{@{$route}}),"\n\n";

    $route->[$p0] = $j0;
    $index->{$j0} = $p0;
    $route->[$p1] = $j1;
    $index->{$j1} = $p1;

    $route->[$x0] = $i1;
    $index->{$i1} = $x0;
    # print STDERR join(' ',map { sprintf '%2s',$_ } @{$route}),"\n";
    # print STDERR join(' ',map { sprintf '%2d',$_ } @{$index}{@{$route}}),"\n";
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
  # print STDERR '-' x 80,"\n";
}

