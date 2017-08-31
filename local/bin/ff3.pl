#!/usr/bin/perl -w    # -*-Perl-*-

use Math::Random::Secure;
use List::Util qw(sum);
use File::IO;
use strict;

use constant MAX_GENERATIONS => 5000;
use constant DELTA           => 0.97;
use constant GAMMA           => 2.5;

local $| = 1;

my @towns = 'A' .. 'Z';
my @pairs = map { [$_,$_+1] } 0 .. $#towns-1;

my $np    = 2*@towns;
my $alpha = 0.2;

my %distance;
my %route;
my %best;
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
  push @{$h->{ROUTE}},shift @{$h->{ROUTE}} until $h->{ROUTE}[0] eq $towns[0];

  push @population,$h;

  $h->{COST}  = sum map { $distance{$h->{ROUTE}[$_]}{$h->{ROUTE}[$_+1]} } (0 .. $#towns-1,-1);
  $h->{INDEX} = {map { $h->{ROUTE}[$_] => $_ } 0 .. $#{$h->{ROUTE}}};
}
@population = sort { $a->{COST} <=> $b->{COST} } @population;

print "    0: ",join(' ',map { sprintf '%4d',$population[$_]{COST} } 0 .. 9)," | ",
  join(' ',map { sprintf '%4d',$population[$_]{COST} } -10 .. -1),"\n";

for my $gen (1 .. MAX_GENERATIONS)
{
  $best{$population[0]{COST}} = 0 unless exists $best{$population[0]{COST}};

  if(++$best{$population[0]{COST}}%100 != 0)
  {
    getNextGeneration($gen);
  }
  else
  {
    doSomethingDifferent();
  }
  printf "%5d: %s | %s\n",$gen,join(' ',map { sprintf '%4d',$population[$_]{COST} } 0 .. 9),
    join(' ',map { sprintf '%4d',$population[$_]{COST} } -10 .. -1),"\n";

  last if int($population[0]{COST} - $population[-1]{COST}) == 0;
  $alpha *= DELTA;
}
print "\n";
print join(' ',map { sprintf '%2s',$_ } @{$population[-1]{ROUTE}}),"\n";
print join(' ',map { sprintf '%2d',$_ } map { $population[-1]{INDEX}{$_} } @{$population[-1]{ROUTE}}),"\n";

sub getNextGeneration
{
  my $gen = shift;

  for my $i (1 .. $#population)
  {
    my $pi = $population[$i];

    my @route = @{$pi->{ROUTE}};
    my %index = %{$pi->{INDEX}};

    for my $j (0 .. $i-1)
    {
      my $pj = $population[$j];
      my $r2 = sum map { ($pi->{INDEX}{$_} - $pj->{INDEX}{$_})**2 } @towns;
      my $ep = 1/(1+GAMMA*$r2);

      my $attr_i = -$pi->{COST};
      my $attr_j = -$pj->{COST}*$ep;

      if($attr_j > $attr_i)
      {
        my $pct = ($pi->{COST} - $pj->{COST})/$pj->{COST};

        $pct = 0.5 if $pct > 0.5;

        my $swaps = int($pct*@towns);

        $swaps = 1 unless $swaps;

        my @t = @towns;

        for(1 .. $swaps)
        {
          my $town = splice @t,int(rand @t),1;

          swapTown($pj,\@route,\%index,$town);
        }
      }
      else
      {
        randomWalk(\@route,\%index);
      }
    }
    $pi->{COST}     = sum map { $distance{$route[$_]}{$route[$_+1]} } (0 .. $#towns-1,-1);
    @{$pi->{ROUTE}} = @route;
    %{$pi->{INDEX}} = %index;
  }
  @population = sort { $a->{COST} <=> $b->{COST} } @population;
}

sub swapTown
{
  my ($pj,$route,$index,$town) = @_;

  my $jx = $pj->{INDEX}{$town};
  my $it = $route->[$jx];
  my $ix = $index->{$town};

  # print STDERR "$town($jx) -- $it($ix)\n";
  # print STDERR join(' ',map { sprintf '%2s',$_ } @{$route}),"\n";
  # print STDERR join(' ',map { sprintf '%2d',$_ } @{$index}{@{$route}}),"\n\n";
  $route->[$jx]   = $town; # put j's town at town's location in i's route
  $index->{$town} = $jx;

  $route->[$ix] = $it;     # move overwritten town
  $index->{$it} = $ix;
  # print STDERR join(' ',map { sprintf '%2s',$_ } @{$route}),"\n";
  # print STDERR join(' ',map { sprintf '%2d',$_ } @{$index}{@{$route}}),"\n";exit;

  unless($route->[0] eq $towns[0])
  {
    push @{$route},shift @{$route} until $route->[0] eq $towns[0];

    %{$index} = map { $route->[$_] => $_ } 0 .. $#{$route};
  }
}

sub randomRoute
{
  my ($route,$index) = @_;

  my @tmp = @towns;

  @{$route} = ();

  push @{$route},splice @tmp,int(rand @tmp),1 while @tmp;
  push @{$route},shift @{$route} until $route->[0] eq $towns[0];

  %{$index} = map { $route->[$_] => $_ } 0 .. $#{$route};
}

sub randomWalk
{
  my ($route,$index) = @_;

  my $town1 = int(rand @towns);
  my $town2 = int(rand @towns);

  $town2 = int(rand @towns) while $town1 == $town2;

  ($route->[$town1],$route->[$town2]) = ($route->[$town2],$route->[$town1]);

  ($index->{$route->[$town1]},$index->{$route->[$town2]}) = ($index->{$route->[$town2]},$index->{$route->[$town1]});

  unless($route->[0] eq $towns[0])
  {
    push @{$route},shift @{$route} until $route->[0] eq $towns[0];

    %{$index} = map { $route->[$_] => $_ } 0 .. $#{$route};
  }
}

sub doSomethingDifferent
{
  print '-' x 108,"\n";

  my $p0 = $population[0];

  my ($A,@route) = @{$p0->{ROUTE}};

  for my $t (1 .. @towns)
  {
    push @route,shift @route;

    @{$population[$t]{ROUTE}} = ($A,@route);
    %{$population[$t]{INDEX}} = ($A => 0,map { $route[$_-1] => $_ } 1 .. @route);

    randomWalk($population[$t]{ROUTE},$population[$t]{INDEX});
    randomWalk($population[$t]{ROUTE},$population[$t]{INDEX});

    $population[$t]{COST} = sum map { $distance{$population[$t]{ROUTE}[$_]}{$population[$t]{ROUTE}[$_+1]} } (0 .. $#towns-1,-1);
  }
  for my $t (@towns+1 .. $#population)
  {
    randomRoute($population[$t]{ROUTE},$population[$t]{INDEX});

    $population[$t]{COST} = sum map { $distance{$population[$t]{ROUTE}[$_]}{$population[$t]{ROUTE}[$_+1]} } (0 .. $#towns-1,-1);
  }
  @population = sort { $a->{COST} <=> $b->{COST} } @population;
}
