#!/usr/bin/perl -w    # -*-Perl-*-

#use Math::Random::Secure;
srand(8675309);
use List::Util qw(sum);
use File::IO;
use strict;

use constant MAX_GENERATIONS => 5000;
use constant DELTA           => 0.97;
use constant GAMMA           => 2.5;

local $| = 1;

my $dataFile = defined $ARGV[0] ? $ARGV[0] : 'towns1.csv';
#
# Have to do better than initial guess of tsp0
#
my @preview = qx(perl tsp0.pl $dataFile);

chomp(my $goal = $preview[-1]);

my $io = File::IO->new(chomp => 1);
#
# Get input data for town map
#
my %distance;

$io->get(path => $dataFile);

for(@{$io->contents})
{
  my ($t1,$t2,$d) = split ',';

  $distance{$t1}{$t2} = $d;
}
#
# Set up search
#
my @towns = sort keys %distance;
my $np    = 2*@towns;

my $alpha;
my $genPassed;
my $genBest;

my $best = 1000*$goal;;

my @population;

while($best > $goal)
{
  $genPassed = undef;

  initializeSearch();

  print "     0: ",join(' ',map { sprintf '%4d',$population[$_]{COST} } 0 .. 9)," | ",
    join(' ',map { sprintf '%4d',$population[$_]{COST} } -10 .. -1),"\n";

  iterateSearch();

  next if $best > $goal;

  my $r0 = $population[0]{ROUTE};
  my $i0 = $population[0]{INDEX};

  print "\n";
  print join(' ',map { sprintf '%4d',$_ } map { $i0->{$_} } @{$r0}),"\n";
  print join(' ',map { sprintf '%4s',$_ } @{$r0}),"\n";
  print join(' ','    ',map { sprintf '%4d',$distance{$r0->[$_]}{$r0->[$_+1]} } (0 .. $#towns-1,-1)),"\n";

  my $t = '    ';
  my $d = 0;

  for(0 .. $#towns-1,-1)
  {
    $d += $distance{$r0->[$_]}{$r0->[$_+1]};

    $t = join(' ',$t,sprintf '%4d',$d);
  }
  print "$t\n--vs--\n";
  print $preview[0];
  print $preview[1];
  print $preview[2],"\n";
  printf "$best/$goal = %4.2f\n",$best/$goal;
  print "Passed goal at generation: $genPassed\n";
  print "Found best solution at generation: $genBest\n";
}
sub initializeSearch
{
  $alpha = 0.2;

  @population = ();

  for(1 .. $np)
  {
    my ($route,$index) = ([],{});

    randomRoute($route,$index);

    my $h = {ROUTE => $route,INDEX => $index};

    $h->{COST} = sum map { $distance{$route->[$_]}{$route->[$_+1]} } (0 .. $#towns-1,-1);

    push @population,$h;
  }
  @population = sort { $a->{COST} <=> $b->{COST} } @population;
}

sub randomRoute
{
  my ($route,$index) = @_;

  my @tmp = @towns;

  @{$route} = (shift @tmp);

  push @{$route},splice @tmp,int(rand @tmp),1 while @tmp;

  %{$index} = map { $route->[$_] => $_ } 0 .. $#{$route};
}

sub swapTown
{
  my ($pj,$route,$index,$town) = @_;

  my $jx = $pj->{INDEX}{$town};
  my $it = $route->[$jx];
  my $ix = $index->{$town};

  $route->[$jx]   = $town; # put j's town at town's location in i's route
  $index->{$town} = $jx;

  $route->[$ix] = $it;     # move overwritten town
  $index->{$it} = $ix;

  unless($route->[0] eq $towns[0])
  {
    push @{$route},shift @{$route} until $route->[0] eq $towns[0];

    %{$index} = map { $route->[$_] => $_ } 0 .. $#{$route};
  }
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

sub iterateSearch
{
  my %bestCount;

  my $done = 0;

  $best = $population[0]{COST};

  for my $gen (1 .. MAX_GENERATIONS)
  {
    unless(exists $bestCount{$best})
    {
      $bestCount{$best} = 0;
      $genBest          = $gen;
    }
    if(++$bestCount{$best}%100 != 0)
    {
      getNextGeneration($gen);
    }
    else
    {
      doSomethingDifferent();

      $done = 1 if $bestCount{$best} == 1000;
    }
    $best = $population[0]{COST};

    my $pf = $best < $goal ? '*' : ' ';

    $genPassed = $gen if !defined $genPassed && $pf eq '*';

    $done |= int($best - $population[-1]{COST}) == 0;

    printf "$pf %4d: %s | %s\n",$gen,join(' ',map { sprintf '%4d',$population[$_]{COST} } 0 .. 9),
      join(' ',map { sprintf '%4d',$population[$_]{COST} } -10 .. -1),"\n" if $done || $gen%100 == 0;

    last if $done;

    $alpha *= DELTA;
  }
  print '  ','=' x 52,"FAIL ($goal)",'=' x 44,"\n" unless $best < $goal;
}

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
        my $diffs = grep { $pi->{ROUTE}[$_] ne $pj->{ROUTE}[$_] } 0 ..  $#towns;

        unless($diffs)
        {
          randomWalk(\@route,\%index);
          next;
        }
        my $swaps = int($diffs)/4;
        # my $pct = ($pi->{COST} - $pj->{COST})/$pj->{COST};

        # $pct = 0.25 if $pct > 0.25;

        # my $swaps = int($pct*@towns);

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

sub doSomethingDifferent
{
  for my $t (1 .. @towns)
  {
    my $route = $population[$t]{ROUTE};
    my $index = $population[$t]{INDEX};

    for(1 .. int($t/2)+1)
    {
      randomWalk($route,$index);
    }
    $population[$t]{COST} = sum map { $distance{$route->[$_]}{$route->[$_+1]} } (0 .. $#towns-1,-1);
  }
  for my $t (@towns+1 .. $#population)
  {
    my $route = $population[$t]{ROUTE};
    my $index = $population[$t]{INDEX};

    randomRoute($route,$index);

    $population[$t]{COST} = sum map { $distance{$route->[$_]}{$route->[$_+1]} } (0 .. $#towns-1,-1);
  }
  @population = sort { $a->{COST} <=> $b->{COST} } @population;
}
