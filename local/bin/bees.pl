#!/usr/bin/perl -w    # -*-Perl-*-

#use Math::Random::Secure;
srand(8675309);
use List::Util qw(sum);
use File::IO;
use strict;

use constant MAX_GENERATIONS => 99999;
use constant N               => 10;
use constant RANDOM_BEES     => 32;

local $| = 1;

my %distance;
my %distribution;

my $dataFile = defined $ARGV[0] ? $ARGV[0] : 'towns1.csv';
#
# Have to do better than initial guess of tsp0
#
my @preview = qx(perl tsp0.pl $dataFile);

chomp(my $goal = $preview[-1]);

my $io = File::IO->new(chomp => 1);

$io->get(path => $dataFile);

for(@{$io->contents})
{
  my ($t1,$t2,$d) = split ',';

  $distance{$t1}{$t2}     = $d;
  $distribution{$t1}{$t2} = 1/$d**2;
}
my @towns = sort keys %distance;
my $np    = N + sum(1 .. N);

my @population;

initializePopulation();

my $pf   = ' ';
my $done = 0;

my %bestBee = copyIndividual($population[0]);

print "      0: ",join(' ',sprintf('(%4d)',$bestBee{COST}),map { sprintf '%4d',$population[$_]{COST} } 0 .. 9)," | ",
  join(' ',map { sprintf '%4d',$population[$_]{COST} } -10 .. -1),"\n";

my $genPassed;
my $genBest;
my %bestCount;

for my $gen (1 .. MAX_GENERATIONS)
{
  getNextGeneration();

  if($population[0]{COST} < $bestBee{COST})
  {
    %bestBee = copyIndividual($population[0]);
    $genBest = $gen;

    if($bestBee{COST} < $goal)
    {
      $genPassed = $gen if !defined $genPassed;

      $pf = '*';
    }
    $bestCount{$bestBee{COST}} = 0;
  }
  $done = 1 if ($population[0]{COST} - $population[-1]{COST}) == 0;

  printf "$pf %5d: (%4d) %s | %s\n",$gen,$bestBee{COST},join(' ',map { sprintf '%4d',$population[$_]{COST} } 0 .. 9),
    join(' ',map { sprintf '%4d',$population[$_]{COST} } -10 .. -1),$bestBee{COST} if $done || $bestCount{$bestBee{COST}}++ == 0;

  last if $done;
}
my $r0 = $bestBee{ROUTE};

print "\n";
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
printf "$bestBee{COST}/$goal = %4.2f\n",$bestBee{COST}/$goal;
print "Passed goal at generation: $genPassed\n";
print "Found best solution at generation: $genBest\n";
print 'Best solutions (',scalar(keys %bestCount),'): ',join(' ',sort { $a <=> $b } keys %bestCount),"\n";

sub initializePopulation
{
  @population = ();

  for(1 .. $np)
  {
    my $route = [];
    my $cost;

    newRoute($route);

    $cost = sum map { $distance{$route->[$_]}{$route->[$_+1]} } (0 .. $#towns-1,-1);

    my $p = {ROUTE => $route,COST => $cost,ACTIVE => 1};

    push @population,$p;
  }
  @population = sort { $a->{COST} <=> $b->{COST} } @population;
}

sub newRoute
{
  my ($route) = @_;

  my $t = $towns[0];

  @{$route} = ($t);

  my %route = ($t => 1);

  for(1 .. $#towns)
  {
    my $d = $distribution{$t};

    my @keys = grep { !exists $route{$_} } keys %{$d};
    my $s    = sum map { $d->{$_} } @keys;

    my @pct  = map { int(100*($d->{$_}/$s) + 0.5) } @keys;
    my @dist = map { ($keys[$_]) x $pct[$_] } 0 .. $#keys;

    $t = $dist[int(rand @dist)];

    push @{$route},$t;

    $route{$t} = 1;
  }
}

sub copyIndividual
{
  my ($individual) = @_;

  return (COST => $individual->{COST},ACTIVE => $individual->{ACTIVE},ROUTE => [@{$individual->{ROUTE}}]);

}

sub getNextGeneration
{
  my $x = N;

  for my $i (0 .. N-1)
  {
    my $j = N - $i;

    for(1 .. $j)
    {
      %{$population[$x]} = copyIndividual($population[$i]);

      my $route = $population[$x]{ROUTE};

      for (1 .. int(rand 4))
      {
        swapTown($route);
      }
      $population[$x++]{COST} = sum map { $distance{$route->[$_]}{$route->[$_+1]} } (0 .. $#towns-1,-1);
    }
  }
  $x = N;

  for my $i (0 .. N-1)
  {
    my $j = N - $i;

    my ($tmp) = sort { $a->{COST} <=> $b->{COST} } @population[$i,$x .. $x+$j-1];

    %{$population[$i]} = copyIndividual($tmp);

    $x += $j
  }
  @population[0 .. N-1] = sort { $a->{COST} <=> $b->{COST} } @population[0 .. N-1];
}

sub swapTown
{
  my ($route) = @_;

  my $town1 = int(rand $#towns);
  my $town2 = int(rand $#towns);

  $town2 = int(rand $#towns) + 1 while $town1 == $town2;

  ($route->[$town1],$route->[$town2]) = ($route->[$town2],$route->[$town1]);

  push @{$route},shift @{$route} while $route->[0] ne $towns[0];
}
