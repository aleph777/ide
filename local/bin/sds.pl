#!/usr/bin/perl -w    # -*-Perl-*-

#use Math::Random::Secure;
srand(8675309);
use List::Util qw(sum min);
use File::IO;
use strict;

use constant MAX_GENERATIONS => 99900;

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

  $distance{$t1}{$t2} = $d;
  $distribution{$t1}{$t2} = 1/$d**2;
}
my @towns = sort keys %distance;
my $np    = 2*@towns;

my @population;

initializePopulation();

my $pf   = ' ';
my $done = 0;

my %bestAgent = copyIndividual($population[0]);

print "      0: ",join(' ',sprintf('(%4d)',$bestAgent{COST}),map { sprintf '%4d',$population[$_]{COST} } 0 .. 9)," | ",
  join(' ',map { sprintf '%4d',$population[$_]{COST} } -10 .. -1),"\n";

my $genPassed;
my $genBest;
my %bestCount;

for my $gen (1 .. MAX_GENERATIONS)
{
  getNextGeneration();

  if($population[0]{COST} < $bestAgent{COST})
  {
    %bestAgent = copyIndividual($population[0]);
    $genBest = $gen;

    if($bestAgent{COST} < $goal)
    {
      $genPassed = $gen if !defined $genPassed;

      $pf = '*';
    }
    $bestCount{$bestAgent{COST}} = 0;
  }
  $done = 1 if ($population[0]{COST} - $population[-1]{COST}) == 0;

  printf "$pf %5d: (%4d) %s | %s\n",$gen,$bestAgent{COST},join(' ',map { sprintf '%4d',$population[$_]{COST} } 0 .. 9),
    join(' ',map { sprintf '%4d',$population[$_]{COST} } -10 .. -1),$bestAgent{COST} if $done || $bestCount{$bestAgent{COST}}++ == 0;

  last if $done;
}
my $r0 = $bestAgent{ROUTE};

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
printf "$bestAgent{COST}/$goal = %4.2f\n",$bestAgent{COST}/$goal;
print "Passed goal at generation: $genPassed\n";
print "Found best solution at generation: $genBest\n";
print 'Best solutions (',scalar(keys %bestCount),'): ',join(' ',sort { $a <=> $b } keys %bestCount),"\n";

sub newRoute
{
  my ($route) = @_;

  my @tmp = @towns;

  my $t = shift @tmp;

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

sub copyIndividual
{
  my ($individual) = @_;

  return (COST => $individual->{COST},ACTIVE => $individual->{ACTIVE},ROUTE => [@{$individual->{ROUTE}}]);

}

sub getNextGeneration
{
  for my $i (0 .. $#population)
  {
    my $j = int(rand @population);

    $population[$i]{ACTIVE} = $population[$i]{COST} < $population[$j]{COST} ? 1 : 0;
  }
  for my $i (grep { $population[$_]{ACTIVE} == 0 } 0 .. $#population)
  {
    my $j  = int(rand @population);
    my $pi = $population[$i];
    my $pj = $population[$j];

    if($pj->{ACTIVE})
    {
      %{$pi} = copyIndividual($pj);

      my $swaps = int(rand @towns/4) + 1;

      for(1 .. $swaps)
      {
        swapTown($pi->{ROUTE});
      }
    }
    else
    {
      newRoute($pi->{ROUTE});
    }
    $pi->{COST} = sum map { $distance{$pi->{ROUTE}[$_]}{$pi->{ROUTE}[$_+1]} } (0 .. $#towns-1,-1);
  }
  @population = sort { $a->{COST} <=> $b->{COST} } @population;
}

sub swapTown
{
  my ($route) = @_;

  my $town1 = int(rand $#towns) + 1;
  my $town2 = int(rand $#towns) + 1;

  $town2 = int(rand @towns) while $town1 == $town2;

  ($route->[$town1],$route->[$town2]) = ($route->[$town2],$route->[$town1]);

}
