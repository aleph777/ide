#!/usr/bin/perl -w    # -*-Perl-*-

#use Math::Random::Secure;
srand(8675309);
use List::Util qw(sum);
use File::IO;
use strict;

use constant MAX_GENERATIONS => 99999;
use constant ALPHA           => 0.1;

local $| = 1;

my %distance;
my %pheremone;

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
}
my @towns = sort keys %distance;
my $np    = 2*@towns;

foreach my $t (@towns)
{
  my @keys = keys %{$distance{$t}};
  my @rcp2 = map { 1/$distance{$t}{$_}**2 } @keys;
  my $s    = sum @rcp2;
  my $r    = {map { $keys[$_] => $rcp2[$_]/$s } 0 .. $#keys};

  $pheremone{$t} = $r;
}
my @population;

initializePopulation();

my $best = $population[0]{COST};
my $pf   = ' ';
my $done = 0;

my %bestAnt = copyIndividual($population[0]);

print "      0: ",join(' ',sprintf('(%4d)',$bestAnt{COST}),map { sprintf '%4d',$population[$_]{COST} } 0 .. 9)," | ",
  join(' ',map { sprintf '%4d',$population[$_]{COST} } -10 .. -1),"\n";

my $genPassed;
my $genBest;
my %bestCount;

for my $gen (1 .. MAX_GENERATIONS)
{
  getNextGeneration();

  if($population[0]{COST} < $best)
  {
    %bestAnt = copyIndividual($population[0]);
    $best    = $population[0]{COST};
    $genBest = $gen;

    if($best < $goal)
    {
      $genPassed = $gen if !defined $genPassed;

      $pf = '*';
    }
    $bestCount{$best} = 0;
  }
  $done = 1 if ($population[0]{COST} - $population[-1]{COST}) == 0;

  printf "$pf %5d: (%4d) %s | %s\n",$gen,$bestAnt{COST},join(' ',map { sprintf '%4d',$population[$_]{COST} } 0 .. 9),
    join(' ',map { sprintf '%4d',$population[$_]{COST} } -10 .. -1),$bestAnt{COST} if $done || $bestCount{$bestAnt{COST}}++ == 0;

  last if $done;
}
my $r0 = $population[0]{ROUTE};

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
printf "$best/$goal = %4.2f\n",$best/$goal;
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

    my $p = {ROUTE => $route,COST => $cost};

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
    my $pt = $pheremone{$t};

    my @keys = grep { !exists $route{$_} } keys %{$pt};
    my $s    = sum map { $pt->{$_} } @keys;

    my @pct  = map { int(100*($pt->{$_}/$s) + 0.5) } @keys;
    my @dist = map { ($keys[$_]) x $pct[$_] } 0 .. $#keys;

    $t = $dist[int(rand @dist)];

    push @{$route},$t;

    $route{$t} = 1;
  }
}

sub getNextGeneration
{
  updatePheremones();

  for(0 .. $#population)
  {
    my $route = $population[$_]{ROUTE};

    newRoute($route);

   $population[$_]{COST} = sum map { $distance{$route->[$_]}{$route->[$_+1]} } (0 .. $#towns-1,-1);
  }
  @population = sort { $a->{COST} <=> $b->{COST} } @population;
}

sub updatePheremones
{
  for(\%bestAnt,@population)
  {
    my $route = $_->{ROUTE};
    my $delta = 1 + ALPHA/$_->{COST};

    for(0 .. $#towns-1,-1)
    {
      $pheremone{$route->[$_]}{$route->[$_+1]} *= $delta;
    }
  }
  foreach my $t (keys %pheremone)
  {
    my @keys = keys %{$pheremone{$t}};
    my $s    = sum @{$pheremone{$t}}{@keys};dumpPheremones() unless $s;

    @{$pheremone{$t}}{@keys} = map { $pheremone{$t}{$_}/$s } @keys;
  }
}

sub dumpPheremones
{
  for my $t (sort keys %pheremone)
  {
    print map { "{$t}{$_}: $pheremone{$t}{$_}\n" } sort keys %{$pheremone{$t}};
  }
  exit;
}

sub copyIndividual
{
  my ($individual) = @_;

  return (COST => $individual->{COST},ROUTE => [@{$individual->{ROUTE}}]);

}
