#!/usr/bin/perl -w    # -*-Perl-*-

use Math::Random::Secure qw(irand);

use List::Util qw(sum max min);
use File::IO;

use strict;
use v5.10;

use constant POPULATION_SIZE  => 100;
use constant MAX_ELITE        => 10;
use constant MAX_ITERATIONS   => 99999;
use constant ENOUGH_IS_ENOUGH => 5;

use constant EVAPORATION_RATE   => 0.75;
use constant PHEREMONE_EXPONENT => 2;
use constant HEURISTIC_EXPONENT => 2;

use constant PDF_MINMAX_MIN =>  10;
use constant PDF_MINMAX_MAX => 500;

use constant MET_GOAL_NO  => '🙁';
use constant MET_GOAL_YES => '😎';
use constant NEW_BEST_NO  => '  ';
use constant NEW_BEST_YES => '😎';
use constant WORKING_HARD => '😰 ';

use constant TEXT_BOLD   => "\e[1m";
use constant TEXT_NORMAL => "\e[0m";
use constant TEXT_YELLOW => "\e[33m";
use constant TEXT_GREEN  => "\e[32m";

local $| = 1;

# Global variables
#
my %distance;
my %pheremone;
my %heuristic;
my %probabilityDistribution;

my @cities;
my @ants;
my @elite;
my @cityIdx;
my @nearestNeighbor;

my $spacer;
my $metGoal;
my $goal;
my $newBest;
my $elite;
my $enoughIsEnough;
my $printFlag;
my $bestCost;
my $iterationPassed;
my $done;

my $dataFile = defined $ARGV[0] ? $ARGV[0] : 'citys1.csv';

die "Can't find data file!!!"     unless -e $dataFile;
die "$dataFile is a directory!!!" if     -d $dataFile;

@ants = map { {ROUTE => [], COST => undef, ITERATION => undef} } 1 .. POPULATION_SIZE;

GetDistanceData($dataFile);

@cities  = sort keys %distance;
@cityIdx = (0 .. $#cities-1,-1);

$metGoal = MET_GOAL_NO;
$newBest = NEW_BEST_NO;

$printFlag = 0;
$done      = 0;

$goal = CheckNearestNeighbor($dataFile);

InitializeHeuristic();
InitializePheremone();
ConstructAntSolutions(0);

$elite    = '';
$bestCost = 9999999;

ProcessSolutions(0);
PrintCurrentIteration($ants[0],0);

$enoughIsEnough = 0;

for my $iter (1 .. MAX_ITERATIONS)
{
  if($iter % 1000 == 0)
  {
    say WORKING_HARD,$iter,':    ',join ' ',map { sprintf '%4d',$_->{COST} } @ants[-26 .. -1];

    $spacer = "\n";

    last if ++$enoughIsEnough >= ENOUGH_IS_ENOUGH;
  }
  UpdatePheremones();
  ConstructAntSolutions($iter);
  ProcessSolutions($iter);
  PrintCurrentIteration($ants[0],$iter) if $printFlag;

  last if $done;
}
$done = 1;

say "\n",'=' x 40,' Finished ','=' x 40,"\n";

for(reverse @elite)
{
  PrintCurrentIteration($_,$_->{ITERATION});
}
printf "$elite[0]{COST}/$goal = %4.2f\n",$elite[0]{COST}/$goal;

sub CopyAnt
{
  my ($ant) = @_;

  my $r = {COST      => $ant->{COST},
           ROUTE     => [@{$ant->{ROUTE}}],
           ITERATION => $ant->{ITERATION}};

  return $r;
}

sub ProcessSolutions
{
  my $iter = shift;

  @elite = map { CopyAnt($_) } sort { $a->{COST} <=> $b->{COST} } @elite,@ants;

  splice @elite,MAX_ELITE;

  my $newElite = join ',',map { $_->{COST} } @elite;

  # die "$elite -- $newElite" if $iter == 1;

  if($newElite ne $elite)
  {
    my $newBestCost = $elite[0]{COST};

    if($newBestCost < $bestCost)
    {
      if(!defined $iterationPassed)
      {
        if($newBestCost < $goal)
        {
          $metGoal         = MET_GOAL_YES;
          $iterationPassed = $iter;
        }
      }
      $newBest  = NEW_BEST_YES;
      $bestCost = $newBestCost;
    }
    else
    {
      $newBest = NEW_BEST_NO;
    }
    $printFlag = 1;
    $elite     = $newElite;

    $enoughIsEnough = 0;
  }
  else
  {
    $printFlag = 0;
    $newBest   = NEW_BEST_NO;
  }
  $done = 1 if $elite[0]{COST} == $elite[-1]{COST};
}

#  τ′ = (1 - ρ)τ + ρ×𝜮τ
#
sub UpdatePheremones
{
  foreach my $ant (@elite)
  {
    my $route = $ant->{ROUTE};
    my $tau   = 1 + 1/$ant->{COST};

    for(@cityIdx)
    {
      $pheremone{$route->[$_]}{$route->[$_+1]} = (1 - EVAPORATION_RATE)*$tau + EVAPORATION_RATE*$pheremone{$route->[$_]}{$route->[$_+1]};
    }
  }
}

sub PrintCurrentIteration
{
  my $ant = shift;
  my $itr = shift;

  my $r = $ant->{ROUTE};

  print $spacer if $spacer;

  unless($done)
  {
    my $formatElite = join ' ',map { sprintf '%4d',$_->{COST} } @elite;

    $formatElite =~ s/$ant->{COST}/join('',TEXT_BOLD,TEXT_GREEN,$ant->{COST},TEXT_NORMAL)/e;
    $formatElite =~ s/$elite[0]{COST}/join('',TEXT_BOLD,TEXT_YELLOW,$elite[0]{COST},TEXT_NORMAL)/e unless $ant->{COST} == $elite[0]{COST};

    printf "%-6s %5d: %s %4s %s\n",
      join('',$newBest,$metGoal),
      $itr,
      $formatElite,
      '....',
      join(' ',map { sprintf '%4d',$ants[$_]{COST} } -10 .. -1);
  }
  else
  {
    say sprintf '%11d: %d',$itr,$ant->{COST};
  }
  say 'CITY:  ',join ' ',map { sprintf '%4s',$_ } @{$r},'A';
  say 'MILES: ',join ' ',' ' x 4,map { sprintf '%4d',$distance{$r->[$_]}{$r->[$_+1]} } @cityIdx;

  my $total = '   0';

  my $d = 0;

  for(@cityIdx)
  {
    $d += $distance{$r->[$_]}{$r->[$_+1]};

    $total .= sprintf ' %4d',$d;
  }
  say 'TOTAL: ',$total,"\n";

  $newBest = '  ';
}

# Get a solution for one ant
#
sub ConstructAntSolution
{
  my $ant  = shift;
  my $iter = shift;
  my $city = $cities[0];

  my $route = $ant->{ROUTE};

  $ant->{ITERATION} = $iter;

  @{$route} = ($city);

  my %route = ($city => 1);

  for(1 .. $#cities)
  {
    my $pheremone = $pheremone{$city};
    my $heuristic = $heuristic{$city};

    my $probabilityDistribution = $probabilityDistribution{$city};

    my @keys = grep { !exists $route{$_} } keys %{$pheremone};
    my %tmp  = map { $_ => $pheremone->{$_}**PHEREMONE_EXPONENT*$heuristic->{$_}**HEURISTIC_EXPONENT } @keys;
    my $sum  = sum @tmp{@keys};

    my @normalization  = map { min(500,int(1000*($tmp{$_}/$sum) + 0.5) + 1) } @keys;

    @{$probabilityDistribution} = map { ($keys[$_]) x $normalization[$_] } 0 .. $#keys;


    my $idx = irand @{$probabilityDistribution};

    $city = $probabilityDistribution->[$idx];

    # $city = $probabilityDistribution->[irand @{$probabilityDistribution}];

    die "$idx: " unless defined $city;

    push @{$route},$city;

    $route{$city} = 1;
  }
  $ant->{COST} = sum map { $distance{$route->[$_]}{$route->[$_+1]} } @cityIdx;
}

# Construct a Solution (route and cost) for ants from startIndex to POPULATION_SIZE
#
sub ConstructAntSolutions
{
  my $iteration  = shift;

  for(0 .. $#ants)
  {
    ConstructAntSolution($ants[$_],$iteration);
  }
  @ants = sort { $a->{COST} <=> $b->{COST} } @ants;
}

# Pheremone initialized to 1
#
sub InitializePheremone
{
  foreach my $t (@cities)
  {
    my @keys = keys %{$distance{$t}};
    my $r    = {$t => 0,map { $keys[$_] => 1 } 0 .. $#keys};

    $pheremone{$t} = $r;
  }
}

# Heuristic between A and B = (1/distance{A}{B}**2)/sum(distance{A}{B}, distance{A}{C}, ...)
#
sub InitializeHeuristic
{
  foreach my $t (@cities)
  {
    my @keys = keys %{$distance{$t}};
    my @rcp2 = map { 1/$distance{$t}{$_}**2 } @keys;
    my $s    = sum @rcp2;
    my $r    = {$t => 0,map { $keys[$_] => $rcp2[$_]/$s } 0 .. $#keys};

    $heuristic{$t} = $r;
  }
}


sub GetDistanceData
{
  my $path = shift;

  my $io = File::IO->new(chomp => 1);

  $io->get(path => $path);

  for(@{$io->contents})
  {
    my ($t1,$t2,$d) = split ',';

    $distance{$t1}{$t2} = $d;
  }
}

# Run naive search algorithm; print results and return basic goal
#
sub CheckNearestNeighbor
{
  my $dataFile = shift;

  @nearestNeighbor = qx(tsp-nearest-neighbor.pl $dataFile);

  chomp @nearestNeighbor;

  say $nearestNeighbor[$_] for 0 .. 2;

  say "\n",'GOAL is: ',TEXT_BOLD,TEXT_YELLOW,$nearestNeighbor[3],TEXT_NORMAL,"\n";

  return $nearestNeighbor[3];
}

