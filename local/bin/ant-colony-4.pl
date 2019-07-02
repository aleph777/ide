#!/usr/bin/perl -w    # -*-Perl-*-

use Math::Random::Secure qw(irand);
use List::Util qw(sum max min);
use List::MoreUtils qw(firstidx);
use Time::Local;
use POSIX qw(strftime);
use File::IO;

use strict;
use v5.10;

use constant POPULATION_SIZE  => 100;
use constant MAX_ELITE        => 10;
use constant MAX_ITERATIONS   => 2000;
use constant ENOUGH_IS_ENOUGH => 5;

use constant EVAPORATION_RATE   => 0.75;
use constant PHEREMONE_EXPONENT => 2;
use constant HEURISTIC_EXPONENT => 2;

use constant STALE_THRESHOLD => 500;

use constant MINMAX_PDF_MAX => 950;
use constant MINMAX_DELTA   =>   5;
use constant MINMAX_PDF_MIN =>   9;

use constant NEW_BEST_NO  => '  ';
use constant NEW_BEST_YES => '😎';
use constant WORKING_HARD => '😰 ';
use constant CLOWN_FACE   => '🤡';

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
my %elite;

my @cities;
my @ants;
my @elite;
my @cityIdx;
my @nearestNeighbor;
my @displayRoutes;

my $spacer;
my $goal;
my $newBest;
my $elite;
my $elitePheremoneMax;
my $minmaxEnabled;
my $minmaxMin;
my $minmaxMax;
my $enoughIsEnough;
my $printFlag;
my $bestLength;
my $done;
my $stale;

my $dataFile = defined $ARGV[0] ? $ARGV[0] : 'citys1.csv';

die "Can't find data file!!!"     unless -e $dataFile;
die "$dataFile is a directory!!!" if     -d $dataFile;

my @outKeys = (qw(TIMESTAMP LOCALTIME ELITE MINMAX MINMAX_UPPER MINMAX_LOWER ITERATION),(map { "LENGTH$_" } 0 .. 9),map { "ROUTE$_" } 0 .. 9);

my @lt = localtime;
my $dt = strftime "%Y%m%d%H%M%S",@lt;

my $outFile = $dataFile;

$outFile =~ s/(?:\.distances)?\.csv//;
$outFile .= ".$dt.output.csv";

my $of = File::IO->new(path => $outFile,newline  => 1,append => 1);

$of->put(contents => [join ',',@outKeys]);

@ants = map { {ROUTE => [], LENGTH => undef, ITERATION => undef} } 1 .. POPULATION_SIZE;

GetDistanceData($dataFile);

@cities  = sort keys %distance;
@cityIdx = (0 .. $#cities-1,-1);

$newBest = NEW_BEST_NO;

$printFlag  = 0;
$done       = 0;

$enoughIsEnough    = 0;

$stale  = 0;
$spacer = '';

$goal = CheckNearestNeighbor($dataFile);

SetAlgorithmVariants(MAX_ELITE-1,1);
InitializeHeuristic();
InitializePheremone();
ConstructAntSolutions(0);

$elite    = '';
$bestLength = 9999999;

ProcessSolutions(0);
PrintCurrentIteration($ants[0],0);

for my $iter (1 .. MAX_ITERATIONS)
{
  if($iter % 1000 == 0)
  {
    say WORKING_HARD,$iter,':    ',join ' ',map { sprintf '%4d',$_->{LENGTH} } @ants[-26 .. -1];

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
printf "$elite[0]{LENGTH}/$goal = %4.2f\n",$elite[0]{LENGTH}/$goal;
say 'POPULATION_SIZE    => ',POPULATION_SIZE;
say 'MAX_ELITE          => ',MAX_ELITE;
say 'EVAPORATION_RATE   => ',EVAPORATION_RATE;
say 'PHEREMONE_EXPONENT => ',PHEREMONE_EXPONENT;
say 'HEURISTIC_EXPONENT => ',HEURISTIC_EXPONENT;

sub SetAlgorithmVariants
{
  my $epm = shift;
  my $mme = shift;

  say 'SetAlgorithmVariants :(',$epm,',',$mme,')';
  say 'Number of Elite pheremone dropping ants changed to: ',$epm if defined $elitePheremoneMax && $elitePheremoneMax != $epm;

  $elitePheremoneMax = $epm;

  if(defined $minmaxEnabled && $minmaxEnabled != $mme)
  {
    if($mme)
    {
      say 'Minmax enabled to (',$minmaxMax,', ',$minmaxMin,')';
    }
    else
    {
      say 'Minmax disabled'
    }
  }
  $minmaxEnabled = $mme;
  $minmaxMax = defined $minmaxMax && $mme ? max($minmaxMax - 5,500) : MINMAX_PDF_MAX;
  $minmaxMin = MINMAX_PDF_MIN;

  say 'minmax: ',$minmaxMax;

  print $spacer;
  say '';
}

sub CopyAnt
{
  my ($ant) = @_;

  my $r = {LENGTH    => $ant->{LENGTH},
           ROUTE     => [@{$ant->{ROUTE}}],
           TEXT      => $ant->{TEXT},
           ITERATION => $ant->{ITERATION},};

  return $r;
}

# Check if we've got a new best elite route or best route
#
sub ProcessSolutions
{
  my $iter = shift;

  my @tmpAnts = grep { !exists $elite{$_->{TEXT}} } @ants;

  @elite = sort { $a->{LENGTH} <=> $b->{LENGTH} } @elite,@tmpAnts;

  splice @elite,MAX_ELITE;

  @elite = map { CopyAnt($_) } @elite;

  my $e = $elite[0];

  my $newElite = join ',',map { $_->{LENGTH} } @elite;

  # $formatElite = join ' ',map { sprintf '%4d',$_->{LENGTH} } @elite;

  if($newElite ne $elite)
  {
    if($e->{LENGTH} < $bestLength)
    {
      $newBest  = NEW_BEST_YES;

      $bestLength = $e->{LENGTH};

      SetAlgorithmVariants(MAX_ELITE-1,0) if $minmaxEnabled;
    }
    else
    {
      $newBest = NEW_BEST_NO;
    }
    for(0 .. $#elite)
    {
      my $el     = $elite[$_];
      my $route  = $el->{ROUTE};
      my $length = $el->{LENGTH};
      my $text   = $el->{TEXT};

      next if exists $elite{$text};

      # $formatElite =~ s/$length/join('',TEXT_BOLD,TEXT_GREEN,$length,TEXT_NORMAL)/e;

      $el->{NEW}    = 1;
      $elite{$text} = $length;
    }
    # $formatElite =~ s/$e->{LENGTH}/join('',TEXT_BOLD,TEXT_YELLOW,$e->{LENGTH},TEXT_NORMAL)/e unless $bestLength == $e->{LENGTH};

    $printFlag = 1;
    $elite     = $newElite;

    $enoughIsEnough = $stale = 0;
  }
  else
  {
    $printFlag = 0;
    $newBest   = NEW_BEST_NO;

    if(++$stale >= STALE_THRESHOLD)
    {
      SetAlgorithmVariants(MAX_ELITE-1,1);
      $stale = 0;
    }
  }
  $done = 1 if $elite[0]{LENGTH} == $elite[-1]{LENGTH};
}

sub UpdatePheremones
{
  my %tau;

  # sum the contributions for each elite ant — τ(ij) = 𝜮(1/L)
  #
  foreach my $ant (@elite[0 .. $elitePheremoneMax-1])
  {
    my $route = $ant->{ROUTE};
    my $tau   = 1 + 1/$ant->{LENGTH};

    for(@cityIdx)
    {
      $tau{$route->[$_]}{$route->[$_+1]} = 0 unless exists $tau{$route->[$_]}{$route->[$_+1]};

      $tau{$route->[$_]}{$route->[$_+1]} += $tau;
    }
  }
  # pheremone(ij) = (1 - ρ)τ(ij) + ρ×𝜮τ
  #
  foreach my $city1 (@cities)
  {
    foreach my $city2 (@cities)
    {
      next unless exists $tau{$city1}{$city2};

      $pheremone{$city1}{$city2} = (1 - EVAPORATION_RATE)*$tau{$city1}{$city2} + EVAPORATION_RATE*$pheremone{$city1}{$city2};
    }
  }
}

sub PrintCurrentIteration
{
  my $ant = shift;
  my $itr = shift;

  my $r = $ant->{ROUTE};

  my $formatElite = join ' ',map { sprintf '%4d',$_->{LENGTH} } @elite;

  my @newElite = grep { exists $_->{NEW} } @elite;

  $formatElite =~ s/$_->{LENGTH}/join('',TEXT_BOLD,TEXT_GREEN,$_->{LENGTH},TEXT_NORMAL)/e for @newElite;
  $formatElite =~ s/$elite[0]{LENGTH}/join('',TEXT_BOLD,TEXT_YELLOW,$elite[0]{LENGTH},TEXT_NORMAL)/e unless exists $elite[0]{NEW};

  print $spacer if $spacer;

  unless($done)
  {
    printf "%s\t %d: %s %4s %s\n",
      $newBest,
      $itr,
      $formatElite,
      '....',
      join(' ',map { sprintf '%4d',$ants[$_]{LENGTH} } -10 .. -1);

    my @lt = localtime;

    my $out = join ',',
      strftime("%Y%m%d %H:%M:%S",@lt),
      timelocal(@lt),
      $elitePheremoneMax,
      $minmaxEnabled,
      $minmaxMax,
      $minmaxMin,
      $itr,
      (map { $elite[$_]{LENGTH} } 0 .. MAX_ELITE-1),
      (map { join '',@{$elite[$_]{ROUTE}} } 0 .. MAX_ELITE-1);

    PrintRoutes(\@newElite);
    $of->put(contents => $out);
  }
  else
  {
    say sprintf '%11d: %d',$itr,$ant->{LENGTH};

    PrintRoutes([$ant]);
  }
}

sub PrintRoutes
{
  my $ants = shift;

  foreach my $ant (@{$ants})
  {
    my $route = $ant->{ROUTE};

    say 'CITY:  ',join ' ',map { sprintf '%4s',$_ } @{$route},'A';
    say 'MILES: ',join ' ',' ' x 4,map { sprintf '%4d',$distance{$route->[$_]}{$route->[$_+1]} } @cityIdx;

    my $total = '   0';

    my $d = 0;

    for(@cityIdx)
    {
      $d += $distance{$route->[$_]}{$route->[$_+1]};

      $total .= sprintf ' %4d',$d;
    }
    say 'TOTAL: ',$total,"\n";
  }
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

    # find all the cities we haven't been too already
    #
    my @destinations = grep { !exists $route{$_} } keys %{$pheremone};

    # P(ij) = τ(ij)**α⋅η(ij)**β/𝜮(τ**α⋅η**β)
    #
    my %tau_x_beta     = map { $_ => $pheremone->{$_}**PHEREMONE_EXPONENT*$heuristic->{$_}**HEURISTIC_EXPONENT } @destinations;
    my $sum_tau_x_beta = sum @tau_x_beta{@destinations};

    my @normalization  = map { max(int(1000*($tau_x_beta{$_}/$sum_tau_x_beta) + 0.5),1) } @destinations;

    # only one possible destination
    #
    if(@normalization == 1)
    {
      $city = $destinations[0];

      push @{$route},$city;

      $route{$city} = 1;

      next;
    }
    # flatten normalized distribution if we're minmaxing
    #
    if($minmaxEnabled)
    {
      my $idx = firstidx { $_ > $minmaxMax } @normalization;

      # if there's a destination above the threshold
      #
      if($idx != -1)
      {
        # calculate how much is it over the threshold
        #
        my $diff  = $normalization[$idx] - $minmaxMax;

        # find all of the others
        #
        my @nidx  = grep { $_ != $idx } 0 .. $#normalization;

        # divide evenly among the rest
        #
        my $divvy = $diff/@nidx;

        # round and set a floor
        #
        $normalization[$_]   = max(int($normalization[$_] + $divvy + 0.5),$minmaxMin) for @nidx;
        $normalization[$idx] = $minmaxMax;
      }
    }
    die "@normalization -- @destinations" if grep { $normalization[$_] < 0 } 0 .. $#destinations;
    @{$probabilityDistribution} = map { ($destinations[$_]) x $normalization[$_] } 0 .. $#destinations;

    my $idx = irand @{$probabilityDistribution};

    $city = $probabilityDistribution->[$idx];

    push @{$route},$city;

    $route{$city} = 1;
  }
  $ant->{LENGTH} = sum map { $distance{$route->[$_]}{$route->[$_+1]} } @cityIdx;
  $ant->{TEXT}   = join '',@{$route};
}

# Construct a Solution (route and length) for ants from startIndex to POPULATION_SIZE
#
sub ConstructAntSolutions
{
  my $iteration  = shift;

  for(0 .. $#ants)
  {
    ConstructAntSolution($ants[$_],$iteration);
  }
  @ants = sort { $a->{LENGTH} <=> $b->{LENGTH} } @ants;
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
    # my @rcp2 = map { 1/$distance{$t}{$_}**2 } @keys;
    my @rcp2 = map { 1/$distance{$t}{$_} } @keys;
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

