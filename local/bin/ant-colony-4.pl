#!/usr/bin/perl -w    # -*-Perl-*-

use Math::Random::Secure qw(irand);
use List::Util qw(sum max min);
use List::MoreUtils qw(firstidx);
use Time::Local;
use POSIX qw(strftime);
use File::IO;

use strict;
use v5.10;

use constant POPULATION_SIZE  => 50;
use constant MAX_ELITE        => 10;
use constant MAX_ITERATIONS   => 9999999;
use constant ENOUGH_IS_ENOUGH => 5;

use constant EVAPORATION_RATE   => 0.975;
use constant INITIAL_EXPONENT   => 1;
use constant PHEREMONE_EXPONENT => 2;
use constant HEURISTIC_EXPONENT => 2;

use constant STALE_THRESHOLD => 250;
use constant STALE_PARTIAL   => STALE_THRESHOLD/10;

use constant NEW_BEST_NO  => '  ';
use constant NEW_BEST_YES => '😎';
use constant CLOWN_FACE   => '🤡';
use constant MONOCLE      => '🧐';
use constant NEUTRAL      => '😐';
use constant WORKING_HARD => '😰';
use constant UNAMUSED     => '😒';
use constant ANGUISHED    => '😧';
use constant BANDAGES     => '🤕';
use constant HOT          => '🥵 ';
use constant ANGRY        => '😠';
use constant DEAD         => '💀';
use constant OOPS         => '🤪';
use constant THINKING     => '🤔 ';  # NOT USED

use constant TEXT_BOLD   => "\e[1m";
use constant TEXT_NORMAL => "\e[0m";
use constant TEXT_RED    => "\e[31m";
use constant TEXT_YELLOW => TEXT_BOLD."\e[33m";
use constant TEXT_GREEN  => "\e[32m";
use constant TEXT_CYAN   => "\e[36m";

local $| = 1;

# Global variables
#
my %distance;
my %pheremone;
my %heuristic;
my %probabilityDistribution;
my %elite;
my %stale;

my @cities;
my @ants;
my @elite;
my @cityIdx;
my @nearestNeighbor;
my @displayRoutes;
my @metadata;
my @stale;

my $spacer;
my $goal;
my $newBest;
my $elite;
my $printFlag;
my $bestLength;
my $baseTime;
my $iter_max;
my $done;
my $stale;
my $bored;
my $oops;
my $metGoalIteration;

my $mapFile  = defined $ARGV[0] ? $ARGV[0] : 'map4';
my $dataFile = "$mapFile.distances.csv";

die "Can't find data file!!!"     unless -e $dataFile;
die "$dataFile is a directory!!!" if     -d $dataFile;

my @outKeys  = (qw(ITERATION),(map { "L$_" } 0 .. 9),map { "R$_" } 0 .. 9);
my @metaKeys = qw(TIMESTAMP GOAL ITERATIONS POPULATION ELITE EVAPORATION_RATE INITIAL_EXPONENT HEURISTIC_EXPONENT PHEREMONE_EXPONENT STALE_THRESHOLD);

@stale = ('',
          join(': ',CLOWN_FACE,q(I was told there would be cake.)),
          join(': ',MONOCLE,q(Nothing to see here...)),
          join(': ',UNAMUSED,q(Been there, done that.)),
          join(': ',NEUTRAL,q(Boring...)),
          join(': ',ANGUISHED,q(Are we there yet?)),
          join(': ',WORKING_HARD,q(Aww, c'mon already!)),
          join(': ',BANDAGES,q(My feet hurt!)),
          join(': ',ANGRY,q(Seriously?)),
          join(': ',HOT,q(Aaargh!)),
          join(': ',DEAD,q(Enough is enough.)),
         );

push @metadata,join ',',@metaKeys;

my @lt = localtime;
my $dt = strftime "%Y%m%d%H%M%S",@lt;

my $outFile  = "$mapFile.$dt.output.csv";
my $metaFile = "$mapFile.$dt.metadata.csv";

$baseTime = timelocal(@lt);

my $of = File::IO->new(path => $outFile, newline  => 1,append => 1);
my $mf = File::IO->new(path => $metaFile,newline  => 1);

$of->put(contents => [join ',',@outKeys]);

@ants = map { {ROUTE => [], LENGTH => undef, ITERATION => undef} } 1 .. POPULATION_SIZE;

GetDistanceData($dataFile);

@cities  = sort keys %distance;
@cityIdx = (0 .. $#cities-1,-1);

$newBest = NEW_BEST_NO;

$printFlag = 0;
$spacer    = '';
$elite     = '';

$done  = 0;
$stale = 0;
$bored = 0;
$oops  = 0;

$goal = CheckNearestNeighbor($dataFile);

InitializeHeuristic();
InitializePheremone();
ConstructAntSolutions(0);

$bestLength = 9999999;

ProcessSolutions(0);
PrintCurrentIteration($ants[0],0);

for my $iter (1 .. MAX_ITERATIONS)
{
  UpdatePheremones();
  ConstructAntSolutions($iter);
  ProcessSolutions($iter);
  PrintCurrentIteration($ants[0],$iter) if $printFlag;

  last if $done;
}
$done = 1;

push @metadata,join ',',$dt,$goal,$iter_max,POPULATION_SIZE,MAX_ELITE,EVAPORATION_RATE,INITIAL_EXPONENT,HEURISTIC_EXPONENT,PHEREMONE_EXPONENT,STALE_THRESHOLD;

$mf->put(contents => \@metadata);

say "\n",'=' x 40,' Finished ','=' x 40,"\n";

for(reverse @elite)
{
  PrintCurrentIteration($_,$_->{ITERATION});
}
CompareResults($elite[0]{ROUTE});

say 'Population Size       => ',POPULATION_SIZE;
say 'Number of Elite       => ',MAX_ELITE;
say 'Evaporation Rate      => ',EVAPORATION_RATE;
say 'Initial Exponent      => ',INITIAL_EXPONENT;
say 'Pheremone Exponent    => ',PHEREMONE_EXPONENT;
say 'Heuristic Exponent    => ',HEURISTIC_EXPONENT;
say 'Number of Interations => ',$iter_max;
say 'Met goal at Iteration => ',$metGoalIteration;
say TEXT_YELLOW,sprintf("$elite[0]{LENGTH}/$goal = %4.2f",$elite[0]{LENGTH}/$goal),TEXT_NORMAL;

sub CompareResults
{
  my $route = shift;

  say '-' x 10,' vs. ','-' x 10,"\n";
  say $nearestNeighbor[$_] for 0 .. 2;

  my @d;

  my $d = 0;

  for(@cityIdx)
  {
    $d += $distance{$route->[$_]}{$route->[$_+1]};

    push @d,$d;
  }
  my @nn = (split / +/,$nearestNeighbor[2])[-@{$route} .. -1];

  my @diff = map { sprintf ' %4d',$d[$_] - $nn[$_] } 0 .. $#{$route};

  @diff = map { $_ > 0 ? TEXT_RED.$_ : TEXT_GREEN.$_ } @diff;

  s/-/length $_ < 11 ? ' ' : ''/e for @diff;

  print '           ',TEXT_BOLD,@diff,TEXT_NORMAL,"\n\n";
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

  my %tmpAnts;
  my @tmpAnts;

  # Don't allow ants from this iteration to have the same route or
  # one of the already discovered elite routes
  #
  for(@ants)
  {
    if(!exists $tmpAnts{$_->{TEXT}})
    {
      push @tmpAnts,$_ unless exists $elite{$_->{TEXT}};

      $tmpAnts{$_->{TEXT}} = 1;
    }
  }
  @elite = sort { $a->{LENGTH} <=> $b->{LENGTH} } @elite,@tmpAnts;

  splice @elite,MAX_ELITE;

  @elite = map { CopyAnt($_) } @elite;

  my $e = $elite[0];

  my $newElite = join ',',map { $_->{LENGTH} } @elite;

  if($newElite ne $elite)
  {
    $metGoalIteration = $iter if !defined $metGoalIteration && $e->{LENGTH} < $goal;

    if($e->{LENGTH} < $bestLength)
    {
      $newBest  = NEW_BEST_YES;

      $bestLength = $e->{LENGTH};
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

      $el->{NEW}    = 1;
      $elite{$text} = $length;
    }
    $printFlag = 1;
    $elite     = $newElite;

    $stale = 0;
    $bored = 0;
  }
  else
  {
    $printFlag = 0;
    $newBest   = NEW_BEST_NO;

    if(++$stale % STALE_PARTIAL == 0)
    {
      my $staleIdx    = $stale/STALE_PARTIAL;
      my @formatStale = split ': ',$stale[$staleIdx];
      my $formatStale = sprintf '%2s%8d',$formatStale[0],$iter;

      $bored = 1 if $formatStale[0] eq NEUTRAL;

      say sprintf '%s: %s',$formatStale,$formatStale[1];

      $spacer = "\n";
    }
    if($stale >= STALE_THRESHOLD)
    {
      $iter_max = $iter;
      $done     = 1;
    }
  }
}

sub UpdatePheremones
{
  my %tau;

  # sum the contributions for each elite ant — τ(ij) = 𝜮(1/L)
  #
  foreach my $ant (@elite[0 .. MAX_ELITE-1])
  {
    my $route = $ant->{ROUTE};
    my $tau   = 10000/$ant->{LENGTH};

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

  my $ratio = sprintf '%4.2f',$elite[0]{LENGTH}/$goal;

  my $formatRatio = $ratio < 1 ?  join '',TEXT_BOLD,TEXT_GREEN,$ratio,TEXT_NORMAL : join '',TEXT_BOLD,TEXT_RED,$ratio,TEXT_NORMAL;

  $formatElite =~ s/$_->{LENGTH}/join('',TEXT_BOLD,TEXT_GREEN,$_->{LENGTH},TEXT_NORMAL)/e for @newElite;
  $formatElite =~ s/$elite[0]{LENGTH}/join('',TEXT_YELLOW,$elite[0]{LENGTH},TEXT_NORMAL)/e unless exists $elite[0]{NEW};

  $formatElite = join ' ',$formatRatio,$formatElite;

  if($spacer)
  {
    print $spacer;

    $spacer = '';
  }
  unless($done)
  {
    printf "%2s%8s: Used uniform PDF %d times...\n\n",OOPS,' ',$oops if $oops;
    printf "%2s%8d: %s %4s %s\n",
      $newBest,
      $itr+1,
      $formatElite,
      '....',
      join(' ',map { sprintf '%4d',$ants[$_]{LENGTH} } -10 .. -1);

    my $out = join ',',
      $itr,
      (map { $elite[$_]{LENGTH} } 0 .. MAX_ELITE-1),
      (map { join '',@{$elite[$_]{ROUTE}} } 0 .. MAX_ELITE-1);

    PrintRoutes(\@newElite);
    $of->put(contents => $out);
  }
  else
  {
    say sprintf '%10d: %d',$itr,$ant->{LENGTH};

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

# τ′ = φ⋅(1 - ⍴)/L + ⍴⋅τ(ij);
#
sub DoLocalPheremoneUpdate
{
  my $route = shift;
  my $L     = shift;

  for(@cityIdx)
  {
    $pheremone{$route->[$_]}{$route->[$_+1]} = 1000*(1 - EVAPORATION_RATE)/$L + EVAPORATION_RATE*$pheremone{$route->[$_]}{$route->[$_+1]};
  }
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
    my $pheremone    = $pheremone{$city};
    my @destinations = grep { !exists $route{$_} } keys %{$pheremone};

    # check if only one possible destination
    #
    if(@destinations == 1)
    {
      $city = $destinations[0];

      push @{$route},$city;

      $route{$city} = 1;

      next;
    }
    my $probabilityDistribution;

    unless($bored && irand @cities == 0)
    {
      my $heuristic = $heuristic{$city};

      $probabilityDistribution = $probabilityDistribution{$city};

      # find all the cities we haven't been too already
      #

      # P(ij) = τ(ij)**α⋅η(ij)**β/𝜮(τ**α⋅η**β)
      #
      my %tau_x_beta     = map { $_ => $pheremone->{$_}**PHEREMONE_EXPONENT*$heuristic->{$_}**HEURISTIC_EXPONENT } @destinations;
      my $sum_tau_x_beta = sum @tau_x_beta{@destinations};

      my @normalization  = map { max(int(1000*($tau_x_beta{$_}/$sum_tau_x_beta) + 0.5),1) } @destinations;

      @{$probabilityDistribution} = map { ($destinations[$_]) x $normalization[$_] } 0 .. $#destinations;
    }
    else
    {
      @{$probabilityDistribution} = @destinations[0 .. $#destinations];

      $oops++;
    }
    my $idx = irand @{$probabilityDistribution};

    $city = $probabilityDistribution->[$idx];

    push @{$route},$city;

    $route{$city} = 1;
  }
  $ant->{LENGTH} = sum map { $distance{$route->[$_]}{$route->[$_+1]} } @cityIdx;
  $ant->{TEXT}   = join '',@{$route};

  DoLocalPheremoneUpdate($ant->{ROUTE},$ant->{LENGTH});
}

# Construct a Solution (route and length) for ants from startIndex to POPULATION_SIZE
#
sub ConstructAntSolutions
{
  my $iteration  = shift;

  $oops = 0;

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

# Heuristic between A and B = (1/distance{A}{B}**INITIAL_EXPONENT)/sum(distance{A}{B}, distance{A}{C}, ...)
#
sub InitializeHeuristic
{
  foreach my $t (@cities)
  {
    my @keys = keys %{$distance{$t}};
    my @rcp2 = map { 1/$distance{$t}{$_}**INITIAL_EXPONENT } @keys;
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

  say "\n",'GOAL is: ',TEXT_YELLOW,$nearestNeighbor[3],TEXT_NORMAL,"\n";

  return $nearestNeighbor[3];
}
