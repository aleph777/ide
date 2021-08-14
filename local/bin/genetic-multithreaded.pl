#!/usr/bin/perl -w    # -*-Perl-*-

#         Copyright © 2021-2021 Tom Fontaine

# Title:  stuff.pl
# Date:   21-Feb-2021

# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software",
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# Except as contained in this notice, the name(s of the above copyright
# holders shall not be used in advertising or otherwise to promote the sale,
# use or other dealings in this Software without prior written authorization.

# The software is provided "As Is", without warranty of any kind, express or
# implied, including but not limited to the warranties of merchantability,
# fitness for a particular purpose and noninfringement. In no event shall
# the authors or copyright holders be liable for any claim, damages or other
# liability, whether in an action of contract, tort or otherwise, arising
# from, out of or in connection with the software or the use or other
# dealings in the software.

#
# Purpose:
#
# Arguments:
#
# Revision:
#

use File::IO;
use List::Util qw(max min sum);
use Math::Random::Secure qw(irand rand);
use threads;
use threads::shared;
use strict;
use v5.10;

use constant _ME_ => $0 =~ m=([^/]+)$=;

my $nproc;
my @genes;

BEGIN
{
  $nproc = qx(nproc);

  chomp $nproc;

  @genes = 'A' .. 'Z';
}


use constant COST_FORMAT         => '%5d';
use constant MAXIMUM_GENERATIONS => 10000;
use constant MINIMUM_DIFFERENCE  => 0;
use constant POPULATION_SCALE    => 12;
use constant POPULATION_SIZE     => POPULATION_SCALE*(@genes - 1);
# use constant THREAD_COUNT        => $nproc;
use constant THREAD_COUNT        => 2;

# Global variables
#
my %distance;
my @population     : shared;
my @nextGeneration : shared;

my $format = join ' ',(('%s') x @genes),'--',(COST_FORMAT) x 10;

my @matingPD   = (0 .. POPULATION_SIZE-1); # uniform PDF for mating selections
my @routeIndex = (0 .. $#genes-1,-1);      # steps 0 to 24 and back to 0

my $populationSize = THREAD_COUNT*int(POPULATION_SIZE/THREAD_COUNT + 0.5) + 200;
my $workAmount     = $populationSize/THREAD_COUNT;

for(1 .. $populationSize)
{
  my %p      :shared = (COST => undef,ROUTE => undef);
  my %n      :shared = (COST => undef,ROUTE => undef);
  my @routeP :shared;
  my @routeN :shared;

  $p{ROUTE} = \@routeP;
  $n{ROUTE} = \@routeN;

  push @population,\%p;
  push @nextGeneration,\%n;
}

sub getDistanceData
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

sub getNextGeneration
{
  my ($populationIndex,$workAmount) = @_;

  for my $i ($populationIndex .. $populationIndex+$workAmount-1)
  {
    # choose parents other than self
    #
    my $a = $i;
    my $b = $i;
    my $p = $population[$i];
    my $n = $nextGeneration[$i];

    $a = $matingPD[irand @matingPD] until $a != $i;
    $b = $matingPD[irand @matingPD] until $b != $i && $b != $a;

    # get route and cost of offspring vector
    #
    my @route = mate($population[$a]{ROUTE},$population[$b]{ROUTE});
    my $cost  = sum(map { $distance{$route[$_]}{$route[$_+1]} } @routeIndex);

    if($cost < $p->{COST})
    {
      # replace individual with offspring
      #
      @{$n->{ROUTE}} = @route;
      $n->{COST}     = $cost;
    }
    else
    {
      @{$n->{ROUTE}} = @{$p->{ROUTE}};
      $n->{COST}     = $p->{COST};
    }
  }
}

sub getUnused
{
  my ($unused,$distance) = @_;

  my @unused = keys %{$unused};

  return $unused[0] if @unused == 1;

  my $s = sum(map { $distance->{$_} } @unused);

  my @d = map { ($_) x int(100*$distance->{$_}/$s) } @unused;

  return $d[irand(@d)];
}

sub initializePopulation
{
  my $populationSize = shift;

  foreach my $p (@population[0 .. $populationSize-1])
  {
    my @tmp = @genes;

    push @{$p->{ROUTE}},shift @tmp;
    push @{$p->{ROUTE}},splice @tmp,irand(@tmp),1 for 1 .. @genes;

    $p->{COST}  = sum(map { $distance{$p->{ROUTE}[$_]}{$p->{ROUTE}[$_+1]} } @routeIndex);
  }
  @population = sort { $a->{COST} <=> $b->{COST} } @population;
}

sub mate
{
  my ($r1,$r2) = @_;

  my @path;
  my %path;

  my @r1 = @{$r1};
  my @r2 = @{$r2};
  my $m  = @{$r2}/2;

  if(@r1 != @genes) {say "R1(before): @r1";exit;}
  if(@r2 != @genes) {say "R2(before): @r2";exit;}

  my $next;

  my %unused;

  for(1 .. @genes)
  {
    my $nextR1 = shift @r1;
    my $nextR2 = shift @r2;

    if(exists $path{$nextR1} && exists $path{$nextR2})
    {
      $m--;

      $next = getUnused(\%unused,$distance{$path[-1]});
    }
    elsif(exists $path{$nextR1})
    {
      $next = $nextR2;
    }
    elsif(exists $path{$nextR2})
    {
      $next = $nextR1;
    }
    elsif($nextR1 eq $nextR2)
    {
      $next = $nextR1;
    }
    else
    {
      my $d1 = $distance{$path[-1]}{$nextR1};
      my $d2 = $distance{$path[-1]}{$nextR2};

      if(rand > $d1/($d1 + $d2))
      {
        $next = $nextR1;

        $unused{$nextR2} = 1;
      }
      else
      {
        $next = $nextR2;

        $unused{$nextR1} = 1;
      }
      $m--;
    }
    push @path,$next;                               # add next to path genome

    $path{$next} = 1;                               # add next to path dictionary

    delete $unused{$next} if exists $unused{$next}; # remove next from unused dictionary
  }
  if(@path != @genes)
  {
    say "R1[@r1]: @{$r1}";
    say "R2[@r2]: @{$r2}";
    say "MATE: @path";
    exit;
  }
  # mutate
  #
  my $idx = irand($#path) + 1;
  my $mxs = $#path - $idx + 1;

  $m = $mxs if $m > $mxs;

  my $len = 1+irand(max($m,2));
  my @rem = splice @path,$idx,$len;

  my $idx1 = $idx;

  $idx1 = irand(@path) + 1 until $idx1 != $idx;

  splice @path,$idx1,0,@rem;

  die "$idx,$len,[@rem],$idx1 | @path" unless @path == @genes;

  return @path;
}

my $path = @ARGV ? shift @ARGV : "$ENV{HOME}/Documents/Personal/tsp/map4.distances.csv";

die "_ME_: $path does not exist!!!\n" unless -e $path;
die "_ME_: $path is a directory!!!\n" if     -d $path;

getDistanceData($path);
initializePopulation($populationSize);

my $savedText = '';
my $newBest   = '';
my $best      = $population[0]{COST};

for(1 .. MAXIMUM_GENERATIONS)
{
  my @threads = map { threads->create(\&getNextGeneration,$_*$workAmount,$workAmount) } 0 .. THREAD_COUNT-1;

  $_->join() for @threads;

  for(0 .. $#population)
  {
    my $p = $population[$_];
    my $n = $nextGeneration[$_];

    @{$p->{ROUTE}} = @{$n->{ROUTE}};
    $p->{COST}     = $n->{COST};
  }
  @population = sort { $a->{COST} <=> $b->{COST} } @population;

  my $text1 = sprintf '%5d. ',$_;
  my $text2 = sprintf $format,@{$population[0]{ROUTE}},map { $population[$_]{COST} } (0 .. 9);

  if($population[0]{COST} < $best)
  {
    $newBest = ' 😎';
    $best    = $population[0]{COST};
  }
  else
  {
    $newBest = '';
  }
  say $text1,' ',$text2,$newBest unless $savedText eq $text2;
  say $text1 if ($_ % 100 == 0) && $savedText eq $text2;

  $savedText = $text2;

  last if ($population[9]{COST} - $population[0]{COST}) <= MINIMUM_DIFFERENCE;
}

# my @shared :shared;

# for(1 .. 10)
# {
#   my %shared :shared = (COST => 0);
#   my @sh :shared;

#   $shared{ROUTE} = \@sh;

#   push @shared,\%shared;
# }
# my $t1 = threads->create(\&work,2);
# my $t2 = threads->create(\&work,3);

# $t1->join();
# $t2->join();

# say $shared[2]{COST};
# say $shared[3]{COST};

# sub work
# {
#   my $i = shift;

#   $shared[$i]{COST} = $i*10;
# }
