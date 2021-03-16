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
my @population1 : shared;
my @population2 : shared;

my %distance;

BEGIN
{
  $nproc = qx(nproc);

  chomp $nproc;

  @genes = 'A' .. 'Z';
}

use constant COST_FORMAT         => '%5d';
use constant MAXIMUM_GENERATIONS => 10000;
use constant MINIMUM_DIFFERENCE  => 0;
use constant POPULATION_SCALE    => 20;
use constant POPULATION_SIZE     => POPULATION_SCALE*(@genes - 1);
use constant THREAD_COUNT        => $nproc;

sub foo
{
  say irand(100);
}

sub bar
{
  say irand(100);
}

my $format = join ' ',(('%s') x @genes),'--',(COST_FORMAT) x 11;

my @matingPD   = (0 .. POPULATION_SIZE-1); # uniform PDF for mating selections
my @routeIndex = (0 .. $#genes-1,-1);      # steps 0 to 24 and back to 0

my $workAmount     = int(POPULATION_SIZE/THREAD_COUNT + 0.5);
my $populationSize = THREAD_COUNT*$workAmount;

for(1 .. $populationSize)
{
  my @r1 :shared;
  my @r2 :shared;

  my %p1 :shared = (COST => undef,ROUTE => \@r1);
  my %p2 :shared = (COST => undef,ROUTE => \@r2);

  push @population1,\%p1;
  push @population2,\%p2;
}

sub getDistances
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

sub initializePopulation
{
  my ($startIndex,$workAmount) = @_;

  foreach my $p (map { $population1[$_] } $startIndex .. $startIndex+$workAmount-1)
  {
    my @tmp = @genes;

    push @{$p->{ROUTE}},shift @tmp;
    push @{$p->{ROUTE}},splice @tmp,irand(@tmp),1 for 1 .. @tmp;

    $p->{COST} = sum(map { $distance{$p->{ROUTE}[$_]}{$p->{ROUTE}[$_+1]} } @routeIndex);
  }
  # @population1 = sort { $a->{COST} <=> $b->{COST} } @population1;
}

sub getUnused
{
  my ($unused,$distance) = @_;

  my @unused = keys %{$unused};

  return $unused[0] if @unused == 1;

  my $s = sum(map { $distance->{$_} } @unused);

  # my @d = map { ($_) x int(100*$distance->{$_}/$s) } @unused;
  my @d = map { ($_) x int(100*(1.0 - $distance->{$_}/$s)) } @unused;

  return $d[irand(@d)];
}

sub mate
{
  my ($parent1,$parent2) = @_;

  my @route;
  my %route;

  # copy parent genes
  #
  my @parent1 = @{$parent1};
  my @parent2 = @{$parent2};

  my $mutationLimit = @parent1/2;

  my $next;
  my %unused;

  for(1 .. @genes)
  {
    my $nextR1 = shift @parent1;
    my $nextR2 = shift @parent2;

    if(exists $route{$nextR1} && exists $route{$nextR2})
    {
      $mutationLimit--;

      $next = getUnused(\%unused,$distance{$route[-1]});
    }
    elsif(exists $route{$nextR1})
    {
      $next = $nextR2;
    }
    elsif(exists $route{$nextR2})
    {
      $next = $nextR1;
    }
    elsif($nextR1 eq $nextR2)
    {
      $next = $nextR1;
    }
    else
    {
      my $d1 = $distance{$route[-1]}{$nextR1};
      my $d2 = $distance{$route[-1]}{$nextR2};

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
      $mutationLimit--;
    }
    push @route,$next;                              # add next to path genome

    $route{$next} = 1;                              # add next to path dictionary

    delete $unused{$next} if exists $unused{$next}; # remove next from unused dictionary
  }
  # mutation
  #
  my $mutationLength = irand(max($mutationLimit,2)) + 1;
  my $mutationIndex  = min(irand($#route)+1,@route-$mutationLength);
  my @mutation       = splice @route,$mutationIndex,$mutationLength;

  splice @route,irand(@route) + 1,0,@mutation;

  if(grep { !defined } @route)
  {
    say join ' ','INDEX:',$mutationLimit,$mutationLength,$mutationIndex,scalar @route;exit;
  }
  elsif(@route == 0)
  {
    say 'ROUTE EMPTY!!!',": @mutation";exit;
  }
  return @route;
}

sub getSurvivors
{
  my ($prev,$next,$startIndex,$workAmount) = @_;

  for my $i ($startIndex .. $startIndex+$workAmount-1)
  {
    # choose parents other than self
    #
    my $a = $i;
    my $b = $i;
    my $p = $prev->[$i];
    my $n = $next->[$i];

    $a = $matingPD[irand @matingPD] until $a != $i;
    $b = $matingPD[irand @matingPD] until $b != $i && $b != $a;

    # get route and cost of offspring vector
    #
    my @route = mate($prev->[$a]{ROUTE},$prev->[$b]{ROUTE});

    if(grep { !exists $distance{$route[$_]}{$route[$_+1]} || !defined $distance{$route[$_]}{$route[$_+1]} }  @routeIndex)
    {
      say "@route";exit;
    }
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
      # copy individual to next generation
      #
      @{$n->{ROUTE}} = @{$p->{ROUTE}};
      $n->{COST}     = $p->{COST};
    }
  }
}

my $path = @ARGV ? shift @ARGV : "$ENV{HOME}/Documents/Personal/tsp/map4.distances.csv";

die "_ME_: $path does not exist!!!\n" unless -e $path;
die "_ME_: $path is a directory!!!\n" if     -d $path;

my $savedText = '';
my $newBest   = '';
my $stale     = 0;

getDistances($path);

my @threads;

for(1 .. THREAD_COUNT)
{
  my $thread = threads->create(\&initializePopulation,($_-1)*$workAmount,$workAmount);

  push @threads,$thread;
}
$threads[$_-1]->join() for 1 .. THREAD_COUNT;

my $best = 10**6;

my $time1 = time();

foreach my $generation (1 .. MAXIMUM_GENERATIONS)
{
  my ($prev,$next) = ($generation % 2 != 0) ? (\@population1,\@population2) : (\@population2,\@population1);

  my @threads;

  for(1 .. THREAD_COUNT)
  {
    my $thread = threads->create(\&getSurvivors,$prev,$next,($_-1)*$workAmount,$workAmount);

    push @threads,$thread;
  }
  $threads[$_-1]->join() for 1 .. THREAD_COUNT;

  @{$next} = sort { $a->{COST} <=> $b->{COST} } @{$next};

  my $text1 = sprintf '%5d. ',$generation;
  my $text2 = sprintf $format,@{$next->[0]{ROUTE}},map { $next->[$_]{COST} } (0 .. 9,-1);

  if($next->[0]{COST} < $best)
  {
    $newBest = ' 😎';
    $best    = $next->[0]{COST};
  }
  else
  {
    $newBest = '';
  }
  if($savedText eq $text2)
  {
    say '😐' if ++$stale % 100 == 0;
  }
  else
  {
    say $text1,' ',$text2,$newBest;

    $stale = 0;
  }
  last if ($next->[-1]{COST} - $next->[0]{COST}) <= MINIMUM_DIFFERENCE || $stale == 1000;

  $savedText = $text2;
}
my $duration = time() - $time1;
my $route    = $population1[0]{ROUTE};

say '';
say 'NODE:  ',join ' ',map { sprintf '%4s',$_ } @{$route},'A';
say 'MILES: ',join ' ',' ' x 4,map { sprintf '%4d',$distance{$route->[$_]}{$route->[$_+1]} } @routeIndex;

my $total = '   0';

my $d = 0;

for(@routeIndex)
{
  $d += $distance{$route->[$_]}{$route->[$_+1]};

  $total .= sprintf ' %4d',$d;
}
say 'TOTAL: ',$total,"\n";

my ($minutes,$seconds) = (int($duration/60),$duration % 60);

say "\nElapsed time: $minutes minutes, $seconds seconds";
