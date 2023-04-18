#!/usr/bin/perl -w    # -*-Perl-*-

#         Copyright © 2021-2023 Tom Fontaine

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
use List::Util qw(sum);
use Math::GeneticRouteMultiThreaded;
use Math::Random::Secure qw(irand rand);
use threads;
use threads::shared;
use strict;
use v5.10;

use constant _ME_ => $0 =~ m=([^/]+)$=;

# sub thrSub
# {
#   my ($minimum) = @_;

#   return map { $minimum + $_ } (1 .. 5);
# }
# my @threads = map { threads->create(\&thrSub,10**$_) } 1 .. 4;

# my @data;

# push @data,map { $_->join() } @threads;

# say "@data";

# say 'Good bye';
my @shared :shared;

for(1 .. 10)
{
  my %shared :shared = (COST => 0);
  my @sh :shared;

  $shared{ROUTE} = \@sh;

  push @shared,\%shared;
}
my $t1 = threads->create(\&work,2);
my $t2 = threads->create(\&work,3);

$t1->join();
$t2->join();

say $shared[2]{COST};
say $shared[3]{COST};
say @{$shared[2]{ROUTE}};

sub work
{
  my ($i) = @_;

  $shared[$i]{COST} = $i*10;

  push @{$shared[$i]{ROUTE}},$i .. $i+10;
}

exit;
my %distance;

my @traits = 'A' .. 'Z';

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

my $path = @ARGV ? shift @ARGV : "$ENV{HOME}/Documents/Personal/tsp/map4.distances.csv";

die "_ME_: $path does not exist!!!\n" unless -e $path;
die "_ME_: $path is a directory!!!\n" if     -d $path;

# my $mg = Math::GeneticRouteMultiThreaded->new(costFunction => \&computeCost,traits => \@traits,generations => 9999,scalePopulationSize => 10);
my $mg = Math::GeneticRouteMultiThreaded->new(traits => \@traits,generations => 9999,scalePopulationSize => 10);

getDistanceData($path);

$mg->setDistances(\%distance);
$mg->solve();
