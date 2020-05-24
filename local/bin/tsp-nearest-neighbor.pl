#!/usr/bin/perl -w    # -*-Perl-*-

use File::IO;
use List::Util qw(sum);
use strict;
use v5.10;

local $| = 1;

my @towns = 'A' .. 'Z';

my %distance;

my $io = File::IO->new(chomp => 1);

$io->get(path => $ARGV[0]);

# Get the distances between the cities
#
for(@{$io->contents})
{
  my ($t1,$t2,$d) = split ',';

  my $key = "$t1$t2";

  $distance{$key} = $d;
}
# Start at 'A'
#
my $town = shift @towns;

my @route = ($town);
my %route = ($town => 1);

my %tmpDistance = %distance;

# Pick the closest neighbor yet to be visited
#
for(1 .. @towns)
{
  # sort the distances from the current town
  #
  my @fromTown = sort { $tmpDistance{$a} <=> $tmpDistance{$b} } grep /^$town/,keys %tmpDistance;
  my @toTown   = grep /$town$/,keys %tmpDistance;

  # remove the fromtown from the map
  #
  delete $tmpDistance{$_} for @fromTown,@toTown;

  # find the nearest neighbor
  #
  my $t = substr($fromTown[0],1,1);

  # add the new town to the route
  #
  push @route,$t;

  $route{$t} = 1;

  # new town is the next from town
  #
  $town = $t;
}
my @distance = (map { $distance{"$route[$_]$route[$_+1]"} } 0 .. $#route-1,-1);

say 'CITY:  ',join ' ',map { sprintf '%4s',$_ } @route,'A';
say 'MILES: ',join ' ','   -',map { sprintf '%4d',$_ } @distance;

my $t = '   0';
my $d = 0;

for(0 .. $#route-1,-1)
{
  $d += $distance{"$route[$_]$route[$_+1]"};

  $t = join(' ',$t,sprintf '%4d',$d);
}
say 'TOTAL: ',$t;
say sum(@distance);
say join ',',@route;


