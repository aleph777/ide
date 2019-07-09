#!/usr/bin/perl -w    # -*-Perl-*-

use Math::Random::Secure qw(irand);
use File::IO;
use strict;
use v5.10;

my @towns = 'A' .. 'Z';

my %distance;
my %grid;
my %town;

my $max_x = defined $ARGV[0] ? $ARGV[0] : 0.9*1920;
my $max_y = defined $ARGV[1] ? $ARGV[1] : 0.9*1080;

my $mapTowns;
my $mapDistances;

my $N = 0;

do
{
  $mapTowns     = join '','map',++$N,'.towns.csv';
  $mapDistances = join '','map',$N,'.distances.csv';

} until !(-e $mapDistances || -e $mapTowns);

for(@towns)
{
  my ($x,$y) = (irand($max_x),irand($max_y));

  my $key = "$x,$y";

  redo if exists $grid{$key};

  $grid{$key} = 1;
  $town{$_}   = [($x,$y)];
}
my $io1 = File::IO->new(newline => 1,contents => [map {join ',',$_,@{$town{$_}}} @towns]);

$io1->put(path => $mapTowns);

for my $i (0 .. $#towns)
{
  my $ti = $town{$towns[$i]};

  for my $j ($i .. $#towns)
  {
    next if $i == $j;

    my $tj = $town{$towns[$j]};

    my $key1 = "$towns[$i]_$towns[$j]";
    my $key2 = "$towns[$j]_$towns[$i]";

    $distance{$key1} = $distance{$key2} = int(sqrt(($ti->[0] - $tj->[0])**2 + ($ti->[1] - $tj->[1])**2) + 0.5);
  }
}
my $io2 = File::IO->new(newline => 1);

@{$io2->contents} = map { join ',',split('_',$_),$distance{$_} } sort keys %distance;

$io2->put(path => $mapDistances);

say "generated $mapTowns ($max_x,$max_y)";
