#!/usr/bin/perl -w    # -*-Perl-*-

use Math::Random::Secure;
use File::IO;
use strict;

use constant MILES => 250;

my @towns = 'A' .. 'Z';

my %distance;
my %grid;
my %town;

for(@towns)
{
  my ($x,$y) = map { int(rand(MILES)) - MILES/2 } 1 .. 2;

  my $key = "$x,$y";

  redo if exists $grid{$key};

  $grid{$key} = 1;
  $town{$_}   = [($x,$y)];
}

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
my $io = File::IO->new(newline => 1);

@{$io->contents} = map { join ',',split('_',$_),$distance{$_} } sort keys %distance;

$io->put(path => $ARGV[0]);
