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

for(@{$io->contents})
{
  my ($t1,$t2,$d) = split ',';

  my $key = "$t1$t2";

  $distance{$key} = $d;
}
my $town = shift @towns;

my @route = ($town);
my %route = ($town => 1);

for(1 .. @towns)
{
  my @next = sort { $distance{$a} <=> $distance{$b} } grep { substr($_,0,1) eq $town } keys %distance;

  my $t = substr(shift @next,1,1);

  while(exists $route{$t})
  {
    $t = substr(shift @next,1,1);
  }
  push @route,$t;

  $route{$t} = 1;

  $town = $t;
}
my @distance = (map { $distance{"$route[$_]$route[$_+1]"} } 0 .. $#route-1,-1);

say 'CITY:  ',join ' ',map { sprintf '%4s',$_ } @route,'A';
say 'MILES: ',join ' ','    ',map { sprintf '%4d',$_ } @distance;

my $t = '    ';
my $d = 0;

for(0 .. $#route-1,-1)
{
  $d += $distance{"$route[$_]$route[$_+1]"};

  $t = join(' ',$t,sprintf '%4d',$d);
}
say 'TOTAL: ',$t;
say sum(@distance);
