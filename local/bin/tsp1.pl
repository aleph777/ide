#!/usr/bin/perl -w    # -*-Perl-*-

use File::IO;
use List::Util qw(sum);
use strict;

local $| = 1;

my @towns = 'A' .. 'Z';

my %distance1;
my %distance2;

my $io = File::IO->new(chomp => 1);

$io->get(path => $ARGV[0]);

for(@{$io->contents})
{
  my ($t1,$t2,$d) = split ',';

  my $key = "$t1$t2";

  $distance1{$key} = $d;
}
for(keys %distance1)
{
  my $d1 = substr($_,0,1);
  my $d2 = substr($_,1,1);

  for(grep { $_ ne $d1 && $_ ne $d2 } @towns)
  {
    $distance2{join('',$d1,$d2,$_)} = $distance1{join('',$d1,$d2)} + $distance1{join('',$d2,$_)};
  }
}
my $town = shift @towns;

my @route = ($town);
my %route = ($town => 1);

for(1 .. @towns)
{
  my @next = sort { $distance2{$a} <=> $distance2{$b} } grep { substr($_,0,1) eq $town } keys %distance2;

  my $t = substr(shift @next,1,1);

  while(exists $route{$t})
  {
    $t = substr(shift @next,1,1);
  }
  push @route,$t;

  $route{$t} = 1;

  $town = $t;
}
my @distance = (map { $distance1{"$route[$_]$route[$_+1]"} } 0 .. $#route-1,-1);

print join(' ',map { sprintf '%4s',$_ } @route),"\n";
print join(' ','    ',map { sprintf '%4d',$_ } @distance),"\n";

my $t = '    ';
my $d = 0;

for(0 .. $#route-1,-1)
{
  $d += $distance1{"$route[$_]$route[$_+1]"};

  $t = join(' ',$t,sprintf '%4d',$d);
}
print "$t\n";
print sum(@distance),"\n";

