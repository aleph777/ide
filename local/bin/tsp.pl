#!/usr/bin/perl -w    # -*-Perl-*-

use Math::Random::Secure;
use List::Util qw(sum);
use strict;
#
# Assumes that graph is fully connected
#
my @towns = 'A' .. 'Z';

my $min = 99999999;

my %route;
my %trip;

for my $i (0 .. $#towns)
{
  for my $j ($i .. $#towns)
  {
    next if $i == $j;

    $route{$towns[$i]}{$towns[$j]} = $route{$towns[$j]}{$towns[$i]} = int(rand 100) + 1;
  }
}
for(1 .. 25)
{
  my @trip = pickTrip(\@towns);
  my $len  = sum map { $route{$trip[$_]}{$trip[$_+1]} } 0 .. $#trip-1;

  $min = $len if $len < $min;

  print "@trip: $len\n";
}
print "===> $min\n";
sub pickTrip
{
  my ($towns) = @_;

  my $i = 0;

  while(1)
  {
    my @tmp = @{$towns};

    my @trip;

    push @trip,splice @tmp,int(rand @tmp),1 while @tmp;

    my $key = join '',@trip;

    unless(exists $trip{$key})
    {

      $trip{$key} = 1;

      return @trip;
    }
  }
}
# sub pickTrip
# {
#   my ($towns) = @_;

#   my @tmp = @{$towns};

#   while(1)
#   {
#     my @trip;

#     print "@tmp\n";

#     push @trip,splice @tmp,int(rand @tmp),1 while @tmp;

#     my $key = join '',@trip;print "$key\n";

#     unless(exists $trip{$key})
#     {
#       print map { join(',',$_,$_+1,$trip[$_],$trip[$_+1],$route{$trip[$_]}{$trip[$_+1]}),"\n" } 0 .. $#trip-1;
#       my $len = sum map { $route{$trip[$_]}{$trip[$_+1]} } 0 .. $#trip-1;

#       $trip{$key} = $len;

#       print "$key: $trip{$key}\n";

#       if($len < $min)
#       {
#         $min = $len;

#         return @trip;
#       }
#     }
#     @tmp  = @{$towns};sleep 1;
#   }
# }
