#!/usr/bin/perl -w+-    00.56-0

use strict;

sub getSubsets
{
  my ($list,$subsets) = shift;

  my $size = scalar @{$list};
  my $frmt = join '','%0',$size,'b';
  my @mask = map { [split '',$_ ] } map { sprintf $frmt,$_ } 0 .. (2**$size)-1;

  for(@mask)
  {
    my @flag = @{$_};

    my $sub = [map { $flag[$_] == 1 ? $list[$_] : () } 0 .. $#list];
    my $rem = [map { $flag[$_] == 0 ? $list[$_] : () } 0 .. $#list];

    my $r = {SUB => $sub,
             REM => $opp,
            };

    push @{$subsets},$r;
  }
}
