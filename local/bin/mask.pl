#!/usr/bin/perl -w

use strict;

my @list = qw(a b c d);

my $fmt = join '','%0',scalar(@list),'b';

my @subsets;

my @mask = map { [split '',$_ ] } map { sprintf $fmt,$_ } 0 .. (2**@list)-1;

for(@mask)
{
  my @flag = @{$_};

  print '(',join(',',map { $flag[$_] ? $list[$_] : () } 0 .. $#list),")\n";
}
