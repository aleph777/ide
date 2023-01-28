# Util::Options --- Provides heavy-duty (legacy) command line option handling -*-Perl-*-

#         Copyright © 2000-2023 Tom Fontaine

# Author: Tom Fontaine
# Date:   24-Feb-2000

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
# Revision: 26-Jun-2000 ArgSwitches can optionally use spaces between switch and value
#           02-Aug-2000 Now using two passes, added group switches
#           12-May-2008 Moved to Util::Options
#                     use strict
#
package Util::Options;

require 5.006;
use Carp;
use strict;

# use Foo::Bar;

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

our $AUTOLOAD;

my @AREF = qw(ARGS);
my @HREF = qw(Options);

my %fields = (ARGS          => undef,
              Options       => undef,
              Switches      => undef,
              GroupSwitches => undef,
              ArgSwitches   => undef,
              Delimeters    => '--?',
              UseDelimeters => undef,
             );

# BEGIN
# {
# }

# END
# {
# }

sub new
{
  my $proto = shift;
  my $class = ref($proto) || $proto;
  my $this  = {_permitted => \%fields,%fields,};

  bless  $this,$class;

  @{$this}{@AREF,@HREF}= ((map { [] } @AREF),(map { {} } @HREF));

  my %parm = @_;

  @{$this}{keys %parm} = values %parm;

  return $this;
}

sub AUTOLOAD
{
  my $this = shift;
  my $type = ref($this) or croak "$this is not an object";
  my $name = $AUTOLOAD;

  $name =~ s/.*://;

  return if $name eq "DESTROY";

  croak "Can't access `$name' field in class $type" unless exists $this->{'_permitted'}->{$name};

  return @_ ? $this->{$name} = shift : $this->{$name};
}

sub configure
{
  my $this = shift;
  my %parm = @_;

  @{$this}{keys %parm} = values %parm;
}
sub get
{
  my $this = shift;
  my %parm = @_;

  my $args = exists $parm{ARGS}          ? $parm{ARGS}          : $this->{ARGS};
  my $opt  = exists $parm{Options}       ? $parm{Options}       : $this->{Options};
  my $swi  = exists $parm{Switches}      ? $parm{Switches}      : $this->{Switches};
  my $grp  = exists $parm{GroupSwitches} ? $parm{GroupSwitches} : $this->{GroupSwitches};
  my $asw  = exists $parm{ArgSwitches}   ? $parm{ArgSwitches}   : $this->{ArgSwitches};
  my $del  = exists $parm{Delimeters}    ? $parm{Delimeters}    : $this->{Delimeters};
  my $used = exists $parm{UseDelimeters} ? $parm{UseDelimeters} : $this->{UseDelimeters};

  my $delregexp = join '','(',ref $del ? join('|',@{$del}) : $del,')';

  my $swiregexp = defined $swi ? join '','(',ref $swi ? join('|',@{$swi}) : $swi,')$' : undef;
  my $aswregexp = defined $asw ? join '','(',ref $asw ? join('|',@{$asw}) : $asw,')'  : undef;

  my $grpregexp = defined $grp ? join '','([',$grp,']+)$' : undef;

  my ($delim,$key,@x);

  my @p = map { {DELIM => undef,TYPE => undef,SWITCH => undef,VALUE => undef} } (0 .. $#{$args});

  for(0 .. $#{$args})
  {
    next unless $args->[$_] =~ /^$delregexp/;

    $p[$_]{DELIM} = $delim = $1;

    if(defined $swiregexp && $args->[$_] =~ /^$delim$swiregexp/)
    {
      $p[$_]{TYPE}   = 'BARE';
      $p[$_]{SWITCH} = $1;
    }
    elsif(defined $aswregexp && $args->[$_] =~ /^$delim$aswregexp/)
    {
      $p[$_]{TYPE}  = 'ARG';
      $p[$_]{SWITCH} = $1;
      $p[$_]{VALUE}  = substr $args->[$_],length($delim)+length($1);
    }
    elsif(defined $grpregexp && $args->[$_] =~ /^$delim$grpregexp/)
    {
      $p[$_]{TYPE}  = 'GROUP';
      $p[$_]{VALUE} = $1;
    }
  }
  for(0 .. $#{$args})
  {
    next unless defined $p[$_]{TYPE};

    if($p[$_]{TYPE} eq 'BARE')
    {
      $key = defined $used && $used ? join '',$p[$_]{DELIM},$p[$_]{SWITCH} : $p[$_]{SWITCH};

      $opt->{$key} = 1;

      unshift @x,$_;
    }
    elsif($p[$_]{TYPE} eq 'GROUP')
    {
      my @key = split '',$p[$_]{VALUE};

      $delim = $p[$_]{DELIM};

      @{$opt}{map { defined $used && $used ? join '',$delim,$_ : $_ } @key} = (1) x @key;

      unshift @x,$_;
    }
    elsif($p[$_]{TYPE} eq 'ARG')
    {
      $key = defined $used && $used ? join '',$p[$_]{DELIM},$p[$_]{SWITCH} : $p[$_]{SWITCH};

      $opt->{$key} = $p[$_]{VALUE};

      if($p[$_]{VALUE})
      {
        unshift @x,$_;
      }
      elsif($_ < $#{$args} && !defined $p[$_+1]{TYPE})
      {
        $opt->{$key} = $args->[$_+1];

        unshift @x,$_+1,$_;
      }
    }
  }
  for(@x) { splice @{$args},$_,1; }
}

1;
