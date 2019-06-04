# Util::GetOpt --- Provides command line option handling -*-Perl-*-

#         Copyright © 2012-2019 Tom Fontaine

# Author: Tom Fontaine
# Date:   25-Sep-2012

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
# Revision:
#
package Util::GetOpt;

require 5.006;
use Carp;
use strict;

# use constant FOO => 'BAR';

our $AUTOLOAD;

my @AREF = qw(args);
my @HREF = qw(option);

my %fields = (args           => undef,
              option         => undef,
              switchesGroup  => undef,
              delimiterGroup => '--?',
              switchesArg    => undef,
              switchesBare   => undef,
             );

my $__ME__ = join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

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

  my $args     = exists $parm{args}           ? $parm{args}           : $this->{args};
  my $option   = exists $parm{option}         ? $parm{option}         : $this->{option};
  my $swGroup  = exists $parm{switchesGroup}  ? $parm{switchesGroup}  : $this->{switchesGroup};
  my $delim    = exists $parm{delimiterGroup} ? $parm{delimiterGroup} : $this->{delimiterGroup};
  my $swArg    = exists $parm{switchesArg}    ? $parm{switchesArg}    : $this->{switchesArg};
  my $swBare   = exists $parm{switchesBare}   ? $parm{switchesBare}   : $this->{switchesBare};

  my $reDelimiterGroup = ref $delim  eq 'ARRAY' ? join('|',@{$delim})  : $delim;
  my $reSwitchesBare   = ref $swBare eq 'ARRAY' ? join('|',@{$swBare}) : $swBare;
  my $reSwitchesArg    = ref $swArg  eq 'ARRAY' ? join('|',@{$swArg})  : $swArg;

  my @tmp;

  while(@{$args})
  {
    my $tmp = shift @{$args};

    if(defined $swGroup && $tmp =~ /^($reDelimiterGroup)([$swGroup]+)$/o)
    {
      @{$option}{map { "$1$_" } split //,$2} = (1) x length($2);
    }
    elsif(defined $swBare && $tmp =~ /^($reSwitchesBare)$/o)
    {
      $option->{$1} = 1;
    }
    elsif(defined $swArg && $tmp =~ /^($reSwitchesArg)$/o)
    {
      $option->{$1} = shift @{$args};
    }
    elsif(defined $swArg && $tmp =~ /^($reSwitchesArg)(.+)$/o)
    {
      $option->{$1} = $2;
    }
    else
    {
      push @tmp,$tmp;
    }
    @{$this}{keys %{$option}} = values %{$option};
  }
  @{$args} = @tmp;
}

1;
