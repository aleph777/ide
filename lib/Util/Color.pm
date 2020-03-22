# Util::Color --- [description] -*-Perl-*-

#         Copyright © 2017-2020 Tom Fontaine

# Author: Tom Fontaine
# Date:   07-Dec-2017

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

package Util::Color;

require 5.008;
use Carp;
use strict;

use Math::Random::Secure;
use List::Util qw(min max);

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

our $AUTOLOAD;

my @AREF = qw();
my @HREF = qw();

my %fields = (contents => undef,

              red   => undef,
              green => undef,
              blue  => undef,

              red_min   => 0,
              red_max   => 0,
              green_min => 0,
              green_max => 0,
              blue_min  => 0,
              blue_max  => 0,

              scheme => undef,
              step   => 1,

              _format => '#%02x%02x%02x',
             );

my $saved_scheme = 'NONE';

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

  bless $this,$class;

  @{$this}{@AREF} = map { [] } @AREF;
  @{$this}{@HREF} = map { {} } @HREF;

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

  croak "Can't access `$name' field in class $type" unless exists $this->{_permitted}->{$name};

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

  my $red   = exists $parm{red}     ? $parm{red}     : $this->{red};
  my $green = exists $parm{green}   ? $parm{green}   : $this->{green};
  my $blue  = exists $parm{blue}    ? $parm{blue}    : $this->{blue};
  my $fmt   = exists $parm{_format} ? $parm{_format} : $this->{_format};

  $red   = 0 unless defined $red;
  $green = 0 unless defined $green;
  $blue  = 0 unless defined $blue;

  $red   = min(max($red, 0), 255);
  $green = min(max($green, 0), 255);
  $blue  = min(max($blue, 0), 255);

  $this->{contents} = sprintf $fmt,$red,$green,$blue;
}

sub invert
{
  my $this = shift;

  $this->get(red => (255 - $this->{red}), green => (255 - $this->{green}), blue => (255 - $this->{blue}));
}

sub random
{
  my $this = shift;
  my %parm = @_;

  my $scheme    = exists $parm{scheme}    ? $parm{scheme}    : $this->{scheme};

  if(defined $scheme)
  {
    $this->setScheme($scheme);
  }
  my $red_min   = exists $parm{red_min}   ? $parm{red_min}   : $this->{red_min};
  my $red_max   = exists $parm{red_max}   ? $parm{red_max}   : $this->{red_max};
  my $green_min = exists $parm{green_min} ? $parm{green_min} : $this->{green_min};
  my $green_max = exists $parm{green_max} ? $parm{green_max} : $this->{green_max};
  my $blue_min  = exists $parm{blue_min}  ? $parm{blue_min}  : $this->{blue_min};
  my $blue_max  = exists $parm{blue_max}  ? $parm{blue_max}  : $this->{blue_max};
  my $step      = exists $parm{step}      ? $parm{step}      : $this->{step};

  $this->rndRed(red_min => $red_min, red_max => $red_max, step => $step);
  $this->rndGreen(green_min => $green_min, green_max => $green_max, step => $step);
  $this->rndBlue(blue_min => $blue_min, blue_max => $blue_max, step => $step);

  $this->get();
}

sub setScheme
{
  my $this   = shift;
  my $scheme = shift;

  return if !defined $scheme || $scheme eq $saved_scheme;

  if($scheme eq 'LIGHT')
  {
    $this->{red_min}   = 160;
    $this->{red_max}   = 248;
    $this->{green_min} = 160;
    $this->{green_max} = 248;
    $this->{blue_min}  = 160;
    $this->{blue_max}  = 248;

  }
  elsif($scheme eq 'DARK')
  {
    $this->{red_min}   = 16;
    $this->{red_max}   = 104;
    $this->{green_min} = 16;
    $this->{green_max} = 104;
    $this->{blue_min}  = 16;
    $this->{blue_max}  = 104;
  }
  elsif($scheme eq 'MEDIUM')
  {
    $this->{red_min}   = 64;
    $this->{red_max}   = 192;
    $this->{green_min} = 64;
    $this->{green_max} = 192;
    $this->{blue_min}  = 64;
    $this->{blue_max}  = 192;
  }
  elsif($scheme eq 'NONE')
  {
    $this->{red_min}   = 0;
    $this->{red_max}   = 255;
    $this->{green_min} = 0;
    $this->{green_max} = 255;
    $this->{blue_min}  = 0;
    $this->{blue_max}  = 255;
  }
  else
  {
    return;
  }
  $saved_scheme = $scheme;
}

sub rndRed
{
  my $this = shift;
  my %parm = @_;

  my $red_min = exists $parm{red_min} ? $parm{red_min} : $this->{red_min};
  my $red_max = exists $parm{red_max} ? $parm{red_max} : $this->{red_max};
  my $step    = exists $parm{step}    ? $parm{step}    : $this->{step};

  $step = 1 unless defined $step;
  $step = int $step;
  $step = 1 if $step == 0;

  my $scale = ($red_max - $red_min)/$step;
  my $red   =  $red_min + ($step*int(rand $scale));

  $this->{red} = $red;
}

sub rndGreen
{
  my $this = shift;
  my %parm = @_;

  my $green_min = exists $parm{green_min} ? $parm{green_min} : $this->{green_min};
  my $green_max = exists $parm{green_max} ? $parm{green_max} : $this->{green_max};
  my $step      = exists $parm{step}      ? $parm{step}      : $this->{step};

  $step = 1 unless defined $step;
  $step = int $step;
  $step = 1 if $step == 0;

  my $scale = ($green_max - $green_min)/$step;
  my $green =  $green_min + ($step*int(rand $scale));

  $this->{green} = $green;
}

sub rndBlue
{
  my $this = shift;
  my %parm = @_;

  my $blue_min = exists $parm{blue_min} ? $parm{blue_min} : $this->{blue_min};
  my $blue_max = exists $parm{blue_max} ? $parm{blue_max} : $this->{blue_max};
  my $step     = exists $parm{step}     ? $parm{step}     : $this->{step};

  $step = 1 unless defined $step;
  $step = int $step;
  $step = 1 if $step == 0;

  my $scale = ($blue_max - $blue_min)/$step;
  my $blue  =  $blue_min + ($step*int(rand $scale));

  $this->{blue} = $blue;
}

1;
