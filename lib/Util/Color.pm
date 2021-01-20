# Util::Color --- color utilities -*-Perl-*-

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
# Revision: 17-May-2020 use v5.10
#

package Util::Color;

require 5.008;
use Carp;
use strict;
use v5.10;

use Math::Random::Secure;
use List::Util qw(min max);

use constant _PROGRAM_ => $0 =~ m=([^/]+)$=;

use constant SCALE_DISTANCE => sqrt 10;

our $AUTOLOAD;

my @AREF = qw(colors);
my @HREF = qw();

my %fields = (contents => undef,

              red   => undef,
              green => undef,
              blue  => undef,

              value => undef,
              hex1  => undef,
              hex2  => undef,

              file   => undef,
              colors => undef,

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

my @hue = (('R' ) x  6,
           ('Ry') x 12,
           ('RY') x 12,
           ('YR') x 12,
           ('Yr') x 12,
           ('Y' ) x 12,
           ('Yg') x 12,
           ('YG') x 12,
           ('GY') x 12,
           ('Gy') x 12,
           ('G' ) x 12,
           ('Gc') x 12,
           ('GC') x 12,
           ('CG') x 12,
           ('Cg') x 12,
           ('C' ) x 12,
           ('Cb') x 12,
           ('CB') x 12,
           ('BC') x 12,
           ('Bc') x 12,
           ('B' ) x 12,
           ('Bm') x 12,
           ('BM') x 12,
           ('MB') x 12,
           ('Mb') x 12,
           ('M' ) x 12,
           ('Mr') x 12,
           ('MR') x 12,
           ('RM') x 12,
           ('Rm') x 12,
           ('R' ) x  6,
          );

my %color = (R  => {TYPE => 'red',     SUBTYPE => 'red',         COMMON => 'red'},
             Ry => {TYPE => 'red',     SUBTYPE => 'red-yellow',  COMMON => 'red'},
             RY => {TYPE => 'red',     SUBTYPE => 'red-yellow',  COMMON => '???'},
             YR => {TYPE => 'yellow',  SUBTYPE => 'yellow-red',  COMMON => '???'},
             Yr => {TYPE => 'yellow',  SUBTYPE => 'yellow-red',  COMMON => '???'},
             Y  => {TYPE => 'yellow',  SUBTYPE => 'yellow'    ,  COMMON => 'yellow'},
             Yg => {TYPE => 'yellow',  SUBTYPE => 'yellow-green',COMMON => '???'},
             YG => {TYPE => 'yellow',  SUBTYPE => 'yellow-green',COMMON => '???'},
             GY => {TYPE => 'green',   SUBTYPE => 'green-yellow',COMMON => '???'},
             Gy => {TYPE => 'green',   SUBTYPE => 'green-yellow',COMMON => '???'},
             G  => {TYPE => 'green',   SUBTYPE => 'green',      ,COMMON => 'green'},
             Gc => {TYPE => 'green',   SUBTYPE => 'green-cyan',  COMMON => '???'},
             GC => {TYPE => 'green',   SUBTYPE => 'green-cyan',  COMMON => '???'},
             CG => {TYPE => 'cyan',    SUBTYPE => 'cyan-green',  COMMON => '???'},
             Cg => {TYPE => 'cyan',    SUBTYPE => 'cyan-green',  COMMON => '???'},
             C  => {TYPE => 'cyan',    SUBTYPE => 'cyan',        COMMON => 'cyan'},
             Cb => {TYPE => 'cyan',    SUBTYPE => 'cyan-blue',   COMMON => '???'},
             CB => {TYPE => 'cyan',    SUBTYPE => 'cyan-blue',   COMMON => '???'},
             BC => {TYPE => 'blue',    SUBTYPE => 'blue-cyan',   COMMON => '???'},
             Bc => {TYPE => 'blue',    SUBTYPE => 'blue-cyan',   COMMON => '???'},
             B  => {TYPE => 'blue',    SUBTYPE => 'blue',        COMMON => 'blue'},
             Bm => {TYPE => 'blue',    SUBTYPE => 'blue-magenta',COMMON => '???'},
             BM => {TYPE => 'blue',    SUBTYPE => 'blue-magenta',COMMON => '???'},
             MB => {TYPE => 'magenta', SUBTYPE => 'magenta-blue',COMMON => '???'},
             Mb => {TYPE => 'magenta', SUBTYPE => 'magenta-blue',COMMON => '???'},
             M  => {TYPE => 'magenta', SUBTYPE => 'magenta',     COMMON => 'magenta'},
             Mr => {TYPE => 'magenta', SUBTYPE => 'magenta-red', COMMON => '???'},
             MR => {TYPE => 'magenta', SUBTYPE => 'magenta-red', COMMON => '???'},
             RM => {TYPE => 'red',     SUBTYPE => 'red-magenta', COMMON => '???'},
             Rm => {TYPE => 'red',     SUBTYPE => 'red-magenta', COMMON => '???'},
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

  my $__ME__ = join '::',_PROGRAM_,(caller(0))[3];

  my $red   = exists $parm{red}     ? $parm{red}     : $this->{red};
  my $green = exists $parm{green}   ? $parm{green}   : $this->{green};
  my $blue  = exists $parm{blue}    ? $parm{blue}    : $this->{blue};
  my $fmt   = exists $parm{_format} ? $parm{_format} : $this->{_format};

  $red   = 0 unless defined $red;
  $green = 0 unless defined $green;
  $blue  = 0 unless defined $blue;

  $red   = min(max($red,   0), 255);
  $green = min(max($green, 0), 255);
  $blue  = min(max($blue,  0), 255);

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

  my $__ME__ = join '::',_PROGRAM_,(caller(0))[3];

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

  $this->rndRed(  red_min   => $red_min,   red_max   => $red_max,   step => $step);
  $this->rndGreen(green_min => $green_min, green_max => $green_max, step => $step);
  $this->rndBlue( blue_min  => $blue_min,  blue_max  => $blue_max,  step => $step);

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

  my $__ME__ = join '::',_PROGRAM_,(caller(0))[3];

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

  my $__ME__ = join '::',_PROGRAM_,(caller(0))[3];

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

  my $__ME__ = join '::',_PROGRAM_,(caller(0))[3];

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

sub getRGB
{
  my $this = shift;
  my %parm = @_;

  my $red   = exists $parm{red}     ? $parm{red}     : $this->{red};
  my $green = exists $parm{green}   ? $parm{green}   : $this->{green};
  my $blue  = exists $parm{blue}    ? $parm{blue}    : $this->{blue};
  my $value = exists $parm{value}   ? $parm{value}   : $this->{value};

  if(defined $value)
  {
    my ($color) = $value =~ /^#*([[:xdigit:]]{6})/;die $value unless defined $color;

    ($red,$green,$blue) = map { hex(join '',"0x",$_)/256 } (substr($color,0,2),substr($color,2,2),substr($color,4,2));

    die "($red,$green,$blue)\n" unless defined $red && defined $green && defined $blue;
  }
  else
  {
    $red   = $red   =~ /^0x/ ? hex($red  /256)   : $red  /256;
    $green = $green =~ /^0x/ ? hex($green/256)   : $green/256;
    $blue  = $blue  =~ /^0x/ ? hex($blue /256)   : $blue /256;
  }
  return ($red,$green,$blue);
}

sub computeDistance
{
  my $this = shift;
  my %parm = @_;

  my $hex1 = exists $parm{hex1} ? $parm{hex1} : $this->{hex1};
  my $hex2 = exists $parm{hex2} ? $parm{hex2} : $this->{hex2};

  my ($red1,$green1,$blue1) = $this->getRGB(value => $hex1);
  my ($red2,$green2,$blue2) = $this->getRGB(value => $hex2);

  return SCALE_DISTANCE*sqrt(($red1   - $red2)  **2 +
                             ($green1 - $green2)**2 +
                             ($blue1  - $blue2) **2);
}

sub computeLuminance
{
  my $this = shift;
  my %parm = @_;

  my $red   = exists $parm{red}     ? $parm{red}     : $this->{red};
  my $green = exists $parm{green}   ? $parm{green}   : $this->{green};
  my $blue  = exists $parm{blue}    ? $parm{blue}    : $this->{blue};
  my $value = exists $parm{value}   ? $parm{value}   : $this->{value};

  my @rgb = map { $_ <= 0.03928 ? $_/12.92 : (($_  + 0.055)/1.055)**2.4 } $this->getRGB(red => $red,green => $green,blue => $blue,value => $value);

  return 0.2126*$rgb[0] + 0.7152*$rgb[1] + 0.0722*$rgb[2];
}

sub computeContrast
{
  my $this = shift;
  my %parm = @_;

  my $hex1 = exists $parm{hex1} ? $parm{hex1} : $this->{hex1};
  my $hex2 = exists $parm{hex2} ? $parm{hex2} : $this->{hex2};


  my $ratio = ($this->computeLuminance(value => $hex1) + 0.05)/($this->computeLuminance(value => $hex2) + 0.05);

  return $ratio >= 1 ? $ratio : 1/$ratio;
}

sub computeHSL
{
  my $this = shift;
  my %parm = @_;

  my $red   = exists $parm{red}     ? $parm{red}     : $this->{red};
  my $green = exists $parm{green}   ? $parm{green}   : $this->{green};
  my $blue  = exists $parm{blue}    ? $parm{blue}    : $this->{blue};
  my $value = exists $parm{value}   ? $parm{value}   : $this->{value};

  ($red,$green,$blue) = $this->getRGB(red => $red,green => $green,blue => $blue,value => $value);

  my %color;

  @color{qw(RED GREEN BLUE)} = ($red,$green,$blue);

  my @keys = sort { $color{$b} <=> $color{$a} } qw(RED GREEN BLUE);

  my $maxColor = $color{$keys[0]};
  my $minColor = $color{$keys[2]};
  my $range    = $maxColor - $minColor;

  my $luminosity = ($maxColor + $minColor)/2;

  my $saturation;
  my $hue;

  if($range < 0.0001)
  {
    $saturation = 0;
    $hue        = 0;
  }
  else
  {
    $saturation = $range/$maxColor;

    if($keys[0] eq 'RED')
    {
      $hue = 60*($green - $blue)/$range;
    }
    elsif($keys[0] eq 'GREEN')
    {
      $hue = 60*(2.0 + ($blue - $red)/$range);
    }
    else
    {
      $hue = 60*(4.0 + ($red - $green)/$range);
    }
  }
  $hue += 360 if $hue < 0;

  return ($hue,$saturation,$luminosity);
}

sub getShade
{
  my $luminosity = 2*shift;

  if($luminosity < 0.125)
  {
    return 'black';
  }
  elsif($luminosity < 0.375)
  {
    return 'dark';
  }
  elsif($luminosity < 0.625)
  {
    return ();
  }
  elsif($luminosity < 0.875)
  {
    return 'light';
  }
  else
  {
    return 'bright';
  }
}

sub getColorType
{
  my $this = shift;
  my %parm = @_;

  my $red   = exists $parm{red}     ? $parm{red}     : $this->{red};
  my $green = exists $parm{green}   ? $parm{green}   : $this->{green};
  my $blue  = exists $parm{blue}    ? $parm{blue}    : $this->{blue};
  my $value = exists $parm{value}   ? $parm{value}   : $this->{value};

  my ($hue,$saturation,$luminosity) = $this->computeHSL(red => $red,green => $green,blue => $blue,value => $value);

  my $r = {TYPE => undef, SHADE => undef};

  if($hue == 0 && $saturation == 0)
  {
    $r->{TYPE}  = 'gray';
  }
  else
  {
    my $h = $hue[int $hue];
    my $c = $color{$h};

    $r->{TYPE} = $saturation >= 0.25 ? $c->{SUBTYPE} : 'gray';

  }
  if($luminosity < 0.125)
  {
    $r->{SHADE} = 'black';
  }
  elsif($luminosity < 0.375)
  {
    $r->{SHADE} = 'dark';
  }
  elsif($luminosity < 0.625)
  {
    $r->{SHADE} = 'medium';
  }
  elsif($luminosity < 0.875)
  {
    $r->{SHADE} = 'light'
  }
  else
  {
    $r->{SHADE} = 'white';
  }
  return $r;
  # elsif($hue >=   0 && $hue <  30)
  # {
  #   return join '-',$saturation >= 0.25 ? 'red' : 'gray',getShade($luminosity);
  # }
  # elsif($hue >=  30 && $hue <  60)
  # {
  #   return join '-',$saturation >= 0.25 ? 'orange' : 'gray',getShade($luminosity);
  # }
  # elsif($hue >=  60 && $hue <  90)
  # {
  #   return join '-',$saturation >= 0.25 ? 'yellow' : 'gray',getShade($luminosity);
  # }
  # elsif($hue >=  90 && $hue < 120)
  # {
  #   return join '-',$saturation >= 0.25 ? 'green-yellow' : 'gray',getShade($luminosity);
  # }
  # elsif($hue >= 120 && $hue < 150)
  # {
  #   return join '-',$saturation >= 0.25 ? 'green' : 'gray',getShade($luminosity);
  # }
  # elsif($hue >= 150 && $hue < 180)
  # {
  #   return join '-',$saturation >= 0.25 ? 'cyan-green' : 'gray',getShade($luminosity);
  # }
  # elsif($hue >= 180 && $hue <  210)
  # {
  #   return join '-',$saturation >= 0.25 ? 'cyan' : 'gray',getShade($luminosity);
  # }
  # elsif($hue >= 210 && $hue <  240)
  # {
  #   return join '-',$saturation >= 0.25 ? 'blue-cyan' : 'gray',getShade($luminosity);
  # }
  # elsif($hue >= 240 && $hue < 270)
  # {
  #   return join '-',$saturation >= 0.25 ? 'blue' : 'gray',getShade($luminosity);
  # }
  # elsif($hue >= 270 && $hue < 300)
  # {
  #   return join '-',$saturation >= 0.25 ? 'indigo' : 'gray',getShade($luminosity);
  # }
  # elsif($hue >= 300 && $hue < 330)
  # {
  #   return join '-',$saturation >= 0.25 ? 'magenta' : 'gray',getShade($luminosity);
  # }
  # elsif($hue >= 330 && $hue < 360)
  # {
  #   return join '-',$saturation >= 0.25 ? 'red-magenta' : 'gray',getShade($luminosity);
  # }
  # else
  # {
  #   die "$value: ($hue,$saturation,$luminosity)";
  # }
}

sub parseThemeColors
{
  my $this = shift;
  my %parm = @_;

  my $file   = exists $parm{file}    ? $parm{file}    : $this->{file};
  my $colors = exists $parm{colors}  ? $parm{colors}  : $this->{colors};

  $file->get();

  for(@{$file->contents})
  {
    if(my ($name,$value) = /^.defconst ([a-z0-9-]+\/[a-z0-9-]+) +"(#[0-9a-f]{6})"\)/)
    {
      my $r = {NAME => $name,VALUE => $value};

      push @{$colors},$r;
    }
  }
}

1;
