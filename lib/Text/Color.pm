# Text::Color --- [description] -*-Perl-*-

#         Copyright © 2026-2026 Thomas Fontaine

# Author: Thomas Fontaine
# Date:   06-Aug-2026

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

# Code:

package Text::Color;

use Carp;
use Modern::Perl;

# use Foo::Bar;

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

our $AUTOLOAD;

my @AREF = qw(contents);
my @HREF = qw();

my %fields = (contents  => undef,
              bold      => undef,
              normal    => undef,
              black     => undef,
              red       => undef,
              green     => undef,
              yellow    => undef,
              blue      => undef,
              magenta   => undef,
              cyan      => undef,
              white     => undef,
              underline => undef,
             );

my %text;

BEGIN
{
  $text{bold}      = qx(tput bold);
  $text{normal}    = qx(tput sgr0);
  $text{underline} = qx(tput smul);
  $text{unormal}   = qx(tput rmul);
  $text{black}     = qx(tput setaf 0);
  $text{red}       = qx(tput setaf 1);
  $text{green}     = qx(tput setaf 2);
  $text{yellow}    = qx(tput setaf 3);
  $text{blue}      = qx(tput setaf 4);
  $text{magenta}   = qx(tput setaf 5);
  $text{cyan}      = qx(tput setaf 6);
  $text{white}     = qx(tput setaf 7);
}


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

sub put
{
  my $this = shift;
  my %parm = @_;

  my $_SELF_ = join '::',_ME_,(caller(0))[3];

  my %c;

  my $contents     = exists $parm{contents}  ? $parm{contents}  : $this->{contents};
     $c{bold}      = exists $parm{bold}      ? $parm{bold}      : $this->{bold};
     $c{black}     = exists $parm{black}     ? $parm{black}     : $this->{black};
     $c{red}       = exists $parm{red}       ? $parm{red}       : $this->{red};
     $c{green}     = exists $parm{green}     ? $parm{green}     : $this->{green};
     $c{yellow}    = exists $parm{yellow}    ? $parm{yellow}    : $this->{yellow};
     $c{blue}      = exists $parm{blue}      ? $parm{blue}      : $this->{blue};
     $c{magenta}   = exists $parm{magenta}   ? $parm{magenta}   : $this->{magenta};
     $c{cyan}      = exists $parm{cyan}      ? $parm{cyan}      : $this->{cyan};
     $c{underline} = exists $parm{underline} ? $parm{underline} : $this->{underline};

  print $_ for (map { $text{$_} } grep { defined $c{$_} } keys %c),@{$contents},$text{normal};
  print $text{unormal} if defined $c{underline};
  print "\n";
}

1;
