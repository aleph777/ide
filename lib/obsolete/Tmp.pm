# File::Tmp --- Provides a /tmp file object with a random name -*-Perl-*-

#         Copyright © 2015-2021 Tom Fontaine

# Author: Tom Fontaine
# Date:   16-Apr-2015

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
# Revision: 10-May-2020 Added userinit method
#                       Added getPath
#           17-May-2020 this file is OBSOLETE
#
package File::Tmp;

require 5.008;
use Carp;
use strict;
use Math::Random::Secure qw(irand);
use v5.10;

# use constant FOO => 'BAR';

our $AUTOLOAD;

my @AREF = qw(contents);
my @HREF = qw();

my %fields = (contents => undef,
              path     => undef,

              ext      => undef,
              basedir  => '/tmp',
              n        => 16,
              chars    => [q(0) .. q(9),q(A) .. q(Z),q(a) .. q(z)],

              io       => undef,
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

  bless $this,$class;

  @{$this}{@AREF} = map { [] } @AREF;
  @{$this}{@HREF} = map { {} } @HREF;

  my %parm = @_;

  @{$this}{keys %parm} = values %parm;

  $this->userinit(%parm);

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

sub userinit
{
  my $this = shift;
  my %parm = @_;

  $this->{path} = $this->getPath(%parm) unless defined $this->{path};

  $this->{io}->configure(path => $this->{path});

  say $__ME__,': ',$this->{io}->contents;
  say $__ME__,': ',scalar @{$this->{io}->contents};
  say $__ME__,': ',$_ for @{$this->{io}->contents};
}

sub put
{
  my $this = shift;
  my %parm = @_;

  my $contents  = exists $parm{contents}  ? $parm{contents}  : $this->{contents};

  $this->{io}->put(contents => $contents);
}

sub getPath
{
  my $this = shift;
  my %parm = @_;

  my $ext      = exists $parm{ext}      ? $parm{ext}      : $this->{ext};
  my $chars    = exists $parm{chars}    ? $parm{chars}    : $this->{chars};
  my $n        = exists $parm{n}        ? $parm{n}        : $this->{n};
  my $basedir  = exists $parm{basedir}  ? $parm{basedir}  : $this->{basedir};

  my $file = join '',map { $chars->[irand @{$chars}] } 1 .. $n;

  $file = join '.',$file,$ext if defined $ext;

  return join '',$basedir,'/',$file;
}

1;
