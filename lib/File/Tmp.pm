# File::Tmp --- Provides a /tmp file object with a random name -*-Perl-*-

#              Copyright © 2015-2017 Tom Fontaine

#
# Author:      Tom Fontaine
# Date:        16-Apr-2015
# Time-stamp: <18-Jan-2017 16:34:49 EST, modified by Tom Fontaine>
#

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
package File::Tmp;

require 5.008;
use Carp;
use strict;
use File::IO;
use Math::Random::Secure qw(irand);

# use constant FOO => 'BAR';

our $AUTOLOAD;

my @AREF = qw(contents);
my @HREF = qw();

my %fields = (contents => undef,
              path     => undef,
              ext      => undef,

              newline  => 0,
              ors      => undef,

              append   => 0,
             );

my $__ME__ = join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

my $io = File::IO->new();

my @chars = (q(0) .. q(9),q(A) .. q(Z),q(a) .. q(z));

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

  $this->{path} = join '','/tmp/',map { $chars[irand @chars] } 1 .. 16 unless defined $this->{path};

  if(defined $this->{ext})
  {
    $this->{ext}  =~ s/^\.+//;
    $this->{path} =  join '.',$this->{path},$this->{ext};
  }
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

  my $contents = exists $parm{contents} ? $parm{contents} : $this->{contents};
  my $path     = exists $parm{path}     ? $parm{path}     : $this->{path};
  my $ext      = exists $parm{ext}      ? $parm{ext}      : $this->{ext};
  my $newline  = exists $parm{newline}  ? $parm{newline}  : $this->{newline};
  my $ors      = exists $parm{ors}      ? $parm{ors}      : $this->{ors};
  my $append   = exists $parm{append}   ? $parm{append}   : $this->{append};

  unless(defined $path)
  {
    $path = join '','/tmp/',map { $chars[irand @chars] } 1 .. 16 unless defined $path;

    if(defined $ext)
    {
      $ext  =~ s/^\.+//;
      $path =  join '.',$path,$ext
    }
    $this->{path} = $path;
  }
  $io->put(contents => $contents,path => $path,newline => $newline,ors => $ors,append => $append);
}

1;
