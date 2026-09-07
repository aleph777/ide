# File::Tmp --- Provides a tmp file object   -*-Perl-*-

#         Copyright © 2007-2026 Tom Fontaine

# Author: Tom Fontaine
# Date:   31-Jan-2007

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
# Revision: 14-Apr-2011 added maxLength method
#                       added support for pipes
#           10-Jan-2013 added support for UTF8
#           25-Mar-2015 removed usage of bareword file handles
#                       require 5.008
#           30-Mar-2015 added auto-support for .gz and .xz files
#           13-Apr-2015 use List::Util
#           20-Jul-2019 added dependency injection for text conversion
#           17-Feb-2021 use v5.10
#           02-Jun-2023 use Modern::Perl
#

# Code:

package File::Tmp;

use Carp;
use Modern::Perl;
use File::IO;

use Math::Random::Secure qw(irand);

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

our $AUTOLOAD;

my @AREF = qw(contents);
my @HREF = qw();

my %fields = (contents => undef,

              path     => undef,
              basename => undef,
              basedir  => '/tmp',
              suffix   => undef,
              namelen  => 16,

              chomp    => 0,
              newline  => 0,
             );

my @chars = ('a' .. 'z','A' .. 'Z','0' .. '9');

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

  my $_SELF_ = join '::',_ME_,(caller(0))[3];

  my $path      = exists $parm{path}      ? $parm{path}      : $this->{path};
  my $contents  = exists $parm{contents}  ? $parm{contents}  : $this->{contents};
  my $basename  = exists $parm{basename}  ? $parm{basename}  : $this->{basename};
  my $basedir   = exists $parm{basedir}   ? $parm{basedir}   : $this->{basedir};
  my $suffix    = exists $parm{suffix}    ? $parm{suffix}    : $this->{suffix};
  my $namelen   = exists $parm{namelen}   ? $parm{namelen}   : $this->{namelen};
  my $chomp     = exists $parm{chomp}     ? $parm{chomp}     : $this->{chomp};

  my $io = File::IO->new(chomp => $chomp,contents => $contents);

  unless (defined $path)
  {
    $path = $this->get_path(basename => $basename,basedir => $basedir,suffix => $suffix,namelen => $namelen);

    $this->{path} = $path;
  }
  $io->get(path => $path);
}

sub get_path
{
  my $this = shift;
  my %parm = @_;

  my $basename  = exists $parm{basename}  ? $parm{basename}  : $this->{basename};
  my $basedir   = exists $parm{basedir}   ? $parm{basedir}   : $this->{basedir};
  my $suffix    = exists $parm{suffix}    ? $parm{suffix}    : $this->{suffix};
  my $namelen   = exists $parm{namelen}   ? $parm{namelen}   : $this->{namelen};

  $basename =  join '',map { $chars[irand @chars] } 1 .. $namelen unless defined $basename;
  $basedir  =  '/tmp' unless defined $basedir;
  $basedir  =~ s=/{2,}=/=g;
  $suffix   =~ s/^\.+//;

  my $path = join '.',join('/',$basedir,$basename),defined $suffix && $suffix ? $suffix : ();

  return $path;
}

sub put
{
  my $this = shift;
  my %parm = @_;

  my $_SELF_ = join '::',_ME_,(caller(0))[3];

  my $path      = exists $parm{path}      ? $parm{path}      : $this->{path};
  my $contents  = exists $parm{contents}  ? $parm{contents}  : $this->{contents};
  my $basename  = exists $parm{basename}  ? $parm{basename}  : $this->{basename};
  my $basedir   = exists $parm{basedir}   ? $parm{basedir}   : $this->{basedir};
  my $suffix    = exists $parm{suffix}    ? $parm{suffix}    : $this->{suffix};
  my $newline   = exists $parm{newline}   ? $parm{newline}   : $this->{newline};
  my $namelen   = exists $parm{namelen}   ? $parm{namelen}   : $this->{namelen};

  my $io = File::IO->new(newline => $newline,contents => $contents);

  unless (defined $path)
  {
    $path = $this->get_path(basename => $basename,basedir => $basedir,suffix => $suffix,namelen => $namelen);

    $this->{path} = $path;
  }
  $io->put(path => $path);
}

1;
