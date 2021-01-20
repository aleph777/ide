# File::C --- Provides a C source code file object -*-Perl-*-

#         Copyright © 2015-2021 Tom Fontaine

# Author: Tom Fontaine
# Date:   17-Apr-2015

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
# Revision: 10-Mar-2016 Fixed problem with contents and chomp parameters
#
package File::C;

require 5.008;
use Carp;
use strict;

use File::SourceCode;

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

our $AUTOLOAD;

my @AREF = qw(contents);
my @HREF = qw();

my %fields = (contents => undef,

              path     => undef,
              basename => undef,
              basedir  => undef,

              chomp    => 0,
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

  my $contents = exists $parm{contents} ? $parm{contents} : $this->{contents};
  my $path     = exists $parm{path}     ? $parm{path}     : $this->{path};
  my $basename = exists $parm{basename} ? $parm{basename} : $this->{basename};
  my $basedir  = exists $parm{basedir}  ? $parm{basedir}  : $this->{basedir};
  my $chomp    = exists $parm{chomp}    ? $parm{chomp}    : $this->{chomp};
  my $pragma   = exists $parm{pragma}   ? $parm{pragma}   : $this->{pragma};

  my $sc = File::SourceCode->new(path  => $path, basename => $basename,basedir => $basedir,
                                 chomp => $chomp,pragma   => $pragma,  style   => 'C');

  $sc->get(contents => $contents);
}

1;
