# File::Hash --- Produces a list of hashes from the input text file -*-Perl-*-

#         Copyright © 2008-2019 Tom Fontaine

# Author: Tom Fontaine
# Date:   12-May-2008

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
package File::Hash;

require 5.006;
use Carp;
use strict;
use File::IO;
use Text::Hash;

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

our $AUTOLOAD;

my @AREF = qw(contents text keys);
my @HREF = qw();

my %fields = (path      => undef,
              basename  => undef,
              basedir   => undef,

              type      => undef,

              irs       => undef,

              append    => 0,
              ors       => undef,

              keys      => undef,
              text      => undef,
              contents  => undef,
              delimiter => undef,

              keyIndex  => undef,
              keyName   => undef,
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

  my $path      = exists $parm{path}      ? $parm{path}      : $this->{path};
  my $basename  = exists $parm{basename}  ? $parm{basename}  : $this->{basename};
  my $basedir   = exists $parm{basedir}   ? $parm{basedir}   : $this->{basedir};
  my $text      = exists $parm{text}      ? $parm{text}      : $this->{text};
  my $irs       = exists $parm{irs}       ? $parm{irs}       : $this->{irs};

  my $file = File::IO->new(path     => $path,
                           basename => $basename,
                           basedir  => $basedir,
                           contents => $text,
                           irs      => $irs,
                           chomp    => 1);

  my $type      = exists $parm{type}      ? $parm{type}      : $this->{type};
  my $delimiter = exists $parm{delimiter} ? $parm{delimiter} : $this->{delimiter};
  my $keys      = exists $parm{keys}      ? $parm{keys}      : $this->{keys};
  my $contents  = exists $parm{contents}  ? $parm{contents}  : $this->{contents};
  my $keyIndex  = exists $parm{keyIndex}  ? $parm{keyIndex}  : $this->{keyIndex};

  my $hash = Text::Hash->new(type      => $type,
                             delimiter => $delimiter,
                             keys      => $keys,
                             text      => $text,
                             contents  => $contents,
                             keyIndex  => $keyIndex);

  $file->get();
  $hash->get();
}

sub put
{
  my $this = shift;
  my %parm = @_;

  my $type      = exists $parm{type}      ? $parm{type}      : $this->{type};
  my $delimiter = exists $parm{delimiter} ? $parm{delimiter} : $this->{delimiter};
  my $keys      = exists $parm{keys}      ? $parm{keys}      : $this->{keys};
  my $text      = exists $parm{text}      ? $parm{text}      : $this->{text};
  my $contents  = exists $parm{contents}  ? $parm{contents}  : $this->{contents};
  my $keyName   = exists $parm{keyName}   ? $parm{keyName}   : $this->{keyName};

  my $hash = Text::Hash->new(type      => $type,
                             delimiter => $delimiter,
                             keys      => $keys,
                             text      => $text,
                             contents  => $contents,
                             keyName   => $keyName);

  my $path      = exists $parm{path}      ? $parm{path}      : $this->{path};
  my $basename  = exists $parm{basename}  ? $parm{basename}  : $this->{basename};
  my $basedir   = exists $parm{basedir}   ? $parm{basedir}   : $this->{basedir};
  my $append    = exists $parm{append}    ? $parm{append}    : $this->{append};
  my $ors       = exists $parm{ors}       ? $parm{ors}       : $this->{ors};

  my $file = File::IO->new(path     => $path,
                           basename => $basename,
                           basedir  => $basedir,
                           append   => $append,
                           contents => $text,
                           ors      => $ors,
                           newline  => 1);

  $hash->put();
  $file->put();
}

1;
