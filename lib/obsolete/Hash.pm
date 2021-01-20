# File::Hash --- [description] -*-Perl-*-

#         Copyright © 2020-2021 Tom Fontaine

# Author: Tom Fontaine
# Date:   10-May-2020

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
# Revision: 17-May-2020 this file is OBSOLETE
#

package File::Hash;

require 5.008;
use Carp;
use strict;
use v5.10;

# use Foo::Bar;

use File::IO;
use Text::Hash;

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

our $AUTOLOAD;

my @AREF = qw(contents text);
my @HREF = qw();

my %fields = (contents => undef,
              text     => undef,

              io       => undef,
              th       => undef,
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

sub userinit
{
  my $this = shift;
  my %parm = @_;

  my $io = File::IO->new(%parm);
  my $th = Text::Hash->new(%parm);

  # die _ME_,': File::IO dependency undefined!!!'   unless exists $parm{io};
  # die _ME_,': Text::Hash dependency undefined!!!' unless exists $parm{th};

  $this->{io} = $io;
  $this->{th} = $th;

  say $this->{io};
  say $this->{th}->delimiter;
  $this->{th}->configure(contents => $this->contents,text => $this->text);
  $this->{io}->configure(contents => $this->text);
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
  my $text     = exists $parm{text}     ? $parm{text}     : $this->{text};

  $this->{th}->configure(contents => $contents,text => $text);
  $this->{io}->configure(contents => $text);
  $this->{io}->get;
}

sub put
{
  my $this = shift;
  my %parm = @_;

  my $contents = exists $parm{contents} ? $parm{contents} : $this->{contents};
  my $text     = exists $parm{text}     ? $parm{text}     : $this->{text};

  $this->{th}->configure(contents => $contents,text => $text);
  $this->{io}->configure(contents => $text);
  $this->{io}->put;
}

1;
