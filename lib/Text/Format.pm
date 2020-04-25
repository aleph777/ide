# Text::Format --- Generates a column-aligned minimally-spaced format string -*-Perl-*-

#         Copyright © 1999-2020 Tom Fontaine

# Author: Tom Fontaine
# Date:   22-Mar-1999

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
# Revision: 03-Jan-2003 Added Meta
#           12-May-2008 Moved to Text::Format
#                       use strict
#
package Text::Format;

require 5.006;
use Carp;
use strict;

use List::Util qw(max);

# use constant FOO => 'BAR';

our $AUTOLOAD;

my @AREF = qw(keys leftKeys contents);
my @HREF = qw(length);

my %fields = (keys       => undef,
              leftKeys   => undef,
              contents   => undef,
              length     => undef,
              delimiter  => ' ',
              useKeys    => undef,
              newline    => undef,
              format     => undef,
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

  my $fkeys    = exists $parm{keys}      ? $parm{keys}      : $this->{keys};
  my $left     = exists $parm{leftKeys}  ? $parm{leftKeys}  : $this->{leftKeys};
  my $contents = exists $parm{contents}  ? $parm{contents}  : $this->{contents};
  my $len      = exists $parm{length}    ? $parm{length}    : $this->{length};
  my $usekeys  = exists $parm{useKeys}   ? $parm{useKeys}   : $this->{useKeys};
  my $newline  = exists $parm{newline}   ? $parm{newline}   : $this->{newline};
  my $delim    = exists $parm{delimiter} ? $parm{delimiter} : $this->{delimiter};

  my %left = map { $_ => 1 } @{$left};

  if(defined $usekeys && $usekeys)
  {
    foreach my $key (@{$fkeys})
    {
      $len->{$key} = max(length($key),map { length $_->{$key} } @{$contents});
    }
  }
  else
  {
    foreach my $key (@{$fkeys})
    {
      $len->{$key} = max(map { length $_->{$key} } @{$contents});
    }
  }
  $this->{format}  = join $delim,map { join '',exists $left{$_} ? '%-' : '%',$len->{$_},'s' } @{$fkeys};
  $this->{format} .= "\n" if defined $newline && $newline;

  return $this->{format};
}

1;
