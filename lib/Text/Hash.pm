# Text::Hash --- Produces a list of hashes from the input text -*-Perl-*-

#         Copyright © 2008-2023 Tom Fontaine

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
# Revision: 14-Jun-2023 use Modern::Perl
#

# Code:

package Text::Hash;

use Carp;
use Modern::Perl;

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

our $AUTOLOAD;

my @AREF = qw(contents text keys);
my @HREF = qw();

my %fields = (type      => undef,

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

  my $_SELF_ = join '::',_ME_,(caller(0))[3];

  my $type      = exists $parm{type}      ? $parm{type}      : $this->{type};
  my $delimiter = exists $parm{delimiter} ? $parm{delimiter} : $this->{delimiter};
  my $keys      = exists $parm{keys}      ? $parm{keys}      : $this->{keys};
  my $text      = exists $parm{text}      ? $parm{text}      : $this->{text};
  my $contents  = exists $parm{contents}  ? $parm{contents}  : $this->{contents};
  my $keyIndex  = exists $parm{keyIndex}  ? $parm{keyIndex}  : $this->{keyIndex};

  $type = uc $type;

  if($type eq 'FIRST')
  {
    first($delimiter,$keys,$text,$contents);
  }
  elsif($type eq 'PREDEFINED')
  {
    predefined($delimiter,$keys,$text,$contents);
  }
  elsif($type eq 'VARIANT')
  {
    variant($delimiter,$keys,$text,$contents,$keyIndex);
  }
  else
  {
    die "$_SELF_: Unknown hash type: $type!!!\n";
  }
}

sub put
{
  my $this = shift;
  my %parm = @_;

  my $_SELF_ = join '::',_ME_,(caller(0))[3];

  my $type      = exists $parm{type}      ? $parm{type}      : $this->{type};
  my $delimiter = exists $parm{delimiter} ? $parm{delimiter} : $this->{delimiter};
  my $keys      = exists $parm{keys}      ? $parm{keys}      : $this->{keys};
  my $text      = exists $parm{text}      ? $parm{text}      : $this->{text};
  my $contents  = exists $parm{contents}  ? $parm{contents}  : $this->{contents};
  my $keyName   = exists $parm{keyName}   ? $parm{keyName}   : $this->{keyName};

  $type = uc $type;

  @{$text} = ();

  push @{$text},join $delimiter,@{$keys} if $type eq 'FIRST';

  if($type ne 'VARIANT')
  {
    for(@{$contents})
    {
      push @{$text},join $delimiter,@{$_}{@{$keys}};
    }
  }
  else
  {
    for(@{$contents})
    {
    }
  }
}

sub first
{
  my ($delimiter,$keys,$text,$contents) = @_;

  my $first = shift @{$text};

  @{$keys} = split $delimiter,$first;

  my $limit = scalar @{$keys};

  @{$contents} = ();

  for(@{$text})
  {
    my $r = {};

    @{$r}{@{$keys}} = split $delimiter,$_,$limit;

    push @{$contents},$r;
  }
}

sub predefined
{
  my ($delimiter,$keys,$text,$contents) = @_;

  my $limit = scalar @{$keys};

  @{$contents} = ();

  for(@{$text})
  {
    my $r = {};

    @{$r}{@{$keys}} = split $delimiter,$_,$limit;

    push @{$contents},$r;
  }
}

sub variant
{
  my ($delimiter,$keys,$text,$contents,$keyIndex) = @_;

  my $__ME__ = (caller(0))[3];

  my ($key,$limit,@tmp);

  @{$contents} = ();

  for(@{$text})
  {
    @tmp = split $delimiter,$_;

    $key = $tmp[$keyIndex];

    die "$__ME__: Key type $key does not exist!!!" unless exists $keys->{$key};

    $limit = scalar @{$keys->{$key}};

    my $r = {};


    push @{$contents},$r;
  }
}

1;
