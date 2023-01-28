# Math::Cards --- Implements a deck of playing cards object -*-Perl-*-

#         Copyright © 2008-2023 Tom Fontaine

# Author: Tom Fontaine
# Date:   18-May-2008

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
# Revision: 25-Mar-2015 use Ascii
#           10-Apr-2015 use Util::Shuffle
#           15-Apr-2015 Changed new to initialize contents from deck
#                       Added methods perfectShuffle and imperfectShuffle
#                       Renamed method shuffle to randomShuffle
#
package Math::Cards;

require 5.006;
use Carp;
use strict;

use Util::Shuffle;

use Math::Random::Secure qw(irand);
use Ascii qw(ESC);

use constant CSI    => join '',ESC,'[';
use constant RED    => join '',CSI,31,';1m';
use constant BLACK  => join '',CSI,30,';1m';
use constant PLAIN  => join '',CSI,0,'m';

use constant RANKS => qw(ACE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN JACK QUEEN KING);

use constant CLUBS    => map { join '',BLACK,"$_ of CLUBS",   PLAIN} RANKS;
use constant SPADES   => map { join '',BLACK,"$_ of SPADES",  PLAIN} RANKS;
use constant HEARTS   => map { join '',RED,  "$_ of HEARTS",  PLAIN} RANKS;
use constant DIAMONDS => map { join '',RED,  "$_ of DIAMONDS",PLAIN} RANKS;

use constant DECK => (SPADES,HEARTS,CLUBS,DIAMONDS);

our $AUTOLOAD;

my @AREF = qw(contents);
my @HREF = qw();

my %fields = (contents => undef,
              deck     => [DECK],);

my $__ME__ = join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

my $sh = Util::Shuffle->new();

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

  @{$this->contents} = @{$this->deck};

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

sub resetDeck
{
  my $this = shift;
  my %parm = @_;

  my $contents = exists $parm{contents} ? $parm{contents} : $this->{contents};
  my $deck     = exists $parm{deck}     ? $parm{deck}     : $this->{deck};

  @{$contents} = @{$deck};
}
sub randomShuffle
{
  my $this = shift;
  my %parm = @_;

  my $contents = exists $parm{contents} ? $parm{contents} : $this->{contents};

  $sh->get(contents => $contents);
}
sub perfectShuffle
{
  my $this = shift;
  my %parm = @_;

  my $contents = exists $parm{contents} ? $parm{contents} : $this->{contents};
  #
  # can't assume deck size is even
  #
  my $size  = @{$contents};
  my $sizel = @{$contents}/2;
  my $sizer = $size - $sizel;

  my @rh = splice @{$contents};
  my @lh = splice @rh,0,$sizel;

  @{$contents} = map { (@lh ? shift @lh : ()),(@rh ? shift @rh : ()) } 1 .. $size;
}
sub imperfectShuffle
{
  my $this = shift;
  my %parm = @_;

  my $contents = exists $parm{contents} ? $parm{contents} : $this->{contents};

  my $size  = @{$contents};
  my $range = int($size/10 + 0.5);
  my $diff  = irand(2) ? irand($range) : irand(-$range);

  my @rh = splice @{$contents};
  my @lh = splice @rh,0,$size/2+$diff;

  while(@rh || @lh)
  {
    unless(@rh)
    {
      push @{$contents},@lh;
      last;
    }
    unless(@lh)
    {
      push @{$contents},@rh;
      last;
    }
    push @{$contents},irand(2) ? shift @lh : shift @rh;
  }
}

1;
