# Util::GetOpt --- Provides command line option handling -*-Perl-*-

#         Copyright © 2012-2023 Tom Fontaine

# Author: Tom Fontaine
# Date:   25-Sep-2012

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
# Revision: 17-May-2020 use v5.10
#           15-Sep-2022 fixed args handling
#                       added ‘=’ handling
#           03-May-2023 moved from ‘Util::GetOptNew’
#           12-Jun-2023 use Modern::Perl
#           12-Jun-2023 use Modern::Perl
>>>>>>> 12b96db1f3d32ff9f2cb57bafc6ef5583c8bad51

# Code:

package Util::GetOpt;

use Carp;
use Modern::Perl;
use Regexp::Assemble;

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

our $AUTOLOAD;

my @AREF = qw(group bare1 bare2 arg1 arg2);
my @HREF = qw(option);

my %fields = (args   => \@ARGV,

              option => undef,

              group  => undef,

              bare1  => undef,
              bare2  => undef,
              args1  => undef,
              args2  => undef,
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

  bless  $this,$class;

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

  my $args   = exists $parm{args}   ? $parm{args}   : $this->{args};
  my $option = exists $parm{option} ? $parm{option} : $this->{option};
  my $group  = exists $parm{group}  ? $parm{group}  : $this->{group};
  my $bare1  = exists $parm{bare1}  ? $parm{bare1}  : $this->{bare1};
  my $bare2  = exists $parm{bare2}  ? $parm{bare2}  : $this->{bare2};
  my $args1  = exists $parm{args1}  ? $parm{args1}  : $this->{args1};
  my $args2  = exists $parm{args2}  ? $parm{args2}  : $this->{args2};

  my $reGroup = defined $group && $group ? "[$group]+" : '^\b';

  my $raBare1 = Regexp::Assemble->new;
  my $raBare2 = Regexp::Assemble->new;
  my $raArgs1 = Regexp::Assemble->new;
  my $raArgs2 = Regexp::Assemble->new;

  $raBare1->add(ref $bare1 ? @{$bare1} : $bare1);
  $raBare2->add(ref $bare2 ? @{$bare2} : $bare2);
  $raArgs1->add(ref $args1 ? @{$args1} : $args1);
  $raArgs2->add(ref $args2 ? @{$args2} : $args2);

  my $reBare1 = $raBare1->re;
  my $reBare2 = $raBare2->re;
  my $reArgs1 = $raArgs1->re;
  my $reArgs2 = $raArgs2->re;

  my @tmp = @{$args};

  my $idx = 0;

  while(@tmp)
  {
    my $arg = shift @tmp;

    if($reArgs1 ne '^\b' && $arg =~ /^-($reArgs1)$/)
    {
      # -p 1
      #
      $option->{$1} = shift @tmp;

      splice @{$args},$idx,2;
    }
    elsif($reArgs1 ne '^\b' && $arg =~ /^-($reArgs1)=([^\s]+)$/)
    {
      # -p=1
      #
      $option->{$1} = $2;

      splice @{$args},$idx,1;
    }
    elsif($reArgs2 ne '^\b' && $arg =~ /^--($reArgs2)$/)
    {
      # --param 1
      #
      $option->{$1} = shift @tmp;

      splice @{$args},$idx,2;
    }
    elsif($reArgs2 ne '^\b' && $arg =~ /^--($reArgs2)=([^\s]+)$/)
    {
      # --param=1
      #
      $option->{$1} = $2;

      splice @{$args},$idx,1;
    }
    elsif($reBare1 ne '^\b' && $arg =~ /^-($reBare1)$/)
    {
      # -p
      #
      $option->{$1} = 1;

      splice @{$args},$idx,1;
    }
    elsif($reBare2 ne '^\b' && $arg =~ /^--($reBare2)$/)
    {
      # --param
      #
      $option->{$1} = 1;

      splice @{$args},$idx,1;
    }
    elsif($reGroup ne '^\b' && $arg =~ /^-($reGroup)$/)
    {
      # -rip
      #
      @{$option}{split //,$1} = (1) x length($1);

      splice @{$args},$idx,1;
    }
    else
    {
      ++$idx;
    }
  }
}

1;
