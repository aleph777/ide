# Text::SQL --- SQL support -*-Perl-*-

#         Copyright © 2015-2019 Tom Fontaine

# Author: Tom Fontaine
# Date:   12-Aug-2015

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
# Revision: 18-Jan-2017 Fixed syntax errors
#
package Text::SQL;

require 5.008;
use Carp;
use strict;

# use Foo::Bar;

# use constant FOO => 'BAR';

our $AUTOLOAD;

my @AREF = qw(contents data);
my @HREF = qw();

my %fields = (contents => undef,
              data     => undef,

              tableName => undef,
              tableKeys => undef,
              tableId   => undef,

              comment => '---------------------------------------',
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

sub getInsert
{
  my $this = shift;
  my %parm = @_;

  my $__ME__ = (caller(0))[3];

  my $contents  = exists $parm{contents}  ? $parm{contents}  : $this->{contents};
  my $data      = exists $parm{data}      ? $parm{data}      : $this->{data};
  my $tableName = exists $parm{tableName} ? $parm{tableName} : $this->{tableName};
  my $tableKeys = exists $parm{tableKeys} ? $parm{tableKeys} : $this->{tableKeys};
  my $tableId   = exists $parm{tableId}   ? $parm{tableId}   : $this->{tableId};

  die "$__ME__: table name is undefined!!!"   unless defined $tableName;
  die "$__ME__: table keys are  undefined!!!" unless defined $tableKeys;

  my $insert = join '','insert into ',$tableName,' ( ',join(', ',map { sprintf '[%s]',$_ } @{$tableKeys}),' )';
  my $select = defined $tableId ? join '','select',$tableId,'= @@Identity;' : undef;

  @{$contents} =();

  foreach my $d (@{$data})
  {
    push @{$contents},$insert,join '','  values ( ',join(', ',map { $d->{$_} } @{$tableKeys}),' );';
  }
}

1;
