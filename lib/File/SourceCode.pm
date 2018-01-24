# File::SourceCode --- Provides a source code object -*-Perl-*-

#         Copyright © 2015-2018 Tom Fontaine

# Author: Tom Fontaine
# Date:   15-Apr-2015

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
package File::SourceCode;

require 5.006;
use Carp;
use strict;
use File::IO;
use Regexp::Assemble;
use constant C_STYLES           => qw(C c);
use constant C_PLUS_PLUS_STYLES => qw(CPP Cpp C++ cpp c++);
use constant CSHARP_STYLES      => ('C#',qw(CSHARP CSharp CS cs));
use constant JAVA_STYLES        => qw(JAVA Java java);
use constant PERL_STYLES        => qw(PERL Perl perl PL pl);

our $AUTOLOAD;

our $AUTOLOAD;

my @AREF = qw(contents);
my @HREF = qw();

my %fields = (contents => undef,

              path     => undef,
              basename => undef,
              basedir  => undef,

              pragma   => 0,

              chomp    => 0,

               style   => undef,
             );

my $__ME__ = join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

my ($rac,$racs,$rapl);

BEGIN
{
  $rac  = Regexp::Assemble->new;
  $racs = Regexp::Assemble->new;
  $rapl = Regexp::Assemble->new;

  $rac->add (map { join '','^',$_,'$' } C_STYLES,C_PLUS_PLUS_STYLES,JAVA_STYLES);
  $racs->add(map { join '','^',$_,'$' } CSHARP_STYLES);
  $rapl->add(map { join '','^',$_,'$' } PERL_STYLES);
}

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

  my $__ME__ = (caller(0))[3];

  my $contents = exists $parm{contents} ? $parm{contents} : $this->{contents};
  my $path     = exists $parm{path}     ? $parm{path}     : $this->{path};
  my $basename = exists $parm{basename} ? $parm{basename} : $this->{basename};
  my $basedir  = exists $parm{basedir}  ? $parm{basedir}  : $this->{basedir};
  my $chomp    = exists $parm{chomp}    ? $parm{chomp}    : $this->{chomp};
  my $pragma   = exists $parm{pragma}   ? $parm{pragma}   : $this->{pragma};
  my $style    = exists $parm{style}    ? $parm{style}    : $this->{style};

  my $filename;

  if(defined $path)
  {
    $filename = $path;
  }
  elsif(defined $basename)
  {
    $filename = defined $basedir ? join '/',$basedir,$basename : $basename;
  }
  die "$__ME__: no file specified!!!\n" unless defined $filename;

  if($style =~ /$rapl/o)
  {
    @{$contents} = qx(perltidy -st -mbl=0 -bl --delete-all-comments $filename);
  }
  elsif($style =~ /$racs/o || $style =~ /$rac/o)
  {
    my @tmp;

    my $io = File::IO->new(path => $filename,contents => \@tmp);

    $io->get();

    if($pragma && ($style =~ /$racs/o))
    {
      s=^//PRAGMA EXCEPTION HANDLER=#PRAGMA= for @tmp;
    }
    my $tmp = join '',@tmp;
    #
    # Remove comments
    #
    $tmp =~ s#/\*[^*]*\*+([^/*][^*]*\*+)*/|//[^\n]*|("(\\.|[^"\\])*"|'(\\.|[^'\\])*'|.[^/"'\\]*)#defined $2 ? $2 : ''#egs;

    #
    # Remove blank lines
    #
    @{$contents} = grep !/^\s*$/,map { "$_\n" } split /\n\r?/,$tmp;

    if($chomp)
    {
      chomp @{$contents};

      local $/ = "\r";

      chomp @{$contents};
    }
  }
  else
  {
    die "$__ME__: style '$style' not supported!!!\n";
  }
}

1;
