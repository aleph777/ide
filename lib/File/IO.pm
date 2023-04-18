# File::IO --- Provides a file object -*-Perl-*-

#         Copyright © 2007-2023 Tom Fontaine

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
#           30-Mar-2015 Added auto-support for .gz and .xz files
#           13-Apr-2015 Use List::Util
#           20-Jul-2019 Added dependency injection for text conversion
#           17-Feb-2021 use v5.10
#
package File::IO;

use Carp;
use IO::Compress::Gzip;
use IO::Compress::Xz;
use IO::Uncompress::Gunzip;
use IO::Uncompress::UnXz;
use List::Util qw(max);
use strict;
use v5.10;

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

our $AUTOLOAD;

my @AREF = qw(contents);
my @HREF = qw();

my %fields = (contents => undef,

              path     => undef,
              basename => undef,
              basedir  => undef,

              command  => undef,

              processor => undef,

              chomp    => 0,

              newline  => 0,
              ors      => undef,

              append   => 0,

              unix     => 0,
              stdio    => 0,
              perlio   => 0,
              crlf     => 0,
              bytes    => 0,
              raw      => 0,
              utf8     => 0,
             );

my %layer = (unix     => ':unix',
             stdio    => ':stdio',
             perlio   => ':perlio',
             crlf     => ':crlf',
             bytes    => ':bytes',
             raw      => ':raw',
             utf8     => ':encoding(utf8)',
            );

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

  my $__ME__ = (caller(0))[3];

  my $path      = exists $parm{path}      ? $parm{path}      : $this->{path};
  my $basename  = exists $parm{basename}  ? $parm{basename}  : $this->{basename};
  my $basedir   = exists $parm{basedir}   ? $parm{basedir}   : $this->{basedir};

  my $filename;

  if(defined $path)
  {
    $filename = $path;
  }
  elsif(defined $basename)
  {
    $filename = defined $basedir ? join '/',$basedir,$basename : $basename;
  }
  my $chomp     = exists $parm{chomp}     ? $parm{chomp}     : $this->{chomp};
  my $contents  = exists $parm{contents}  ? $parm{contents}  : $this->{contents};
  my $unix      = exists $parm{unix}      ? $parm{unix}      : $this->{unix};
  my $stdio     = exists $parm{stdio}     ? $parm{stdio}     : $this->{stdio};
  my $perlio    = exists $parm{perlio}    ? $parm{perlio}    : $this->{perlio};
  my $crlf      = exists $parm{crlf}      ? $parm{crlf}      : $this->{crlf};
  my $bytes     = exists $parm{bytes}     ? $parm{bytes}     : $this->{bytes};
  my $raw       = exists $parm{raw}       ? $parm{raw}       : $this->{raw};
  my $utf8      = exists $parm{utf8}      ? $parm{utf8}      : $this->{utf8};
  my $processor = exists $parm{processor} ? $parm{processor} : $this->{processor};
  my $command   = exists $parm{command}   ? $parm{command}   : $this->{command};

  if(defined $filename)
  {
    my $fh;

    if(substr($filename,-3) eq '.gz')
    {
      $fh = IO::Uncompress::Gunzip->new($filename) or die "$__ME__ - Unable to open $filename: $!!!!\n";
    }
    elsif(substr($filename,-3) eq '.xz')
    {
      $fh = IO::Uncompress::UnXz->new($filename) or die "$__ME__ - Unable to open $filename: $!!!!\n";
    }
    else
    {
      my $mode = '<';

      if(substr($filename,0,1) eq '|')
      {
        $mode = '-|';

        substr($filename,0,1) = '';
      }
      open($fh,$mode,$filename) or die "$__ME__ - Unable to open $filename: $!!!!\n";
    }
    @{$contents} = <$fh>;
    close $fh;
  }
  elsif(defined $command)
  {
    @{$contents} = qx($command);
  }
  else
  {
    @{$contents} = <>;
  }
  if($chomp)
  {
    chomp @{$contents};

    local $/ = "\r";

    chomp @{$contents};
  }
  $processor->get if defined $processor;
}

sub put
{
  my $this = shift;
  my %parm = @_;

  my $__ME__ = (caller(0))[3];

  my $path      = exists $parm{path}      ? $parm{path}      : $this->{path};
  my $basename  = exists $parm{basename}  ? $parm{basename}  : $this->{basename};
  my $basedir   = exists $parm{basedir}   ? $parm{basedir}   : $this->{basedir};
  my $newline   = exists $parm{newline}   ? $parm{newline}   : $this->{newline};
  my $append    = exists $parm{append}    ? $parm{append}    : $this->{append};
  my $ors       = exists $parm{ors}       ? $parm{ors}       : $this->{ors};
  my $contents  = exists $parm{contents}  ? $parm{contents}  : $this->{contents};
  my $processor = exists $parm{processor} ? $parm{processor} : $this->{processor};

  my $filename;

  $processor->put if defined $processor;

  if(defined $path)
  {
    $filename = $path;
  }
  elsif(defined $basename)
  {
    $filename = defined $basedir ? join '/',$basedir,$basename : $basename;
  }
  $contents = [$contents] unless ref $contents;

  if(defined $filename)
  {
    my $fh;

    if(substr($filename,-3) eq '.gz')
    {
      $fh = IO::Compress::Gzip->new($filename) or die "$__ME__ - Unable to open $filename: $!!!!\n";
    }
    elsif(substr($filename,-3) eq '.xz')
    {
      $fh = IO::Compress::Xz->new($filename) or die "$__ME__ - Unable to open $filename: $!!!!\n";
    }
    else
    {
      my $mode = $append ? '>>' : '>';

      if(substr($filename,0,1) eq '|')
      {
        $mode = '|-';

        substr($filename,0,1) = '';
      }
      open($fh,$mode,$filename) or die "$__ME__ - Unable to open $filename: $!!!!\n";
    }
    if($newline)
    {
      say $fh $_ for @{$contents};
    }
    elsif(defined $ors)
    {
      print $fh $_,$ors for @{$contents};
    }
    else
    {
      print $fh @{$contents};
    }
    close $fh;
  }
  else
  {
    if($newline)
    {
      say $_ for @{$contents};
    }
    elsif(defined $ors)
    {
      print $_,$ors for @{$contents};
    }
    else
    {
      print @{$contents};
    }
  }
}

sub maxLength
{
  my $this = shift;
  my %parm = @_;

  my $contents  = exists $parm{contents}  ? $parm{contents}  : $this->{contents};

  return max(map { length $_ } @{$contents});
}

1;
