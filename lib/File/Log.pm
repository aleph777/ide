# File::Log --- Provides a log-file object -*-Perl-*-

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
# Revision: 31-Jan-2007 initial version
#           15-May-2008 use Util::Time
#                       use IO::Handle
#           25-Mar-2015 removed usage of bareword file handles
#                       require 5.008
#           21-Apr-2015 changed to 3 argument open from critique
#
package File::Log;

require 5.008;
use Carp;
use strict;
use File::IO;
use Util::Time;
use IO::Handle;

# use constant FOO => 'BAR';

our $AUTOLOAD;

my @AREF = qw();
my @HREF = qw();

my %fields = (path      => undef,
              basename  => undef,
              basedir   => undef,

              clobber   => 1,
              timestamp => 0,
              echo      => 0,

              who       => undef,
             );

my $logfile;
my $ts;
my $timestamp;
my $who;
my $msg;
my $echo;
my $open;
my $stderr;
my $saverr;

my $__ME__ = join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

BEGIN
{
  open $saverr,'>&STDERR';

  $stderr = IO::Handle->new_from_fd(fileno($saverr),'w');
}

END
{
  close $saverr;

  $stderr = undef;
}

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

sub open
{
  my $this = shift;
  my %parm = @_;

  my $path      = exists $parm{path}      ? $parm{path}      : $this->{path};
  my $basename  = exists $parm{basename}  ? $parm{basename}  : $this->{basename};
  my $basedir   = exists $parm{basedir}   ? $parm{basedir}   : $this->{basedir};
  my $clobber   = exists $parm{clobber}   ? $parm{clobber}   : $this->{clobber};

  my $filename;

  if(defined $path)
  {
    $filename = $path;
  }
  elsif(defined $basename)
  {
    $filename = defined $basedir ? join '/',$basedir,$basename : $basename;
  }
  unlink $filename if $clobber && defined $filename && -e $filename;

  $timestamp = exists $parm{timestamp} ? $parm{timestamp} : $this->{timestamp};
  $who       = exists $parm{who}       ? $parm{who}       : $this->{who};
  $echo      = exists $parm{echo}      ? $parm{echo}      : $this->{echo};

  $msg  = '';
  $open = 1;

  $logfile = File::IO->new(path => $filename,newline => 1,append  => 1);
  $ts      = Util::Time->new(style => 'LOG');

  open STDERR,'>>',$filename;
}
sub close
{
  $timestamp = undef;
  $who       = undef;
  $msg       = undef;
  $open      = undef;

  close STDERR;

  STDERR->fdopen($stderr,'w');
}
sub append
{
  my $this = shift;
  my $text = shift;

  $msg .= $text;
}
sub prepend
{
  my $this = shift;
  my $text = shift;

  $msg = join '',$text,$msg;
}
sub put
{
  my $this = shift;
  my %parm = @_;

  unless(defined $open)
  {
    print "Logfile is not OPEN!!!\n";
    exit;
  }
  $who  = exists $parm{who}  ? $parm{who}  : $who;
  $echo = exists $parm{echo} ? $parm{echo} : $echo;

  my $time = $timestamp ? getTimestamp() : undef;

  $msg = join ' ',grep { defined } $time,$who,"$msg$parm{text}";

  $logfile->put(contents => $msg);

  print $msg,"\n" if $echo;

  $msg = '';
}
sub getTimestamp
{
  $ts->get();

  return $ts->timestamp;
}

1;
