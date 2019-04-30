# File::FindWrapper --- Provides a ‘find’ object -*-Perl-*-

#         Copyright © 2000-2018 Tom Fontaine

# Author: Tom Fontaine
# Date:   28-Jun-2000

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
# Revision: 17-Feb-2005 Added field for returning absolute pathnames
#           12-May-2008 Moved to File::FindWrapper
#                       use strict
#           07-Jul-2014 Added -suffix, -no-suffix, -dirname, and -no-dirname
#           25-Mar-2015 Removed -tty and -no-tty
#           31-Mar-2015 Now using File::FixPath

package File::FindWrapper;

require 5.006;
require Exporter;
use Carp;
use strict;
use File::Find;
use File::Basename;
use File::FixPath qw(fixpath);

use Cwd qw(abs_path);

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

my @switches = qw(read-effective    no-read-effective
                  write-effective   no-write-effective
                  execute-effective no-execute-effective
                  owned-effective   no-owned-effective
                  read-real         no-read-real
                  write-real        no-write-real
                  execute-real      no-execute-real
                  owned-real        no-owned-real
                  zero-size         no-zero-size
                  file              no-file
                  dir               no-dir
                  link              no-link
                  pipe              no-pipe
                  socket            no-socket
                  block             no-block
                  char              no-char
                  setuid            no-setuid
                  setgid            no-setgid
                  sticky            no-sticky
                  text              no-text
                  bin               no-bin

                  sort

                  OR
                 );

my @argswitches = qw(size-gt   size-lt   size-ge   size-le
                     age-gt    age-lt    age-ge    age-le
                     access-gt access-lt access-ge access-le
                     inode-gt  inode-lt  inode-ge  inode-le

                     name              no-name
                     basename          no-basename
                     suffix            no-suffix
                     dirname           no-dirname
                   );

our $AUTOLOAD;

my @AREF = qw(contents);
my @HREF = qw(option);

my %fields = (root        => '.',
              contents    => undef,
              properties  => undef,
              option      => undef,
              useFind     => 1,
              absolute    => 0,
              switches    => \@switches,
              argSwitches => \@argswitches,
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

  my $root     = exists $parm{root}       ? $parm{root}       : $this->{root};
  my $contents = exists $parm{contents}   ? $parm{contents}   : $this->{contents};
  my $props    = exists $parm{properties} ? $parm{properties} : $this->{properties};
  my $option   = exists $parm{option}     ? $parm{option}     : $this->{option};
  my $absolute = exists $parm{absolute}   ? $parm{absolute}   : $this->{absolute};
  my $usefind  = exists $parm{useFind}    ? $parm{useFind}    : $this->{useFind};

  if($usefind)
  {
    fixpath(\@{$root});

    my @dirs = ref $root ? map { glob $_ } @{$root} : glob $root;

    find(sub { push @{$contents},$File::Find::name },@dirs) if $usefind;

    @{$contents} = map { abs_path($_) } @{$contents} if $absolute;
  }
  my @keys = keys %{$option};

  if(defined $props && $props)
  {
    my $file;

    do
    {
      $file = shift @{$contents};

      my $h = {NAME => $file};

      $h->{READ_EFFECTIVE}    = 1 if -r $file;
      $h->{WRITE_EFFECTIVE}   = 1 if -w $file;
      $h->{EXECUTE_EFFECTIVE} = 1 if -x $file;
      $h->{OWNED_EFFECTIVE}   = 1 if -o $file;
      $h->{READ_REAL}         = 1 if -R $file;
      $h->{WRITE_REAL}        = 1 if -W $file;
      $h->{EXECUTE_REAL}      = 1 if -X $file;
      $h->{OWNED_REAL}        = 1 if -O $file;
      $h->{ZERO_SIZE}         = 1 if -z $file;

      $h->{SIZE}              = -s $file;

      $h->{FILE}              = 1 if -f $file;
      $h->{DIR}               = 1 if -d $file;
      $h->{LINK}              = 1 if -l $file;
      $h->{PIPE}              = 1 if -p $file;
      $h->{SOCKET}            = 1 if -S $file;
      $h->{BLOCK}             = 1 if -b $file;
      $h->{CHAR}              = 1 if -c $file;
      $h->{SETUID}            = 1 if -u $file;
      $h->{SETGID}            = 1 if -g $file;
      $h->{STICKY}            = 1 if -k $file;
      $h->{TEXT}              = 1 if -T $file;
      $h->{BIN}               = 1 if -B $file;

      $h->{AGE}               = -M $file;
      $h->{ACCESS}            = -A $file;
      $h->{INODE}             = -C $file;

      push @{$contents},$h;

    } until ref $contents->[0];
  }
  else
  {
    return unless @keys;

    my $name        = exists $option->{   'name'}     ? $option->{   'name'}     : undef;
    my $no_name     = exists $option->{'no-name'}     ? $option->{'no-name'}     : undef;
    my $basename    = exists $option->{   'basename'} ? $option->{   'basename'} : undef;
    my $no_basename = exists $option->{'no-basename'} ? $option->{'no-basename'} : undef;
    my $suffix      = exists $option->{   'suffix'}   ? $option->{   'suffix'}   : undef;
    my $no_suffix   = exists $option->{'no-suffix'}   ? $option->{'no-suffix'}   : undef;
    my $dirname     = exists $option->{   'dirname'}  ? $option->{   'dirname'}  : undef;
    my $no_dirname  = exists $option->{'no-dirname'}  ? $option->{'no-dirname'}  : undef;

    $suffix    =~ s/^\.?/\\./ if defined $suffix;
    $no_suffix =~ s/^\.?/\\./ if defined $no_suffix;

    if(exists $option->{OR})
    {
      my @files;

      push @files,grep     /$name/,@{$contents} if defined $name;
      push @files,grep !/$no_name/,@{$contents} if defined $no_name;

      push @files,grep { basename($_) =~    /$basename/ } @{$contents} if defined $basename;
      push @files,grep { basename($_) !~ /$no_basename/ } @{$contents} if defined $no_basename;

      push @files,grep     /$suffix$/,@{$contents} if defined $suffix;
      push @files,grep !/$no_suffix$/,@{$contents} if defined $no_suffix;

      push @files,grep { dirname($_) =~    /$dirname/ } @{$contents} if defined $dirname;
      push @files,grep { dirname($_) !~ /$no_dirname/ } @{$contents} if defined $no_dirname;

      push @files,grep {  -r $_ } @{$contents} if exists $option->{   'read-effective'};
      push @files,grep { !-r $_ } @{$contents} if exists $option->{'no-read-effective'};
      push @files,grep {  -w $_ } @{$contents} if exists $option->{   'write-effective'};
      push @files,grep { !-w $_ } @{$contents} if exists $option->{'no-write-effective'};
      push @files,grep {  -x $_ } @{$contents} if exists $option->{   'execute-effective'};
      push @files,grep { !-x $_ } @{$contents} if exists $option->{'no-execute-effective'};
      push @files,grep {  -o $_ } @{$contents} if exists $option->{   'owned-effective'};
      push @files,grep { !-o $_ } @{$contents} if exists $option->{'no-owned-effective'};
      push @files,grep {  -R $_ } @{$contents} if exists $option->{   'read-real'};
      push @files,grep { !-R $_ } @{$contents} if exists $option->{'no-read-real'};
      push @files,grep {  -W $_ } @{$contents} if exists $option->{   'write-real'};
      push @files,grep { !-W $_ } @{$contents} if exists $option->{'no-write-real'};
      push @files,grep {  -X $_ } @{$contents} if exists $option->{   'execute-real'};
      push @files,grep { !-X $_ } @{$contents} if exists $option->{'no-execute-real'};
      push @files,grep {  -O $_ } @{$contents} if exists $option->{   'owned-real'};
      push @files,grep { !-O $_ } @{$contents} if exists $option->{'no-owned-real'};
      push @files,grep {  -z $_ } @{$contents} if exists $option->{   'zero-size'};
      push @files,grep { !-z $_ } @{$contents} if exists $option->{'no-zero-size'};

      push @files,grep { -s $_ >  $option->{'size-gt'} } @{$contents} if exists $option->{'size-gt'};
      push @files,grep { -s $_ <  $option->{'size-lt'} } @{$contents} if exists $option->{'size-lt'};
      push @files,grep { -s $_ >= $option->{'size-ge'} } @{$contents} if exists $option->{'size-ge'};
      push @files,grep { -s $_ <= $option->{'size-le'} } @{$contents} if exists $option->{'size-le'};

      push @files,grep {  -f $_ } @{$contents} if exists $option->{   'file'};
      push @files,grep { !-f $_ } @{$contents} if exists $option->{'no-file'};
      push @files,grep {  -d $_ } @{$contents} if exists $option->{   'dir'};
      push @files,grep { !-d $_ } @{$contents} if exists $option->{'no-dir'};
      push @files,grep {  -l $_ } @{$contents} if exists $option->{   'link'};
      push @files,grep { !-l $_ } @{$contents} if exists $option->{'no-link'};
      push @files,grep {  -p $_ } @{$contents} if exists $option->{   'pipe'};
      push @files,grep { !-p $_ } @{$contents} if exists $option->{'no-pipe'};
      push @files,grep {  -S $_ } @{$contents} if exists $option->{   'socket'};
      push @files,grep { !-S $_ } @{$contents} if exists $option->{'no-socket'};
      push @files,grep {  -b $_ } @{$contents} if exists $option->{   'block'};
      push @files,grep { !-b $_ } @{$contents} if exists $option->{'no-block'};
      push @files,grep {  -c $_ } @{$contents} if exists $option->{   'char'};
      push @files,grep { !-c $_ } @{$contents} if exists $option->{'no-char'};
      push @files,grep {  -u $_ } @{$contents} if exists $option->{   'setuid'};
      push @files,grep { !-u $_ } @{$contents} if exists $option->{'no-setuid'};
      push @files,grep {  -g $_ } @{$contents} if exists $option->{   'setgid'};
      push @files,grep { !-g $_ } @{$contents} if exists $option->{'no-setgid'};
      push @files,grep {  -k $_ } @{$contents} if exists $option->{   'sticky'};
      push @files,grep { !-k $_ } @{$contents} if exists $option->{'no-sticky'};
      push @files,grep {  -T $_ } @{$contents} if exists $option->{   'text'};
      push @files,grep { !-T $_ } @{$contents} if exists $option->{'no-text'};
      push @files,grep {  -B $_ } @{$contents} if exists $option->{   'bin'};
      push @files,grep { !-B $_ } @{$contents} if exists $option->{'no-bin'};

      push @files,grep { -M $_ >  $option->{'age-gt'} } @{$contents} if exists $option->{'age-gt'};
      push @files,grep { -M $_ <  $option->{'age-lt'} } @{$contents} if exists $option->{'age-lt'};
      push @files,grep { -M $_ >= $option->{'age-ge'} } @{$contents} if exists $option->{'age-ge'};
      push @files,grep { -M $_ <= $option->{'age-le'} } @{$contents} if exists $option->{'age-le'};

      push @files,grep { -A $_ >  $option->{'access-gt'} } @{$contents} if exists $option->{'access-gt'};
      push @files,grep { -A $_ <  $option->{'access-lt'} } @{$contents} if exists $option->{'access-lt'};
      push @files,grep { -A $_ >= $option->{'access-ge'} } @{$contents} if exists $option->{'access-ge'};
      push @files,grep { -A $_ <= $option->{'access-le'} } @{$contents} if exists $option->{'access-le'};

      push @files,grep { -C $_ >  $option->{'inode-gt'} } @{$contents} if exists $option->{'inode-gt'};
      push @files,grep { -C $_ <  $option->{'inode-lt'} } @{$contents} if exists $option->{'inode-lt'};
      push @files,grep { -C $_ >= $option->{'inode-ge'} } @{$contents} if exists $option->{'inode-ge'};
      push @files,grep { -C $_ <= $option->{'inode-le'} } @{$contents} if exists $option->{'inode-le'};

      my %file = map { $_ => 1 } @files;

      @{$contents} = sort keys %file;
    }
    else
    {
      @{$contents} = grep     /$name/,@{$contents} if defined $name;
      @{$contents} = grep !/$no_name/,@{$contents} if defined $no_name;

      @{$contents} = grep { basename($_) =~    /$basename/ } @{$contents} if defined $basename;
      @{$contents} = grep { basename($_) !~ /$no_basename/ } @{$contents} if defined $no_basename;

      @{$contents} = grep     /$suffix$/,@{$contents} if exists $option->{   'suffix'};
      @{$contents} = grep !/$no_suffix$/,@{$contents} if exists $option->{'no-suffix'};

      @{$contents} = grep { dirname($_) =~    /$dirname/ } @{$contents} if defined $dirname;
      @{$contents} = grep { dirname($_) !~ /$no_dirname/ } @{$contents} if defined $no_dirname;

      @{$contents} = grep {  -r $_ } @{$contents} if exists $option->{   'read-effective'};
      @{$contents} = grep { !-r $_ } @{$contents} if exists $option->{'no-read-effective'};
      @{$contents} = grep {  -w $_ } @{$contents} if exists $option->{   'write-effective'};
      @{$contents} = grep { !-w $_ } @{$contents} if exists $option->{'no-write-effective'};
      @{$contents} = grep {  -x $_ } @{$contents} if exists $option->{   'execute-effective'};
      @{$contents} = grep { !-x $_ } @{$contents} if exists $option->{'no-execute-effective'};
      @{$contents} = grep {  -o $_ } @{$contents} if exists $option->{   'owned-effective'};
      @{$contents} = grep { !-o $_ } @{$contents} if exists $option->{'no-owned-effective'};
      @{$contents} = grep {  -R $_ } @{$contents} if exists $option->{   'read-real'};
      @{$contents} = grep { !-R $_ } @{$contents} if exists $option->{'no-read-real'};
      @{$contents} = grep {  -W $_ } @{$contents} if exists $option->{   'write-real'};
      @{$contents} = grep { !-W $_ } @{$contents} if exists $option->{'no-write-real'};
      @{$contents} = grep {  -X $_ } @{$contents} if exists $option->{   'execute-real'};
      @{$contents} = grep { !-X $_ } @{$contents} if exists $option->{'no-execute-real'};
      @{$contents} = grep {  -O $_ } @{$contents} if exists $option->{   'owned-real'};
      @{$contents} = grep { !-O $_ } @{$contents} if exists $option->{'no-owned-real'};
      @{$contents} = grep {  -z $_ } @{$contents} if exists $option->{   'zero-size'};
      @{$contents} = grep { !-z $_ } @{$contents} if exists $option->{'no-zero-size'};

      @{$contents} = grep { -s $_ >  $option->{'size-gt'} } @{$contents} if exists $option->{'size-gt'};
      @{$contents} = grep { -s $_ <  $option->{'size-lt'} } @{$contents} if exists $option->{'size-lt'};
      @{$contents} = grep { -s $_ >= $option->{'size-ge'} } @{$contents} if exists $option->{'size-ge'};
      @{$contents} = grep { -s $_ <= $option->{'size-le'} } @{$contents} if exists $option->{'size-le'};

      @{$contents} = grep {  -f $_ } @{$contents} if exists $option->{   'file'};
      @{$contents} = grep { !-f $_ } @{$contents} if exists $option->{'no-file'};
      @{$contents} = grep {  -d $_ } @{$contents} if exists $option->{   'dir'};
      @{$contents} = grep { !-d $_ } @{$contents} if exists $option->{'no-dir'};
      @{$contents} = grep {  -l $_ } @{$contents} if exists $option->{   'link'};
      @{$contents} = grep { !-l $_ } @{$contents} if exists $option->{'no-link'};
      @{$contents} = grep {  -p $_ } @{$contents} if exists $option->{   'pipe'};
      @{$contents} = grep { !-p $_ } @{$contents} if exists $option->{'no-pipe'};
      @{$contents} = grep {  -S $_ } @{$contents} if exists $option->{   'socket'};
      @{$contents} = grep { !-S $_ } @{$contents} if exists $option->{'no-socket'};
      @{$contents} = grep {  -b $_ } @{$contents} if exists $option->{   'block'};
      @{$contents} = grep { !-b $_ } @{$contents} if exists $option->{'no-block'};
      @{$contents} = grep {  -c $_ } @{$contents} if exists $option->{   'char'};
      @{$contents} = grep { !-c $_ } @{$contents} if exists $option->{'no-char'};
      @{$contents} = grep {  -u $_ } @{$contents} if exists $option->{   'setuid'};
      @{$contents} = grep { !-u $_ } @{$contents} if exists $option->{'no-setuid'};
      @{$contents} = grep {  -g $_ } @{$contents} if exists $option->{   'setgid'};
      @{$contents} = grep { !-g $_ } @{$contents} if exists $option->{'no-setgid'};
      @{$contents} = grep {  -k $_ } @{$contents} if exists $option->{   'sticky'};
      @{$contents} = grep { !-k $_ } @{$contents} if exists $option->{'no-sticky'};
      @{$contents} = grep {  -T $_ } @{$contents} if exists $option->{   'text'};
      @{$contents} = grep { !-T $_ } @{$contents} if exists $option->{'no-text'};
      @{$contents} = grep {  -B $_ } @{$contents} if exists $option->{   'bin'};
      @{$contents} = grep { !-B $_ } @{$contents} if exists $option->{'no-bin'};

      @{$contents} = grep { -M $_ >  $option->{'age-gt'} } @{$contents} if exists $option->{'age-gt'};
      @{$contents} = grep { -M $_ <  $option->{'age-lt'} } @{$contents} if exists $option->{'age-lt'};
      @{$contents} = grep { -M $_ >= $option->{'age-ge'} } @{$contents} if exists $option->{'age-ge'};
      @{$contents} = grep { -M $_ <= $option->{'age-le'} } @{$contents} if exists $option->{'age-le'};

      @{$contents} = grep { -A $_ >  $option->{'access-gt'} } @{$contents} if exists $option->{'access-gt'};
      @{$contents} = grep { -A $_ <  $option->{'access-lt'} } @{$contents} if exists $option->{'access-lt'};
      @{$contents} = grep { -A $_ >= $option->{'access-ge'} } @{$contents} if exists $option->{'access-ge'};
      @{$contents} = grep { -A $_ <= $option->{'access-le'} } @{$contents} if exists $option->{'access-le'};

      @{$contents} = grep { -C $_ >  $option->{'inode-gt'} } @{$contents} if exists $option->{'inode-gt'};
      @{$contents} = grep { -C $_ <  $option->{'inode-lt'} } @{$contents} if exists $option->{'inode-lt'};
      @{$contents} = grep { -C $_ >= $option->{'inode-ge'} } @{$contents} if exists $option->{'inode-ge'};
      @{$contents} = grep { -C $_ <= $option->{'inode-le'} } @{$contents} if exists $option->{'inode-le'};

      @{$contents} = sort @{$contents} if exists $option->{'sort'};
    }
  }
}

1;
