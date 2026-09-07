# File::Core --- Textfile finder for Core repo -*-Perl-*-

#         Copyright © 2026-2026 Thhomas Fontaine

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

# Code:

package File::Core;

use Carp;
use Modern::Perl;

use File::Tmp;

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

use constant _ARG_MAX_ => substr((qx(getconf ARG_MAX))[0],0,-1);
use constant _NPROC_   => substr((qx(nproc)),0,-1);

our $AUTOLOAD;

my @AREF = qw(contents dir);
my @HREF = qw();

my @no_name = qw(adminconsole
                 [Dd]ebugger
                 doc
                 emacs
                 (?:ex|s)amples
                 \.git
                 history
                 LICENSE
                 metadata
                 node_modules
                 opensource
                 [Ss]im
                 stage/dpkg\.
                 yocto);

my @no_suffix = qw([[:xdigit:]]+\.js
                   a
                   [Bb]ak
                   bnf
                   bodies
                   cab
                   d
                   diagram
                   dll
                   elf
                   gif
                   hex
                   jpe?g
                   map
                   md
                   pdf
                   pdf
                   png
                   PNG
                   so
                   xcf
                   zip);

my %fields = (contents => undef,
              dirs     => undef,

              filter   => '-no-link -no-dir -no-bin -no-zero-size',
              ignore   => join('|',@no_name),
              ignore1  => join('|',@no_suffix),
              charset  => '',
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

sub get
{
  my $this = shift;
  my %parm = @_;

  my $_SELF_ = join '::',_ME_,(caller(0))[3];

  my $contents = exists $parm{contents} ? $parm{contents} : $this->{contents};
  my $dirs     = exists $parm{dirs}     ? $parm{dirs}     : $this->{dirs};
  my $filter   = exists $parm{filter}   ? $parm{filter}   : $this->{filter};
  my $ignore   = exists $parm{ignore}   ? $parm{ignore}   : $this->{ignore};
  my $ignore1  = exists $parm{ignore1}  ? $parm{ignore1}  : $this->{ignore1};
  my $charset  = exists $parm{charset}  ? $parm{charset}  : $this->{charset};

  my $no_name   = join ' ','-no-name',join('',"'(?:",$ignore,")'");
  my $no_suffix = defined $ignore1 && $ignore1 ? join ' ','-no-suffix',join('',"'(?:",$ignore1,")'") : '';

  my $cmd = qq(pfind @{$dirs} $filter $no_name $no_suffix);

  @{$contents} = ();

  if ($charset)
  {
    my @files = grep !/'/,qx($cmd);  # !!!

    s/ /\ /g for @files;             # !!!

    my $xargs = join ' ','xargs -P',_NPROC_,'-n 1 file -i';

    my $tmp = File::Tmp->new(contents => \@files,suffix => 'core.s2');

    $tmp->put();

     @{$contents} = sort map { (split ': ',$_)[0] } grep /:\s+$charset/o,qx(cat $tmp->{path} | $xargs);
  }
  else
  {
    @{$contents} = sort qx($cmd);

    chomp @{$contents};
  }
}

1;
