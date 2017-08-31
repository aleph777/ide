# File::SLOC --- Provides a SLOC object based on sloccount -*-Perl-*-

#              Copyright © 2015-2017 Tom Fontaine

#
# Author:      Tom Fontaine
# Date:        17-Apr-2015
# Time-stamp: <19-Jan-2017 11:25:30 EST, modified by Tom Fontaine>
#

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
package File::SLOC;

require 5.008;
use Carp;
use strict;
use File::Basename;
use File::Hash;
use List::MoreUtils qw(uniq);
use List::Util qw(sum);
use Text::Format;

# use constant FOO => 'BAR';

our $AUTOLOAD;

my @AREF = qw(details summary);
my @HREF = qw();

my %fields = (details     => undef,
              summary     => undef,

              dirs        => undef,
              files       => undef,

              outdir      => '.',
              fileDetails => 'sloc.details.csv',
              fileSummary => 'sloc.summary.csv',

              factor      => 2.8,
              exponent    => 1.2,
              salary      => 100*1000,

              delimiter   => "\t",
             );

my @parseDetailKeys  = qw(SLOC LANGUAGE DIRECTORY FILE);
my @printDetailKeys  = qw(DIRECTORY FILE LANGUAGE SLOC);
my @putssSummaryKeys = qw(MAN_YEARS MAN_MONTHS SCHEDULE_YEARS SCHEDULE_MONTHS NUMBER_OF_DEVELOPERS TOTAL_COST);
my @printSummaryKeys = qw(DEVELOPMENT_EFFORT SCHEDULE DEVELOPERS COST);

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

  my $__ME__ = (caller(0))[3];

  my $details  = exists $parm{details}  ? $parm{details}  : $this->{details};
  my $summary  = exists $parm{summary}  ? $parm{summary}  : $this->{summary};
  my $dirs     = exists $parm{dirs}     ? $parm{dirs}     : $this->{dirs};
  my $files    = exists $parm{files}    ? $parm{files}    : $this->{files};
  my $factor   = exists $parm{factor}   ? $parm{factor}   : $this->{factor};
  my $exponent = exists $parm{exponent} ? $parm{exponent} : $this->{exponent};
  my $salary   = exists $parm{salary}   ? $parm{salary}   : $this->{salary};

  my @dirs;

  if(ref $dirs)
  {
    if(ref $dirs eq 'ARRAY')
    {
      push @dirs,@{$dirs};
    }
    else
    {
      die "$__ME__: attribute 'dirs' should be ",ref($dirs),"!!!\n";
    }
  }
  else
  {
    push @dirs,split ' ',$dirs;
  }
  if(ref $files)
  {
    if(ref $files eq 'ARRAY')
    {
      push @dirs,@{$files};
    }
    else
    {
      die "$__ME__: attribute 'files' should be ",ref($files),"!!!\n";
    }
  }
  else
  {
    push @dirs,split ' ',$files;
  }
  for(@dirs) {die "$__ME__: $_ does not exist!!!\n"  unless -e $_;}
  die "$__ME__: no valid directories specified!!!\n" unless @dirs;
  die "$__ME__: non-unique directory basenames not supported!!!\n" if scalar(grep { -d } @dirs) != scalar(uniq grep { -d } @dirs);

  my @details = qx(sloccount --details @dirs 2>>/dev/null);
  my @summary = qx(sloccount --effort $factor $exponent --personcost $salary @dirs 2>>/dev/null);

  ProcessDetails(\@details,$details);
  ProcessSummary(\@summary,$summary);
}

sub put
{
  my $this = shift;
  my %parm = @_;

  my $details     = exists $parm{details}     ? $parm{details}     : $this->{details};
  my $summary     = exists $parm{summary}     ? $parm{summary}     : $this->{summary};
  my $outdir      = exists $parm{outdir}      ? $parm{outdir}      : $this->{outdir};
  my $fileDetails = exists $parm{fileDetails} ? $parm{fileDetails} : $this->{fileDetails};
  my $fileSummary = exists $parm{fileSummary} ? $parm{fileSummary} : $this->{fileSummary};
  my $delimiter   = exists $parm{delimiter}   ? $parm{delimiter}   : $this->{delimiter};

  my $fhDetails = File::Hash->new(type => 'FIRST',delimiter => $delimiter,keys => \@printDetailKeys);
  my $fhSummary = File::Hash->new(type => 'FIRST',delimiter => $delimiter,keys => \@putssSummaryKeys);

  $fhDetails->put(contents => $details,basedir => $outdir,basename => $fileDetails);
  $fhSummary->put(contents => $summary,basedir => $outdir,basename => $fileSummary);
}

sub show
{
  my $this = shift;
  my %parm = @_;

  my $details   = exists $parm{details}   ? $parm{details}   : $this->{details};
  my $summary   = exists $parm{summary}   ? $parm{summary}   : $this->{summary};

  my $formatDetails = Text::Format->new(keys     => \@printDetailKeys,
                                        leftKeys => [qw(FILE LANGUAGE DIRECTORY)],
                                        contents => $details,
                                        useKeys  => 1,
                                        newline  => 1);
  my $fd = $formatDetails->get();

  printf $fd,@printDetailKeys;
  printf $fd,@{$_}{@printDetailKeys} for @{$details};
  print "\nTotal SLOC: ",sum(map { $_->{SLOC} } @{$details}),"\n\n";

  print map { $_,"\n" } @{$_}{@printSummaryKeys} for @{$summary};
}

sub ProcessDetails
{
  my ($text,$details) = @_;

  chomp @{$text};

  @{$details} = ();

  for(reverse @{$text})
  {
    last if /^$/;

    my $r = {};

    @{$r}{@parseDetailKeys} = split /\s+/,$_,4;

    $r->{DIRECTORY} = dirname($r->{FILE});
    $r->{FILE}      = basename($r->{FILE});

    push @{$details},$r;
  }
  @{$details} = sort { $a->{DIRECTORY} cmp $b->{DIRECTORY} ||
                        $a->{LANGUAGE}  cmp $b->{LANGUAGE}  ||
                        $a->{FILE}      cmp $b->{FILE} } @{$details};

}

sub ProcessSummary
{
  my ($text,$summary) = @_;

  chomp @{$text};

  @{$summary} = ();

  my $r = {};

  for(grep /Estimate/,@{$text})
  {
    if(/^Development Effort Estimate/)
    {
      $r->{DEVELOPMENT_EFFORT} = $_;

      ($r->{MAN_YEARS},$r->{MAN_MONTHS}) = /(\d+\.\d\d) \((\d+\.\d\d)\)$/;
    }
    elsif(/^Schedule Estimate/)
    {
      $r->{SCHEDULE} = $_;

      ($r->{SCHEDULE_YEARS},$r->{SCHEDULE_MONTHS}) = /(\d+\.\d\d) \((\d+\.\d\d)\)$/;
    }
    elsif(/^Estimated Average Number of Developers/)
    {
      $r->{DEVELOPERS} = $_;

      ($r->{NUMBER_OF_DEVELOPERS}) = /(\d+\.\d\d)$/;
    }
    elsif(/^Total Estimated Cost/)
    {
      $r->{COST} =  $_;
      $r->{COST} =~ s/\$ +/\$/;

      ($r->{TOTAL_COST}) = /\$ (.+)$/;

      $r->{TOTAL_COST} =~ s/,//g;
    }
  }
  push @{$summary},$r;
}

1;
