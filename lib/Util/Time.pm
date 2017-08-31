# -*-Perl-*-
#
# Time-stamp:  <18-Jan-2017 18:32:41 EST, modified by Tom Fontaine>
#
# Title:        Util::Time
# Author:       Tom Fontaine
# Date:         14-May-2008
#
# Purpose:      Provides Date/Time utilities
#
# Revision:     09-Feb-2012 removed colons from HHMMSS and LOG (windows incompatible)
#               25-Mar-2015 require 5.008
#
package Util::Time;

require 5.008;
use Carp;
use strict;

use File::IO;
use Text::Hash;
use Time::Local;

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

use constant SECONDS_PER_MINUTE => 60;
use constant MINUTES_PER_HOUR   => 60;
use constant HOURS_PER_DAY      => 24;
use constant DAYS_PER_WEEK      => 7;
use constant SECONDS_PER_HOUR   => SECONDS_PER_MINUTE * MINUTES_PER_HOUR;
use constant SECONDS_PER_DAY    => HOURS_PER_DAY      * SECONDS_PER_HOUR;
use constant SECONDS_PER_WEEK   => DAYS_PER_WEEK      * SECONDS_PER_DAY;

our $AUTOLOAD;

my @AREF = qw();
my @HREF = qw();

my %fields = (time      => undef,

              style     => undef,

              hour      => undef,
              minute    => undef,
              second    => undef,
              month     => undef,
              day       => undef,
              year      => undef,

              dow       => undef,
              yearDay   => undef,
              isDST     => undef,

              Weekday   => undef,
              Wday      => undef,

              Month     => undef,
              Mon       => undef,

              timestamp => undef,
             );

my @weekdays = qw(Sunday Monday Tuesday Wednesday Thursday Friday Saturday);
my @months   = qw(January February March April May June July August September October November December);

my %keys = (LOG       => [qw(year month day hour minute second)],
            HHMMSS    => [qw(hour minute second)],
            MMDDYYYY  => [qw(month day year)],
            DDMMYYYY  => [qw(day month year)],
            YYYYMMDD  => [qw(year month day)],
            DATE      => [qw(Month day year)],
            DAY_DATE  => [qw(Weekday Month day year)],
            DMY       => [qw(day Mon year)],
           );

my %fmt = (LOG       => '%4d%02d%02d_%02d%02d%02d',
           HHMMSS    => '%02d%02d%02d',
           MMDDYYYY  => '%02d/%02d/%4d',
           DDMMYYYY  => '%02d/%02d/%4d',
           YYYYMMDD  => '%4d%02d%02d',
           DATE      => '%s %d, %d',
           DAY_DATE  => '%s %s %d, %d',
           DMY       => '%02d-%s-%4d',
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

  my $time  = exists $parm{'time'} ? $parm{'time'} : $this->{'time'};
  my $style = exists $parm{style}  ? $parm{style}  : $this->{style};

  $time = time unless defined $time;

  $this->{time} = $time;

  my @localtime = localtime($time);

  $this->{second} = $localtime[0];
  $this->{minute} = $localtime[1];
  $this->{hour}   = $localtime[2];
  $this->{day}    = $localtime[3];

  my $month = $localtime[4];

  $this->{month} = $month + 1;
  $this->{Month} = $months[$month];
  $this->{Mon}   = substr($months[$month],0,3);

  $this->{year} = 1900 + $localtime[5];

  $this->{dow}     = $localtime[6];
  $this->{Weekday} = $weekdays[$localtime[6]];
  $this->{Wday}    = substr($weekdays[$localtime[6]],0,3);

  $this->{yearDay} = $localtime[7];
  $this->{isDST}   = $localtime[8];

}
sub add
{
  my $this = shift;
  my %parm = @_;

  my $time = exists $parm{'time'} ? $parm{'time'} : $this->{'time'};

  $time = time unless defined $time;

  $time += $parm{second}                      if exists $parm{second};
  $time += $parm{minute} * SECONDS_PER_MINUTE if exists $parm{minute};
  $time += $parm{hour}   * SECONDS_PER_HOUR   if exists $parm{hour};
  $time += $parm{day}    * SECONDS_PER_DAY    if exists $parm{day};
  $time += $parm{week}   * SECONDS_PER_WEEK   if exists $parm{week};

  if(exists $parm{month} || exists $parm{year})
  {
    my @localtime = localtime($time);

    $localtime[4] += $parm{month} if exists $parm{month};
    $localtime[5] += $parm{year}  if exists $parm{year};

    $time = timelocal(@localtime);
  }
  $this->get(time => $time);
}
sub set
{
  my $this = shift;
  my %parm = @_;

  my @localtime;

  $localtime[0] = exists $parm{second} ? $parm{second}        : $this->{second};
  $localtime[1] = exists $parm{minute} ? $parm{minute}        : $this->{minute};
  $localtime[2] = exists $parm{hour}   ? $parm{hour}          : $this->{hour};
  $localtime[3] = exists $parm{day}    ? $parm{day}           : $this->{day};
  $localtime[4] = exists $parm{month}  ? $parm{month} - 1     : $this->{month};
  $localtime[5] = exists $parm{year}   ? $parm{year}  - 1900  : $this->{year};

  $this->get(time => timelocal(@localtime));
}
sub configureStyle
{
  my $this = shift;

  my ($style,$fmt,$keys) = @_;

  $fmt{$style}  = $fmt;
  $keys{$style} = $keys;

  $this->{style} = $style;
}
sub getTimestamp
{
  my $this  = shift;
  my $style = shift;

  $this->{style}     = $style;

  return $this->{timestamp};
}

1;
