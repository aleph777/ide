# Util::Time --- [description] -*-Perl-*-

#         Copyright © 2022-2023 Tom Fontaine

# Date:   16-Sep-2022

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
# The following patterns are allowed in the format string given to the $dt->strftime() method:

#     %a The abbreviated weekday name.
#     %A The full weekday name.
#     %b The abbreviated month name.
#     %B The full month name.
#     %c The default datetime format for the object's locale.
#     %C The century number (year/100) as a 2-digit integer.
#     %d The day of the month as a decimal number (range 01 to 31).
#     %D Equivalent to %m/%d/%y.
#     %e Like %d, the day of the month as a decimal number, but a leading zero is replaced by a space.
#     %F Equivalent to %Y-%m-%d (the ISO 8601 date format)
#     %G The ISO 8601 year with century as a decimal number. The 4-digit year corresponding to the ISO
#        week number (see %V). This has the same format and value as %Y, except that if the ISO week
#        number belongs to the previous or next year, that year is used instead. (TZ)
#     %g Like %G, but without century, i.e., with a 2-digit year (00-99).
#     %h Equivalent to %b.
#     %H The hour as a decimal number using a 24-hour clock (range 00 to 23).
#     %I The hour as a decimal number using a 12-hour clock (range 01 to 12).
#     %j The day of the year as a decimal number (range 001 to 366).
#     %k The hour (24-hour clock) as a decimal number (range 0 to 23); single digits are preceded by a blank. (See also %H.)
#     %l The hour (12-hour clock) as a decimal number (range 1 to 12); single digits are preceded by a blank. (See also %I.)
#     %m The month as a decimal number (range 01 to 12).
#     %M The minute as a decimal number (range 00 to 59).
#     %n A newline character.
#     %N The fractional seconds digits. Default is 9 digits (nanoseconds).

#          %3N   milliseconds (3 digits)
#          %6N   microseconds (6 digits)
#          %9N   nanoseconds  (9 digits)

#        This value will always be rounded down to the nearest integer.
#     %p Either 'AM' or 'PM' according to the given time value, or the corresponding strings for the current
#        locale. Noon is treated as 'pm' and midnight as 'am'.
#     %P Like %p but in lowercase: 'am' or 'pm' or a corresponding string for the current locale.
#     %r The time in a.m. or p.m. notation. In the POSIX locale this is equivalent to '%I:%M:%S %p'.
#     %R The time in 24-hour notation (%H:%M). (SU) For a version including the seconds, see %T below.
#     %s The number of seconds since the epoch.
#     %S The second as a decimal number (range 00 to 61).
#     %t A tab character.
#     %T The time in 24-hour notation (%H:%M:%S).
#     %u The day of the week as a decimal, range 1 to 7, Monday being 1. See also %w.
#     %U The week number of the current year as a decimal number, range 00 to 53, starting with the first Sunday
#        as the first day of week 01. See also %V and %W.
#     %V The ISO 8601:1988 week number of the current year as a decimal number, range 01 to 53, where week 1 is the
#        first week that has at least 4 days in the current year, and with Monday as the first day of the week. See
#        also %U and %W.
#     %w The day of the week as a decimal, range 0 to 6, Sunday being 0. See also %u.
#     %W The week number of the current year as a decimal number, range 00 to 53, starting with the first Monday as
#        the first day of week 01.
#     %x The default date format for the object's locale.
#     %X The default time format for the object's locale.
#     %y The year as a decimal number without a century (range 00 to 99).
#     %Y The year as a decimal number including the century.
#     %z The time-zone as hour offset from UTC. Required to emit RFC822-conformant dates (using "%a, %d %b %Y %H:%M:%S %z").
#     %Z The time zone or name or abbreviation.
#     %% A literal '%' character.
#     %{method} Any method name may be specified using the format %{method} name where "method" is a valid DateTime.pm object method.
#
# $t->sec                 # also available as $t->second
# $t->min                 # also available as $t->minute
# $t->hour                # 24 hour
# $t->mday                # also available as $t->day_of_month
# $t->mon                 # 1 = January
# $t->_mon                # 0 = January
# $t->monname             # Feb
# $t->month               # same as $t->monname
# $t->fullmonth           # February
# $t->year                # based at 0 (year 0 AD is, of course 1 BC)
# $t->_year               # year minus 1900
# $t->yy                  # 2 digit year
# $t->wday                # 1 = Sunday
# $t->_wday               # 0 = Sunday
# $t->day_of_week         # 0 = Sunday
# $t->wdayname            # Tue
# $t->day                 # same as wdayname
# $t->fullday             # Tuesday
# $t->yday                # also available as $t->day_of_year, 0 = Jan 01
# $t->isdst               # also available as $t->daylight_savings

# $t->hms                 # 12:34:56
# $t->hms(".")            # 12.34.56
# $t->time                # same as $t->hms

# $t->ymd                 # 2000-02-29
# $t->date                # same as $t->ymd
# $t->mdy                 # 02-29-2000
# $t->mdy("/")            # 02/29/2000
# $t->dmy                 # 29-02-2000
# $t->dmy(".")            # 29.02.2000
# $t->datetime            # 2000-02-29T12:34:56 (ISO 8601)
# $t->cdate               # Tue Feb 29 12:34:56 2000
# "$t"                    # same as $t->cdate

# $t->epoch               # seconds since the epoch
# $t->tzoffset            # timezone offset in a Time::Seconds object

# $t->julian_day          # number of days since Julian period began
# $t->mjd                 # modified Julian date (JD-2400000.5 days)

# $t->week                # week number (ISO 8601)

# $t->is_leap_year        # true if it's a leap year
# $t->month_last_day      # 28-31

# $t->time_separator($s)  # set the default separator (default ":")
# $t->date_separator($s)  # set the default separator (default "-")
# $t->day_list(@days)     # set the default weekdays
# $t->mon_list(@days)     # set the default months

# $t->strftime(FORMAT)    # same as POSIX::strftime (without the overhead
#                         # of the full POSIX extension)
# $t->strftime()          # "Tue, 29 Feb 2000 12:34:56 GMT"

package Util::Time;

use Carp;
use strict;
use v5.10;

use Time::Piece;

use constant _ME_ => join '::',$0 =~ m=([^/]+)$=,__PACKAGE__;

our $AUTOLOAD;

my @AREF = qw();
my @HREF = qw();

my %fields = (tp => undef,

              MMDDYYYY => "%D",
              DATE_ISO => "%F",
              TOD_AMPM => "%r",
              TOD      => "%T",
              DEF_DATE => "%x",
              DEF_TIME => "%X",

              YYYYMMDD => "%Y%m%d",
              HHMMSS   => "%H%M%S",
              LOG      => "%Y%m%d%H%M%S",
              DATE     => "%B%e, %Y",
              DAY_DATE => "%A, %B%e, %Y",
             );


# BEGIN
# {
#   $lt = localtime;
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

  $this->{tp} = localtime;

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

  $this->{tp} = localtime;
}

1;
