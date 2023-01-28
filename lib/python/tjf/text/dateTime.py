# -*-coding: utf-8-*- ; -*-Python-*-

#         Copyright © 2022-2023 Tom Fontaine

# Title:  dateTime.py
# Date:   26-Apr-2022


from datetime import datetime, timezone

class DateTime:
    """"""
    MMDDYYYY = '%D'
    DATE_ISO = '%F'
    TOD_AMPM = '%r'
    TOD      = '%T'
    DEF_DATE = '%x'
    DEF_TIME = '%X'

    YYYYMMDD  = '%Y%m%d'
    HHMMSS    = '%H%M%S'
    HH_MM_SS  = '%H:%M:%S'
    LOG       = '%Y%m%d_%H%M%S'
    TIMESTAMP = '%Y%m%d%H%M%S'
    DATE      = '%B %e, %Y'
    DAY_DATE  = '%A, %B %e, %Y'

    def __init__(self, format=DAY_DATE, timezone='local'):
        self.format   = format
        self.timezone = timezone

        self.year        = None
        self.month       = None
        self.day         = None
        self.hour        = None
        self.minute      = None
        self.second      = None
        self.microsecond = None
        pass


    def now(self):
        now = datetime.now()

        self.year        = now.year
        self.month       = now.month
        self.day         = now.day
        self.hour        = now.hour
        self.minute      = now.minute
        self.second      = now.second
        self.microsecond = now.microsecond

        return now.strftime(self.format)


    def utcnow(self):
        now = datetime.now(timezone.utc)

        self.year        = now.year
        self.month       = now.month
        self.day         = now.day
        self.hour        = now.hour
        self.minute      = now.minute
        self.second      = now.second
        self.microsecond = now.microsecond

        return now.strftime(self.format)


    def set(self, **kwargs):
        """Set private attributes."""
        for key, value in kwargs.items():
            if key == 'format':
                self.format = value
            elif key == 'timezone':
                self.timezone = value
            elif key == 'year':
                self.year = value
            elif key == 'month':
                self.month = value
            elif key == 'day':
                self.day = value
            elif key == 'hour':
                self.hour = value
            elif key == 'minute':
                self.minute = value
            elif key == 'second':
                self.second = value
            elif key == 'microsec':
                self.microsecond = value
            else:
                print('{:s} - oh oh!!!'.format(key))
                exit()

    def get(self):
        dt = datetime(self.year,
                      self.month,
                      self.day,
                      self.hour,
                      self.minute,
                      self.second,
                      self.microsecond)

        return dt.strftime(self.format)
