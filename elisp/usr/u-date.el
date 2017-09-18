;;; u-date.el --- Date support for GNU Emacs -*- lexical-binding: t; -*- ;; -*-Emacs-Lisp-*-

;;              Copyright © 2016-2017 Tom Fontaine

;;
;; Author:      Tom Fontaine — Copyright © 2016
;; Date:        28-Feb-2016
;; Time-stamp: <18-Jan-2017 12:25:51 EST, modified by Tom Fontaine>
;;

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software",
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; Except as contained in this notice, the name(s of the above copyright
;; holders shall not be used in advertising or otherwise to promote the sale,
;; use or other dealings in this Software without prior written authorization.

;; The software is provided "As Is", without warranty of any kind, express or
;; implied, including but not limited to the warranties of merchantability,
;; fitness for a particular purpose and noninfringement. In no event shall
;; the authors or copyright holders be liable for any claim, damages or other
;; liability, whether in an action of contract, tort or otherwise, arising
;; from, out of or in connection with the software or the use or other
;; dealings in the software.

;;; Commentary:

;; Revision: 11-Aug-2016 Use `format-time-string'
;;           14-Aug-2016 Added `insert-dd-mon-yyyy'
;;                       Added `insert-month-day-year'
;;

;;; Code:

(message "Loading u-date...")
;;
(defconst month-abbrev-list (list "-" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defconst month-list (list "-" "January" "February" "March" "April" "May" "June"
                           "July" "August" "September" "October" "November" "December"))

;; ===== format-time-string =====

;; %Y is the year, %y within the century, %C the century.
;; %G is the year corresponding to the ISO week, %g within the century.
;; %m is the numeric month.
;; %b and %h are the locale’s abbreviated month name, %B the full name.
;;  (%h is not supported on MS-Windows.)
;; %d is the day of the month, zero-padded, %e is blank-padded.
;; %u is the numeric day of week from 1 (Monday) to 7, %w from 0 (Sunday) to 6.
;; %a is the locale’s abbreviated name of the day of week, %A the full name.
;; %U is the week number starting on Sunday, %W starting on Monday,
;;  %V according to ISO 8601.
;; %j is the day of the year.

;; %H is the hour on a 24-hour clock, %I is on a 12-hour clock, %k is like %H
;;  only blank-padded, %l is like %I blank-padded.
;; %p is the locale’s equivalent of either AM or PM.
;; %M is the minute.
;; %S is the second.
;; %N is the nanosecond, %6N the microsecond, %3N the millisecond, etc.
;; %Z is the time zone name, %z is the numeric form.
;; %s is the number of seconds since 1970-01-01 00:00:00 +0000.

;; %c is the locale’s date and time format.
;; %x is the locale’s "preferred" date format.
;; %D is like "%m/%d/%y".
;; %F is the ISO 8601 date format (like "%Y-%m-%d").

;; %R is like "%H:%M", %T is like "%H:%M:%S", %r is like "%I:%M:%S %p".
;; %X is the locale’s "preferred" time format.

(defun get-dd-mon-yyyy ()
  "Return today's date in dd-mon-yyyy format i.e. '22-Jun-2000'."
  (format-time-string "%d-%b-%Y"))

(defun insert-dd-mon-yyyy ()
  "Insert today's date at point in dd-mon-yyyy format i.e. '22-Jun-2000'."
  (interactive "*")
  (insert (get-dd-mon-yyyy)))

(defun get-month-day-year ()
  "Return today's date in month day, year format i.e. 'June 22, 2000'."
  (format-time-string "%B %d, %Y"))

(defun insert-month-day-year ()
   "Insert today's date in month day, year format i.e. 'June 22, 2000'."
  (interactive "*")
   (insert (get-month-day-year)))

;;
(message "Loading u-date...done")
(provide 'u-date)

;;; u-date.el ends here
