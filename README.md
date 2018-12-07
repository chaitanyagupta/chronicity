# Chronicity

A natural language date and time parser for Common Lisp.

Inspired by (and copied verbatim from) from [Chronic][], the natural
language date and time parser for Ruby by Tom Preston-Werner.

[Chronic]: http://chronic.rubyforge.org/

## Download and Installation

Use [quicklisp][]:

    (ql:quickload "chronicity")

Or get the cutting-edge version from github:
  https://github.com/chaitanyagupta/chronicity/tree/master

Or get the latest stable release (usually updates before quicklisp):
  https://github.com/chaitanyagupta/chronicity/releases

[quicklisp]: https://www.quicklisp.org

## Usage

Use `CHRONICITY:PARSE` to parse date/time strings.

If `*NOW*` is not set, "now" is assumed to be this instant. All relative
date/time calculations are made with respect to `*NOW*`.

    (setf chronicity:*now* (chronicity:make-datetime 2009 3 27 12 34 56))
    => @2009-03-27T12:34:56.000000+05:30

    (chronicity:parse "today")
    => @2009-03-27T13:00:00.000000+05:30

    (chronicity:parse "tomorrow" :now (chronicity:make-date 2009 1 1))
    => @2009-01-02T00:00:00.000000+05:30

    (chronicity:parse "3 days from now")
    => @2009-03-30T12:34:56.000000+05:30

    (chronicity:parse "next month")
    => @2009-04-01T00:00:00.000000+05:30

`:ENDIAN-PREFERENCE` indicates which date format to prefer in case of
ambiguity over days and months. `:LITTLE` indicates the format
"dd/mm/yyyy", `:MIDDLE` indicates "mm/dd/yyy". Default is :LITTLE.

    (chronicity:parse "1/2/2003")
    => @2003-02-01T00:00:00.000000+05:30

    (chronicity:parse "1/2/2003" :endian-preference :middle)
    => @2003-01-02T00:00:00.000000+05:30

Default value for `:CONTEXT` is `:FUTURE`.

    (chronicity:parse "April 1st at 12:30 PM")
    => @2009-04-01T12:30:00.000000+05:30

    (chronicity:parse "April 1st at 12:30 PM" :context :past)
    => @2008-04-01T12:30:00.000000+05:30

`CHRONICITY:PARSE` usually returns a DATETIME object. Its attributes can
be accessed using the datetime readers.

    (chronicity:parse "next month" :guess :end)
    => @2009-04-30T23:59:59.000000+05:30

    (values (chronicity:year-of *)
            (chronicity:month-of *)
            (chronicity:day-of *)
            (chronicity:hour-of *)
            (chronicity:minute-of *)
            (chronicity:sec-of *))
    => 2009, 4, 30, 23, 59, 59

Passing `NIL` as the value for `:GUESS` returns a `SPAN`, which is a range
of datetime values.

    (chronicity:parse "next month" :guess nil)
    => #<CHRONICITY::SPAN 2009-04-01T00:00:00.000000+05:30...2009-05-01T00:00:00.000000+05:30>

    (values (chronicity:span-start *)
            (chronicity:span-end *)
            (chronicity:span-end-included-p *))
    => @2009-04-01T00:00:00.000000+05:30,
       @2009-05-01T00:00:00.000000+05:30,
       NIL

Other possible values are `:START`, `:MIDDLE`, or `:END` which return
the start, mid-point or the end of a span respectively.

## Input Examples

Simple

    thursday
    november
    summer
    friday 13:00
    mon 2:35
    4pm
    6 in the morning
    friday 1pm
    sat 7 in the evening
    yesterday
    today
    tomorrow
    this tuesday
    next month
    this morning
    last night
    this second
    yesterday at 4:00
    last friday at 20:00
    last week tuesday
    tomorrow at 6:45pm
    afternoon yesterday
    thursday last week

Complex

    3 years ago
    5 months before now
    7 hours ago
    7 days from now
    1 week hence
    in 3 hours
    1 year ago tomorrow
    3 months ago saturday at 5:00 pm
    7 hours before tomorrow at noon
    3rd wednesday in november
    3rd month next year
    3rd thursday this september
    4th day last week

Specific Dates

    January 5
    dec 25
    may 27th
    October 2006
    oct 06
    jan 3 2010
    february 14, 2004
    3 jan 2000
    17 april 85
    5/27/1979
    27/5/1979
    05/06
    1979-05-27
    Friday
    5
    4:00
    17:00
    0800

Specific Times (many of the above with an added time)

    January 5 at 7pm
    1979-05-27 05:00:00
    etc

## Limitations

Chronicity only works with the current timezone (as returned by
`LOCAL-TIME:*DEFAULT-TIMEZONE*`) . Support for different timezones is
planned for a future release.

The datetime object(s) returned by the parser are of type
`LOCAL-TIME:TIMESTAMP`. Be aware of any limitations that may apply to
them.

Another problem is that parsing ordinals as words is supported except
for `second`, which is ambiguous with second the unit of time. This
should be fixed in a future release.
