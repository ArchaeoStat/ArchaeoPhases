# Coerce to a Data Frame

Coerce to a Data Frame

## Usage

``` r
# S4 method for class 'CumulativeEvents'
as.data.frame(x, ..., calendar = get_calendar())

# S4 method for class 'ActivityEvents'
as.data.frame(x, ..., calendar = get_calendar())

# S4 method for class 'OccurrenceEvents'
as.data.frame(x, ..., calendar = get_calendar())

# S4 method for class 'TimeRange'
as.data.frame(x, ..., calendar = get_calendar())
```

## Arguments

- x:

  An R object.

- ...:

  Further parameters to be passed to `data.frame()`.

- calendar:

  A
  [`aion::TimeScale`](https://packages.tesselle.org/aion//reference/TimeScale-class.html)
  object specifying the target calendar (see
  [`calendar()`](https://packages.tesselle.org/aion//reference/calendar.html)).

## Value

A `data.frame` with an extra `time` column giving the (decimal) years at
which the time series was sampled.

## Author

N. Frerebeau
