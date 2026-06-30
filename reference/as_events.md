# Coerce to Events

Coerce to Events

## Usage

``` r
as_events(from, ...)

# S4 method for class 'matrix'
as_events(from, calendar, iteration = NULL)

# S4 method for class 'data.frame'
as_events(from, calendar, iteration = NULL)
```

## Arguments

- from:

  from An object to be coerced.

- ...:

  Currently not used.

- calendar:

  A
  [`aion::TimeScale`](https://packages.tesselle.org/aion/reference/TimeScale-class.html)
  object specifying the source calendar (see
  [`calendar()`](https://packages.tesselle.org/aion/reference/calendar.html)).

- iteration:

  A length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  specifying the index of the iteration column.

## Value

An
[`EventsMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/EventsMCMC-class.md)
object.

## See also

Other read methods:
[`as_coda()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/as_coda.md),
[`as_phases()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/as_phases.md),
[`check`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/check.md),
[`read_bcal()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/read_bcal.md),
[`read_chronomodel`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/read_chronomodel.md),
[`read_oxcal()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/read_oxcal.md)

## Author

A. Philippe, M.-A. Vibet, N. Frerebeau

## Examples

``` r
## Coerce to events
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)

## Plot first event
plot(eve[, 1], interval = "hdr")


## Colorfull plot
plot(eve, col.density = c("#4477AA", "#EE6677", "#228833", "#CCBB44"))


## Plot events
plot(eve, calendar = CE(), interval = "credible", level = 0.68)

plot(eve, calendar = BP(), interval = "hdr", level = 0.68)


## Plot only 95% credible interval
plot(eve, density = FALSE, interval = "credible", lwd = 3, tcl = 0)
```
