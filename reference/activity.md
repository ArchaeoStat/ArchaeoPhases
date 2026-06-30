# Activity Plot

Plots the first derivative of the
[`tempo`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/tempo.md)
plot Bayesian estimate.

## Usage

``` r
activity(object, ...)

# S4 method for class 'EventsMCMC'
activity(
  object,
  from = min(object),
  to = max(object),
  grid = getOption("ArchaeoPhases.grid")
)

# S4 method for class 'CumulativeEvents'
activity(object)

# S4 method for class 'ActivityEvents,missing'
plot(
  x,
  calendar = get_calendar(),
  main = NULL,
  sub = NULL,
  ann = graphics::par("ann"),
  axes = TRUE,
  frame.plot = axes,
  panel.first = NULL,
  panel.last = NULL,
  ...
)
```

## Arguments

- object:

  An
  [`EventsMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/EventsMCMC-class.md)
  or a
  [`CumulativeEvents`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/CumulativeEvents-class.md)
  object.

- ...:

  Other [graphical parameters](https://rdrr.io/r/graphics/par.html) may
  also be passed as arguments to this function, particularly, `border`,
  `col`, `lwd` or `lty`.

- from:

  A length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the earliest date to estimate for (expressed in *[rata
  die](https://packages.tesselle.org/aion/reference/RataDie-class.html)*).

- to:

  A length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the latest date to estimate for (expressed in *[rata
  die](https://packages.tesselle.org/aion/reference/RataDie-class.html)*.

- grid:

  A length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  specifying the number of equally spaced points of the temporal grid.

- x:

  An
  [`ActivityEvents`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/ActivityEvents-class.md)
  object.

- calendar:

  A
  [`aion::TimeScale`](https://packages.tesselle.org/aion/reference/TimeScale-class.html)
  object specifying the target calendar (see
  [`calendar()`](https://packages.tesselle.org/aion/reference/calendar.html)).

- main:

  A [`character`](https://rdrr.io/r/base/character.html) string giving a
  main title for the plot.

- sub:

  A [`character`](https://rdrr.io/r/base/character.html) string giving a
  subtitle for the plot.

- ann:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should the
  default annotation (title and x and y axis labels) appear on the plot?

- axes:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should axes
  be drawn on the plot?

- frame.plot:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should a
  box be drawn around the plot?

- panel.first:

  An an `expression` to be evaluated after the plot axes are set up but
  before any plotting takes place. This can be useful for drawing
  background grids.

- panel.last:

  An `expression` to be evaluated after plotting has taken place but
  before the axes, title and box are added.

## Value

- `activity()` returns an
  [`ActivityEvents`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/ActivityEvents-class.md)
  object.

- [`plot()`](https://packages.tesselle.org/aion/reference/plot.html) is
  called it for its side-effects: it results in a graphic being
  displayed (invisibly returns `x`).

## References

Dye, T. S. (2016). Long-term rhythms in the development of Hawaiian
social stratification. *Journal of Archaeological Science*, 71: 1-9.
[doi:10.1016/j.jas.2016.05.006](https://doi.org/10.1016/j.jas.2016.05.006)
.

## See also

Other event tools:
[`elapse()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/elapse.md),
[`occurrence()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/occurrence.md),
[`tempo()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/tempo.md)

## Author

A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau

## Examples

``` r
## Coerce to MCMC
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:10000, ]

## Tempo plot
tmp <- tempo(eve)
plot(tmp, interval = "credible", panel.first = grid())

plot(tmp, interval = "gauss", panel.first = grid())


## Activity plot
act <- activity(tmp)
plot(act, panel.first = grid())
```
