# Occurrence Plot

A statistical graphic designed for the archaeological study of when
events of a specified kind occurred.

## Usage

``` r
occurrence(object, ...)

# S4 method for class 'EventsMCMC'
occurrence(object, level = 0.95)
```

## Arguments

- object:

  An
  [`EventsMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/EventsMCMC-class.md)
  object.

- ...:

  Currently not used.

- level:

  A length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the confidence level.

## Value

An
[`OccurrenceEvents`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/OccurrenceEvents-class.md)
object.

An
[`OccurrenceEvents`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/OccurrenceEvents-class.md)
object.

## Details

If we have \\k\\ events, then we can estimate the calendar date \\t\\
corresponding to the smallest date such that the number of events
observed before \\t\\ is equal to \\k\\.

The `occurrence()` estimates these occurrences and gives the credible
interval or the highest posterior density (HPD) region for a given
`level` of confidence.

## See also

Other event tools:
[`activity()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/activity.md),
[`elapse()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/elapse.md),
[`tempo()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/tempo.md)

## Author

A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau

## Examples

``` r
## Coerce to MCMC
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:10000, ]

## Occurrence plot
occ <- occurrence(eve)
plot(occ, panel.first = graphics::grid())
```
