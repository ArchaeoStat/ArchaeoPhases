# Read BCal Output

Reads MCMC output.

## Usage

``` r
read_bcal(file, ...)

# S4 method for class 'character'
read_bcal(file, bin_width = 1, calendar = BP())
```

## Arguments

- file:

  A [`character`](https://rdrr.io/r/base/character.html) string giving
  the name of the CSV file which the data are to be read from.

- ...:

  Currently not used.

- bin_width:

  The bin width specified for the [BCal](https://bcal.shef.ac.uk/)
  calibration. Defaults to the BCal default of 1.

- calendar:

  A
  [`aion::TimeScale`](https://packages.tesselle.org/aion/reference/TimeScale-class.html)
  object specifying the calendar (see
  [`calendar()`](https://packages.tesselle.org/aion/reference/calendar.html)).
  It should be
  [`BP()`](https://packages.tesselle.org/aion/reference/gregorian.html)
  unless you change the default settings in 'BCal'.

## Value

An
[`EventsMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/EventsMCMC-class.md)
object.

## References

Buck C. E., Christen J. A. & James G. N. (1999). BCal: an on-line
Bayesian radiocarbon calibration tool. *Internet Archaeology*, 7.
[doi:10.11141/ia.7.1](https://doi.org/10.11141/ia.7.1) .

## See also

[`utils::read.table()`](https://rdrr.io/r/utils/read.table.html)

Other read methods:
[`as_coda()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/as_coda.md),
[`as_events()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/as_events.md),
[`as_phases()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/as_phases.md),
[`check`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/check.md),
[`read_chronomodel`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/read_chronomodel.md),
[`read_oxcal()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/read_oxcal.md)

## Author

T. S. Dye, N. Frerebeau

## Examples

``` r
if (requireNamespace("ArchaeoData", quietly = TRUE)) {
  ## Construct the path to the data
  path_output <- system.file("bcal", "fishpond.csv", package = "ArchaeoData")

  ## Import BCal Output
  (bcal <- read_bcal(path_output))
}
#> <EventsMCMC>
#> - Number of events: 10
#> - Number of MCMC samples: 55964
```
