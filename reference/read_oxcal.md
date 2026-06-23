# Read OxCal Output

Reads MCMC output.

## Usage

``` r
read_oxcal(file, ...)

# S4 method for class 'character'
read_oxcal(file, calendar = CE())
```

## Arguments

- file:

  A [`character`](https://rdrr.io/r/base/character.html) string giving
  the name of the CSV file which the data are to be read from.

- ...:

  Currently not used.

- calendar:

  A
  [`aion::TimeScale`](https://packages.tesselle.org/aion//reference/TimeScale-class.html)
  object specifying the calendar (see
  [`calendar()`](https://packages.tesselle.org/aion//reference/calendar.html)).
  It should be
  [`CE()`](https://packages.tesselle.org/aion//reference/gregorian.html)
  unless you change the default settings in 'OxCal'.

## Value

An
[`EventsMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/EventsMCMC-class.md)
object.

## References

Bronk Ramsey, C. (2009). Bayesian Analysis of Radiocarbon Dates.
*Radiocarbon*, 51(1), 337-360.
[doi:10.1017/S0033822200033865](https://doi.org/10.1017/S0033822200033865)
.

## See also

[`utils::read.table()`](https://rdrr.io/r/utils/read.table.html)

Other read methods:
[`as_coda()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/as_coda.md),
[`as_events()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/as_events.md),
[`as_phases()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/as_phases.md),
[`check`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/check.md),
[`read_bcal()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/read_bcal.md),
[`read_chronomodel`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/read_chronomodel.md)

## Author

T. S. Dye, N. Frerebeau

## Examples

``` r
if (requireNamespace("ArchaeoData", quietly = TRUE)) {
  ## Construct the path to the data
  path <- file.path("oxcal", "ksarakil")
  path_output <- system.file(path, "MCMC_Sample.csv", package = "ArchaeoData")

  ## Import OxCal Output
  (oxcal <- read_oxcal(path_output))
}
#> <EventsMCMC>
#> - Number of events: 26
#> - Number of MCMC samples: 1000
```
