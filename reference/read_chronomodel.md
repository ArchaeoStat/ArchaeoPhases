# Read ChronoModel Output

Reads MCMC output.

## Usage

``` r
read_chronomodel_events(file, ...)

read_chronomodel_phases(file, ...)

# S4 method for class 'character'
read_chronomodel_events(file, calendar = CE(), sep = ",", dec = ".")

# S4 method for class 'character'
read_chronomodel_phases(file, calendar = CE(), sep = ",", dec = ".")
```

## Arguments

- file:

  A [`character`](https://rdrr.io/r/base/character.html) string giving
  the name of the CSV file which the data are to be read from.

- ...:

  Currently not used.

- calendar:

  A
  [`aion::TimeScale`](https://packages.tesselle.org/aion/reference/TimeScale-class.html)
  object specifying the calendar (see
  [`calendar()`](https://packages.tesselle.org/aion/reference/calendar.html)).
  It should be
  [`CE()`](https://packages.tesselle.org/aion/reference/gregorian.html)
  unless you change the default settings in 'ChronoModel'.

- sep:

  A [`character`](https://rdrr.io/r/base/character.html) string
  specifying the field separator character (see
  [`utils::read.table()`](https://rdrr.io/r/utils/read.table.html)).

- dec:

  A [`character`](https://rdrr.io/r/base/character.html) string
  specifying the character used in the file for decimal points (see
  [`utils::read.table()`](https://rdrr.io/r/utils/read.table.html)).

## Value

An
[`EventsMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/EventsMCMC-class.md)
or a
[`PhasesMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/PhasesMCMC-class.md)
object.

## References

Lanos, Ph., Philippe, A. & Dufresne, Ph. (2015). Chronomodel:
Chronological Modeling of Archaeological Data using Bayesian Statistics.
URL: <https://chronomodel.com/>.

## See also

[`utils::read.table()`](https://rdrr.io/r/utils/read.table.html)

Other read methods:
[`as_coda()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/as_coda.md),
[`as_events()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/as_events.md),
[`as_phases()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/as_phases.md),
[`check`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/check.md),
[`read_bcal()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/read_bcal.md),
[`read_oxcal()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/read_oxcal.md)

## Author

T. S. Dye, N. Frerebeau

## Examples

``` r
if (requireNamespace("ArchaeoData", quietly = TRUE)) {
  ## Construct the paths to the data
  path <- file.path("chronomodel", "ksarakil")
  path_events <- system.file(path, "Chain_all_Events.csv", package = "ArchaeoData")
  path_phases <- system.file(path, "Chain_all_Phases.csv", package = "ArchaeoData")

  ## Import ChronoModel events
  (chrono_events <- read_chronomodel_events(path_events))

  ## Import ChronoModel phases
  (chrono_phases <- read_chronomodel_phases(path_phases))
}
#> <PhasesMCMC>
#> - Number of phases: 4
#> - Number of MCMC samples: 30000
```
