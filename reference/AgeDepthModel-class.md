# Age-Depth Model

An S4 class to represents an age-depth model.

## Slots

- `depth`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector giving the
  depth of the samples.

- `model`:

  A [`list`](https://rdrr.io/r/base/list.html) of local polynomial
  regressions (see
  [`stats::loess()`](https://rdrr.io/r/stats/loess.html)).

- `hash`:

  A [`character`](https://rdrr.io/r/base/character.html) string giving
  the 32-byte MD5 hash of the original data file.

## See also

Other classes:
[`ActivityEvents-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/ActivityEvents-class.md),
[`CumulativeEvents-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/CumulativeEvents-class.md),
[`DurationsMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/DurationsMCMC-class.md),
[`EventsMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/EventsMCMC-class.md),
[`MCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/MCMC-class.md),
[`OccurrenceEvents-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/OccurrenceEvents-class.md),
[`PhasesMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/PhasesMCMC-class.md),
[`TimeRange-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/TimeRange-class.md)

## Author

N. Frerebeau
