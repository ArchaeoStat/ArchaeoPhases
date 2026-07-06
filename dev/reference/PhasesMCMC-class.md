# MCMC Phases

An S4 class to represent a collection of phases.

## Details

A phase object is an\\n x m x 2\\ array, with \\n\\ being the number of
iterations, \\m\\ being the number of phases and with the 2 columns of
the third dimension containing the boundaries of the phases expressed in
*[rata
die](https://packages.tesselle.org/aion/reference/RataDie-class.html)*.

## Slots

- `labels`:

  A [`character`](https://rdrr.io/r/base/character.html) vector
  specifying the name of the phases.

- `hash`:

  A [`character`](https://rdrr.io/r/base/character.html) string giving
  the 32-byte MD5 hash of the original data file.

## Note

This class inherits from [`array`](https://rdrr.io/r/base/array.html).

## Subset

In the code snippets below, `x` is a `PhasesMCMC` object.

- `x[[i]]`:

  Extracts a single phase (two chains) selected by subscript `i`. `i` is
  a length-one [`numeric`](https://rdrr.io/r/base/numeric.html) or
  [`character`](https://rdrr.io/r/base/character.html) vector.

## See also

Other classes:
[`ActivityEvents-class`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ActivityEvents-class.md),
[`AgeDepthModel-class`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/AgeDepthModel-class.md),
[`CumulativeEvents-class`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/CumulativeEvents-class.md),
[`DurationsMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/DurationsMCMC-class.md),
[`EventsMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/EventsMCMC-class.md),
[`MCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/MCMC-class.md),
[`OccurrenceEvents-class`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/OccurrenceEvents-class.md),
[`TimeRange-class`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/TimeRange-class.md)

## Author

N. Frerebeau
