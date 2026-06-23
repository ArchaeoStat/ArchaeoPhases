# MCMC

An S4 class to represent the output of a MCMC algorithm.

## Slots

- `.Data`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) `matrix` giving the
  MCMC samples expressed in *[rata
  die](https://packages.tesselle.org/aion//reference/RataDie-class.html)*.

- `labels`:

  A [`character`](https://rdrr.io/r/base/character.html) vector
  specifying the name of the events.

- `depth`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector giving the
  sample depth.

- `hash`:

  A [`character`](https://rdrr.io/r/base/character.html) string giving
  the 32-byte MD5 hash of the original data file.

## Note

This class inherits from [`matrix`](https://rdrr.io/r/base/matrix.html).

## Subset

In the code snippets below, `x` is a `MCMC` object.

- `x[[i]]`:

  Extracts a single event (one chain) selected by subscript `i`. `i` is
  a length-one [`numeric`](https://rdrr.io/r/base/numeric.html) or
  [`character`](https://rdrr.io/r/base/character.html) vector.

## See also

Other classes:
[`ActivityEvents-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/ActivityEvents-class.md),
[`AgeDepthModel-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/AgeDepthModel-class.md),
[`CumulativeEvents-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/CumulativeEvents-class.md),
[`DurationsMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/DurationsMCMC-class.md),
[`EventsMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/EventsMCMC-class.md),
[`OccurrenceEvents-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/OccurrenceEvents-class.md),
[`PhasesMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/PhasesMCMC-class.md),
[`TimeRange-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/TimeRange-class.md)

## Author

N. Frerebeau
