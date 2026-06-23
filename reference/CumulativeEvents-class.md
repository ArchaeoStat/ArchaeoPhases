# Cumulative Events

An S4 class to store the result of a
[tempo](https://ArchaeoStat.github.io/ArchaeoPhases/reference/tempo.md)
plot.

## Slots

- `lower`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector giving the
  lower boundaries of the credibility interval expressed in *[rata
  die](https://packages.tesselle.org/aion//reference/RataDie-class.html)*.

- `upper`:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector giving the
  upper boundaries of the credibility interval expressed in *[rata
  die](https://packages.tesselle.org/aion//reference/RataDie-class.html)*.

- `level`:

  A length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the confidence level.

- `gauss`:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar indicating
  if the Gaussian approximation of the credible interval was used.

- `counts`:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar.

- `events`:

  An [`integer`](https://rdrr.io/r/base/integer.html) scalar giving the
  number of events included in the tempo plot.

- `hash`:

  A [`character`](https://rdrr.io/r/base/character.html) string giving
  the 32-byte MD5 hash of the original data file.

## Note

This class inherits from
[`aion::TimeSeries`](https://packages.tesselle.org/aion//reference/TimeSeries-class.html).

## Coerce

In the code snippets below, `x` is a `CumulativeEvents` object.

- `as.data.frame(x)`:

  Coerces to a
  [`data.frame`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/data.frame.md).

## See also

Other classes:
[`ActivityEvents-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/ActivityEvents-class.md),
[`AgeDepthModel-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/AgeDepthModel-class.md),
[`DurationsMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/DurationsMCMC-class.md),
[`EventsMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/EventsMCMC-class.md),
[`MCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/MCMC-class.md),
[`OccurrenceEvents-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/OccurrenceEvents-class.md),
[`PhasesMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/PhasesMCMC-class.md),
[`TimeRange-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/TimeRange-class.md)

## Author

N. Frerebeau
