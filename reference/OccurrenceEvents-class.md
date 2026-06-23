# Occurrence

An S4 class to store the result of an
[occurrence](https://ArchaeoStat.github.io/ArchaeoPhases/reference/occurrence.md)
plot.

## Slots

- `events`:

  An [`integer`](https://rdrr.io/r/base/integer.html) vector giving the
  occurrence.

- `level`:

  A length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the confidence level.

- `hash`:

  A [`character`](https://rdrr.io/r/base/character.html) string giving
  the 32-byte MD5 hash of the original data file.

## Note

This class inherits from
[`aion::TimeIntervals`](https://packages.tesselle.org/aion//reference/TimeIntervals-class.html).

## Coerce

In the code snippets below, `x` is an `OccurrenceEvents` object.

- `as.data.frame(x)`:

  Coerces to a
  [`data.frame`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/data.frame.md).

## Plot

In the code snippets below, `x` is a `OccurrenceEvents` object.

- `plot(x)`:

  Results in a graphic being displayed (invisibly returns `x`).

## See also

Other classes:
[`ActivityEvents-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/ActivityEvents-class.md),
[`AgeDepthModel-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/AgeDepthModel-class.md),
[`CumulativeEvents-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/CumulativeEvents-class.md),
[`DurationsMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/DurationsMCMC-class.md),
[`EventsMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/EventsMCMC-class.md),
[`MCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/MCMC-class.md),
[`PhasesMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/PhasesMCMC-class.md),
[`TimeRange-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/TimeRange-class.md)

## Author

N. Frerebeau
