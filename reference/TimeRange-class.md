# Time Range

An S4 class to represent time ranges.

## Slots

- `hash`:

  A [`character`](https://rdrr.io/r/base/character.html) string giving
  the 32-byte MD5 hash of the original data file.

## Note

This class inherits from
[`aion::TimeIntervals`](https://packages.tesselle.org/aion/reference/TimeIntervals-class.html).

## Coerce

In the code snippets below, `x` is a `TimeRange` object.

- `as.data.frame(x)`:

  Coerces to a
  [`data.frame`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/data.frame.md).

## Plot

In the code snippets below, `x` is a `TimeRange` object.

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
[`OccurrenceEvents-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/OccurrenceEvents-class.md),
[`PhasesMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/PhasesMCMC-class.md)

## Author

N. Frerebeau
