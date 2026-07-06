# Activity

An S4 class to store the result of an
[activity](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/activity.md)
plot.

## Slots

- `hash`:

  A [`character`](https://rdrr.io/r/base/character.html) string giving
  the 32-byte MD5 hash of the original data file.

## Note

This class inherits from
[`aion::TimeSeries`](https://packages.tesselle.org/aion/reference/TimeSeries-class.html).

## Coerce

In the code snippets below, `x` is an `ActivityEvents` object.

- `as.data.frame(x)`:

  Coerces to a
  [`data.frame`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/data.frame.md).

## See also

Other classes:
[`AgeDepthModel-class`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/AgeDepthModel-class.md),
[`CumulativeEvents-class`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/CumulativeEvents-class.md),
[`DurationsMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/DurationsMCMC-class.md),
[`EventsMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/EventsMCMC-class.md),
[`MCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/MCMC-class.md),
[`OccurrenceEvents-class`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/OccurrenceEvents-class.md),
[`PhasesMCMC-class`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/PhasesMCMC-class.md),
[`TimeRange-class`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/TimeRange-class.md)

## Author

N. Frerebeau
