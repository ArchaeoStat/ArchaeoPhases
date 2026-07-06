# Transition Range Between Successive Phases

Estimates the transition endpoints between two phases.

## Usage

``` r
transition(x, y, ...)

# S4 method for class 'numeric,numeric'
transition(x, y, level = 0.95)

# S4 method for class 'PhasesMCMC,missing'
transition(x, level = 0.95)
```

## Arguments

- x, y:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector. If `y` is
  missing, `x` must be an
  [`PhasesMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/PhasesMCMC-class.md)
  object.

- ...:

  Currently not used.

- level:

  A length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the confidence level.

## Value

The endpoints of the transition interval for each pair of successive
phases (at a given `level`).

## Details

The transition is the shortest interval that satisfies \\P(IntervalInf
\< Phase1Max \< Phase2Min \< IntervalSup \| M) = level\\.

This assumes that the phases are in temporal order constraint.

## Methods (by class)

- `transition(x = numeric, y = numeric)`: Returns a length-two
  [`numeric`](https://rdrr.io/r/base/numeric.html) vector (terminal
  times of the transition interval).

- `transition(x = PhasesMCMC, y = missing)`: Returns a
  [`TimeRange`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/TimeRange-class.md)
  object.

## See also

Other time ranges:
[`boundaries()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/boundaries.md),
[`hiatus()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/hiatus.md)

## Author

A. Philippe, M.-A. Vibet, N. Frerebeau

## Examples

``` r
## Coerce to events
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:10000, ]

## Compute min-max range by group
pha <- phases(eve, groups = list(A = c(1, 3), B = c(2, 4)))

## Compute phase ranges
bou <- boundaries(pha)
as.data.frame(bou)
#>   label     start        end duration
#> 1     A -1046.091  -202.1504 843.9401
#> 2     B -1972.281 -1078.0925 894.1889

## Compute phase transition
tra <- transition(pha)
as.data.frame(tra)
#>   label    start       end duration
#> 1   B-A -1409.47 -501.6004 907.8698

## Compute phase hiatus
hia <- hiatus(pha)
as.data.frame(hia)
#>   label     start       end duration
#> 1   B-A -1062.599 -1046.091 16.50864
```
