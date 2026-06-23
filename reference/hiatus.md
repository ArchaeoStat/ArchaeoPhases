# Hiatus Between Two Dates

Tests for the existence of a hiatus between two parameters.

## Usage

``` r
hiatus(x, y, ...)

# S4 method for class 'numeric,numeric'
hiatus(x, y, level = 0.95)

# S4 method for class 'EventsMCMC,missing'
hiatus(x, level = 0.95)

# S4 method for class 'PhasesMCMC,missing'
hiatus(x, level = 0.95)
```

## Arguments

- x, y:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector. If `y` is
  missing, `x` must be an
  [`PhasesMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/PhasesMCMC-class.md)
  or an
  [`EventsMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/EventsMCMC-class.md)
  object.

- ...:

  Currently not used.

- level:

  A length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the confidence level.

## Value

The endpoints of the hiatus between successive events/phases (at a given
`level`).

## Details

Finds if a gap exists between two dates and returns the longest interval
that satisfies \\P(x \< HiatusInf \< HiatusSup \< y \| M) = level\\

The hiatus between two successive phases is the longest interval that
satisfies \\P(Phase1Max \< IntervalInf \< IntervalSup \< Phase2Min \| M)
= level\\ (this assumes that the phases are in temporal order
constraint).

## Methods (by class)

- `hiatus(x = numeric, y = numeric)`: Returns a length-three
  [`numeric`](https://rdrr.io/r/base/numeric.html) vector (terminal
  times and hiatus duration, if any).

- `hiatus(x = EventsMCMC, y = missing)`: Returns a
  [`TimeRange`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/TimeRange-class.md)
  object.

- `hiatus(x = PhasesMCMC, y = missing)`: Returns a
  [`TimeRange`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/TimeRange-class.md)
  object.

## See also

Other time ranges:
[`boundaries()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/boundaries.md),
[`transition()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/transition.md)

## Author

A. Philippe, M.-A. Vibet, N. Frerebeau

## Examples

``` r
## Coerce to MCMC
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:10000, ]

## Test for anteriority
older(eve)
#>        E1    E2     E3     E4
#> E1 0.0000 0e+00 0.4925 0.0000
#> E2 1.0000 0e+00 1.0000 0.9996
#> E3 0.5075 0e+00 0.0000 0.0000
#> E4 1.0000 4e-04 1.0000 0.0000

## Test for hiatus
hia <- hiatus(eve)
as.data.frame(hia)
#>   label     start        end  duration
#> 1 E2-E1 -1601.050 -1059.8011 541.24859
#> 2 E4-E1 -1062.599 -1045.3355  17.26366
#> 3 E2-E3 -1603.777  -803.1072 800.67025
#> 4 E4-E3 -1067.124  -793.8029 273.32145
#> 5 E2-E4 -1599.472 -1408.9400 190.53204
```
