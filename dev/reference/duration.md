# Phase Duration

Phase Duration

## Usage

``` r
duration(x, y, ...)

# S4 method for class 'numeric,numeric'
duration(x, y)

# S4 method for class 'PhasesMCMC,missing'
duration(x)
```

## Arguments

- x, y:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector. If `y` is
  missing, `x` must be an
  [`PhasesMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/PhasesMCMC-class.md)
  object.

- ...:

  Currently not used.

## See also

Other phase tools:
[`phases()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/phases.md)

## Author

A. Philippe, M.-A. Vibet, N. Frerebeau

## Examples

``` r
## Coerce to phases
pha <- as_phases(mcmc_phases, start = c(1, 3), calendar = CE(), iteration = 1)

## Compute phase duration
dur <- duration(pha)
summary(dur)
#>    mad mean  sd min  q1 median  q3  max start end
#> P1 278  253 138   1 151    249 345  880     1 487
#> P2 561  551 132   5 464    552 639 1157   297 806
```
