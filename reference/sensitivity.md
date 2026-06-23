# Sensitivity

Calculates the ranges of summary statistics from the output of two or
more runs of the MCMC algorithm.

## Usage

``` r
sensitivity(...)

# S4 method for class 'EventsMCMC'
sensitivity(..., positions = NULL, level = 0.95)
```

## Arguments

- ...:

  Any
  [`EventsMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/EventsMCMC-class.md)
  object.

- positions:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector specifying
  the positions of the columns corresponding to the MCMC chains of
  interest, or a [`character`](https://rdrr.io/r/base/character.html)
  vector of column names.

- level:

  A length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the confidence level.

## Value

A
[`data.frame`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/data.frame.md).

## Details

This function is useful for estimating the sensitivity of calibration
results to different model parameters.

## See also

[`summary()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/summary.md)

Other statistics:
[`interval_credible()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/interval_credible.md),
[`interval_hdr()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/interval_hdr.md),
[`summary()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/summary.md)

## Author

T. S. Dye, N. Frerebeau

## Examples

``` r
## Coerce to MCMC
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)

## Returns 0's
sensitivity(eve, eve)
#>    mad mean sd min q1 median q3 max start end
#> E1   0    0  0   0  0      0  0   0     0   0
#> E2   0    0  0   0  0      0  0   0     0   0
#> E3   0    0  0   0  0      0  0   0     0   0
#> E4   0    0  0   0  0      0  0   0     0   0
```
