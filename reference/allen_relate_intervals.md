# Relate Two or More Observed Intervals

Reads MCMC output to create a data frame suitable for plotting the
observed Allen relation of two intervals.

## Usage

``` r
allen_relate_intervals(mcmc, converse = TRUE)
```

## Arguments

- mcmc:

  An
  [`EventsMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/EventsMCMC-class.md)
  object containing the output of the MCMC algorithm.

- converse:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should
  converse relations be observed?

## Value

A `list` of `data.frame` to be passed to
[`allen_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_plot.md).

## Author

T. S. Dye, N. Frerebeau
