# Coerce to Coda

Extracts parallel chains from an
[`MCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/MCMC-class.md)
object to create an `mcmc.list` object for use with coda diagnostic
tools.

## Usage

``` r
as_coda(from, ...)

# S4 method for class 'MCMC'
as_coda(from, chains = 1)
```

## Arguments

- from:

  from An object to be coerced.

- ...:

  Currently not used.

- chains:

  An [`integer`](https://rdrr.io/r/base/integer.html) specifying the
  number of parallel chains (defaults to \\1\\).

## Value

An [`coda::mcmc.list`](https://rdrr.io/pkg/coda/man/mcmc.list.html)
object.

## See also

[`coda::mcmc()`](https://rdrr.io/pkg/coda/man/mcmc.html),
[`coda::mcmc.list()`](https://rdrr.io/pkg/coda/man/mcmc.list.html)

Other read methods:
[`as_events()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/as_events.md),
[`as_phases()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/as_phases.md),
[`check`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/check.md),
[`read_bcal()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/read_bcal.md),
[`read_chronomodel`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/read_chronomodel.md),
[`read_oxcal()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/read_oxcal.md)

## Author

A. Philippe, M.-A. Vibet

## Examples

``` r
if (requireNamespace("coda", quietly = TRUE)) {
  ## Load coda
  library(coda)

  ## Coerce to MCMC
  eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)

  ## Coerce to coda
  mc <- as_coda(eve[, 1:2], chains = 3)
  plot(mc)

  ## Autocorrelation
  autocorr.plot(mc)

  ## Gelman-Rubin diagnostic
  ## The multivariate criterion can not be evaluated when a phase
  ## contains only one date. This induces colinearity problems.
  gelman.diag(mc)
  gelman.plot(mc)
}




```
