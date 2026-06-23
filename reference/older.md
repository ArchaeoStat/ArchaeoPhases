# Bayesian Test for Anteriority/Posteriority

A Bayesian test for checking the following assumption: "event `x` is
older than event `y`".

## Usage

``` r
older(x, y, ...)

# S4 method for class 'numeric,numeric'
older(x, y)

# S4 method for class 'EventsMCMC,missing'
older(x, y)
```

## Arguments

- x:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector giving the
  output of the MCMC algorithm for the first parameter, or an
  [`EventsMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/EventsMCMC-class.md)
  object.

- y:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector giving the
  output of the MCMC algorithm for the second parameter.

- ...:

  Currently not used.

## Details

For a given output of MCMC algorithm, this function estimates the
posterior probability of the event \\x \< y\\ by the relative frequency
of the event "the value of event `x` is less than the value of event
`y`" in the simulated Markov chain.

## Methods (by class)

- `older(x = numeric, y = numeric)`: Returns a length-one
  [`numeric`](https://rdrr.io/r/base/numeric.html) vector (the posterior
  probability of the assumption: "event `x` is older than event `y`").

- `older(x = EventsMCMC, y = missing)`: Returns a
  [`numeric`](https://rdrr.io/r/base/numeric.html) matrix of posterior
  probabilities.

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
