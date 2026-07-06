# Extract or Replace Parts of an Object

Operators acting on objects to extract or replace parts.

## Usage

``` r
# S4 method for class 'MCMC'
x[i, j, ..., drop = FALSE]

# S4 method for class 'PhasesMCMC'
x[i, j, k, drop = FALSE]
```

## Arguments

- x:

  An object from which to extract element(s) or in which to replace
  element(s).

- i, j, k:

  Indices specifying elements to extract or replace.

- ...:

  Currently not used.

- drop:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should the
  result be coerced to the lowest possible dimension? This only works
  for extracting elements, not for the replacement.

## Value

A subsetted object.

## See also

Other subsetting methods:
[`bind`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/bind.md)

## Author

N. Frerebeau

## Examples

``` r
## Events
(eve <- as_events(mcmc_events, calendar = CE(), iteration = 1))
#> <EventsMCMC>
#> - Number of events: 4
#> - Number of MCMC samples: 30000

eve[1:1000, ] # Select the first 1000 iterations
#> <EventsMCMC>
#> - Number of events: 4
#> - Number of MCMC samples: 1000
eve[, 1:2]    # Select the first 2 events
#> <EventsMCMC>
#> - Number of events: 2
#> - Number of MCMC samples: 30000

cbind2(eve[, 1:2], eve[, 3:4]) # Combine two MCMC objects
#> <MCMC>
#> - Number of events: 4
#> - Number of MCMC samples: 30000
sort(eve, decreasing = TRUE)   # Sort events in descending order
#> <EventsMCMC>
#> - Number of events: 4
#> - Number of MCMC samples: 30000

## Phases
(pha <- as_phases(mcmc_phases, start = c(1, 3), calendar = CE(), iteration = 1))
#> <PhasesMCMC>
#> - Number of phases: 2
#> - Number of MCMC samples: 30000

pha[1:1000, , ]          # Select the first 1000 iterations
#> <PhasesMCMC>
#> - Number of phases: 2
#> - Number of MCMC samples: 1000
pha[, 1, , drop = FALSE] # Select the first phase
#> <PhasesMCMC>
#> - Number of phases: 1
#> - Number of MCMC samples: 30000
```
