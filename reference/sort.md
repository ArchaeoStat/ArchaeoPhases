# Sort an MCMC Object

Sort (or order) an object into ascending or descending temporal order.

## Usage

``` r
# S4 method for class 'MCMC'
sort(x, decreasing = FALSE)

# S4 method for class 'PhasesMCMC'
sort(x, decreasing = FALSE)
```

## Arguments

- x:

  An
  [`MCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/MCMC-class.md)
  object.

- decreasing:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should the
  sort order be decreasing?

## Value

An object of the same sort as `x`.

## See also

Other mutators:
[`names`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/names.md),
[`sort.list()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/sort.list.md)

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
