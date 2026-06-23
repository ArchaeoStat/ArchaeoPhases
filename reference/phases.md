# Compute Phases

Constructs the minimum and maximum for a group of events (phase).

## Usage

``` r
phases(x, groups, ...)

# S4 method for class 'EventsMCMC,missing'
phases(x)

# S4 method for class 'EventsMCMC,list'
phases(x, groups)
```

## Arguments

- x:

  An
  [`EventsMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/EventsMCMC-class.md).

- groups:

  A [`list`](https://rdrr.io/r/base/list.html) of (named) vector of
  names or indexes of columns in `x` (see `phases()`).

- ...:

  Currently not used.

## Value

A
[`PhasesMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/PhasesMCMC-class.md)
object.

## Note

The default value of `start` or `end` corresponds to a CSV file exported
from [ChronoModel](https://chronomodel.com/).

## See also

Other phase tools:
[`duration()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/duration.md)

## Author

A. Philippe, M.-A. Vibet, N. Frerebeau

## Examples

``` r
## Coerce to phases
(pha <- as_phases(mcmc_phases, calendar = CE(), start = c(1, 3), iteration = 1))
#> <PhasesMCMC>
#> - Number of phases: 2
#> - Number of MCMC samples: 30000
summary(pha, calendar = CE())
#> $P1
#>           mad mean  sd   min   q1 median   q3  max start  end
#> start    -708 -773 148 -1349 -890   -749 -671 -207 -1059 -501
#> end      -690 -521 169 -1050 -670   -537 -384   -5  -776 -214
#> duration  278  253 138     1  151    249  345  880     1  487
#> 
#> $P2
#>            mad  mean  sd   min    q1 median    q3   max start   end
#> start    -1766 -1785 100 -2000 -1857  -1785 -1719 -1223 -1981 -1611
#> end      -1240 -1235  87 -1833 -1289  -1235 -1181  -719 -1404 -1067
#> duration   561   551 132     5   464    552   639  1157   297   806
#> 

## Plot phases
plot(pha)

plot(pha, succession = "hiatus")

plot(pha, succession = "transition")


## Compute phases from events
(eve <- as_events(mcmc_events, calendar = CE(), iteration = 1))
#> <EventsMCMC>
#> - Number of events: 4
#> - Number of MCMC samples: 30000

## Compute min-max range for all chains
pha1 <- phases(eve)
summary(pha1, calendar = CE())
#> $P1
#>            mad  mean  sd   min    q1 median    q3   max start   end
#> start    -1766 -1785 100 -2000 -1857  -1785 -1719 -1223 -1981 -1611
#> end       -690  -521 169 -1050  -670   -537  -384    -5  -776  -214
#> duration  1167  1265 196   483  1116   1253  1411  1947   915  1637
#> 

## Compute min-max range by group
pha2 <- phases(eve, groups = list(phase_1 = c(1, 3), phase_2 = c(2, 4)))
summary(pha2, calendar = CE())
#> $phase_1
#>           mad mean  sd   min   q1 median   q3  max start  end
#> start    -708 -773 148 -1349 -890   -749 -671 -207 -1059 -501
#> end      -690 -521 169 -1050 -670   -537 -384   -5  -776 -214
#> duration  278  253 138     1  151    249  345  880     1  487
#> 
#> $phase_2
#>            mad  mean  sd   min    q1 median    q3   max start   end
#> start    -1766 -1785 100 -2000 -1857  -1785 -1719 -1223 -1981 -1611
#> end      -1240 -1235  87 -1833 -1289  -1235 -1181  -719 -1404 -1067
#> duration   561   551 132     5   464    552   639  1157   297   806
#> 
```
