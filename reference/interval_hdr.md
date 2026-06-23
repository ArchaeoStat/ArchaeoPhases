# Bayesian HPD Regions

Bayesian HPD Regions

## Usage

``` r
interval_hdr(x, y, ...)

# S4 method for class 'MCMC,missing'
interval_hdr(x, level = 0.95, calendar = get_calendar(), ...)
```

## Arguments

- x:

  An
  [`MCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/MCMC-class.md)
  object containing the output of the MCMC algorithm.

- y:

  Currently not used.

- ...:

  Extra arguments to be passed to
  [`stats::density()`](https://rdrr.io/r/stats/density.html).

- level:

  A length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the confidence level.

- calendar:

  A
  [`aion::TimeScale`](https://packages.tesselle.org/aion//reference/TimeScale-class.html)
  object specifying the target calendar (see
  [`calendar()`](https://packages.tesselle.org/aion//reference/calendar.html)).

## Value

Returns a [`list`](https://rdrr.io/r/base/list.html) of `numeric`
[`matrix`](https://rdrr.io/r/base/matrix.html).

## References

Hyndman, R. J. (1996). Computing and graphing highest density regions.
*American Statistician*, 50: 120-126.
[doi:10.2307/2684423](https://doi.org/10.2307/2684423) .

## See also

[`stats::density()`](https://rdrr.io/r/stats/density.html),
[`arkhe::interval_hdr()`](https://packages.tesselle.org/arkhe/reference/interval_hdr.html)

Other statistics:
[`interval_credible()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/interval_credible.md),
[`sensitivity()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/sensitivity.md),
[`summary()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/summary.md)

## Author

A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau

## Examples

``` r
## Coerce to events
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:10000, ]

## Rata die
interval_credible(eve, level = 0.95) # Credible interval
#> $E1
#>         start      end    p
#> [1,] -1045.21 -202.337 0.95
#> 
#> $E2
#>          start      end    p
#> [1,] -1979.809 -1613.59 0.95
#> 
#> $E3
#>         start       end    p
#> [1,] -809.057 -459.0902 0.95
#> 
#> $E4
#>         start      end    p
#> [1,] -1400.04 -1064.29 0.95
#> 
interval_hdr(eve, level = 0.68) # HPD interval
#> $E1
#>           start       end    p
#> [1,] -1009.3041 -774.1172 0.35
#> [2,]  -490.0579 -257.9274 0.33
#> 
#> $E2
#>          start       end    p
#> [1,] -1891.749 -1690.389 0.68
#> 
#> $E3
#>          start       end    p
#> [1,] -757.2022 -604.5305 0.68
#> 
#> $E4
#>          start       end    p
#> [1,] -1311.728 -1149.417 0.68
#> 

## BP
interval_credible(eve, level = 0.95, calendar = BP()) # Credible interval
#> $E1
#>        start      end    p
#> [1,] 2996.79 2153.663 0.95
#> 
#> $E2
#>         start     end    p
#> [1,] 3930.191 3564.41 0.95
#> 
#> $E3
#>         start     end    p
#> [1,] 2760.943 2410.91 0.95
#> 
#> $E4
#>        start     end    p
#> [1,] 3351.96 3015.71 0.95
#> 
interval_hdr(eve, level = 0.95, calendar = BP()) # HPD interval
#> $E1
#>         start      end    p
#> [1,] 3012.773 2144.216 0.95
#> 
#> $E2
#>         start      end    p
#> [1,] 3931.124 3560.483 0.95
#> 
#> $E3
#>         start      end    p
#> [1,] 2756.947 2402.143 0.95
#> 
#> $E4
#>         start      end    p
#> [1,] 3357.589 3017.994 0.95
#> 
```
