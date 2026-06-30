# Bayesian Credible Interval

Computes the shortest credible interval of the output of the MCMC
algorithm for a single parameter.

## Usage

``` r
interval_credible(x, ...)

# S4 method for class 'MCMC'
interval_credible(x, level = 0.95, calendar = get_calendar())
```

## Arguments

- x:

  An
  [`MCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/MCMC-class.md)
  object containing the output of the MCMC algorithm.

- ...:

  Currently not used.

- level:

  A length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the confidence level.

- calendar:

  A
  [`aion::TimeScale`](https://packages.tesselle.org/aion/reference/TimeScale-class.html)
  object specifying the target calendar (see
  [`calendar()`](https://packages.tesselle.org/aion/reference/calendar.html)).

## Value

Returns a [`list`](https://rdrr.io/r/base/list.html) of `numeric`
[`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

A \\(100 \times level)\\ % credible interval is an interval that keeps
\\N \times (1 - level)\\ elements of the sample outside the interval.

The \\(100 \times level)\\ % credible interval is the shortest of all
those intervals.

For instance, the 95% credible interval is the central portion of the
posterior distribution that contains 95% of the values.

## See also

[`arkhe::interval_credible()`](https://packages.tesselle.org/arkhe/reference/interval_credible.html)

Other statistics:
[`interval_hdr()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/interval_hdr.md),
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
