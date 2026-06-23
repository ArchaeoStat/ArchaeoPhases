# Elapsed Time Scale

Elapsed Time Scale

## Usage

``` r
elapse(object, ...)

# S4 method for class 'MCMC'
elapse(object, origin = 1)
```

## Arguments

- object:

  An object (typically an
  [`MCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/MCMC-class.md)
  object).

- ...:

  Currently not used.

- origin:

  An [`integer`](https://rdrr.io/r/base/integer.html) giving the
  position of the column corresponding to the event from which elapsed
  time is calculated.

## Value

Returns an object of the same class as `object` with an elapsed

An object of the same sort as `object` with a new time scale.

## Note

There is no year \\0\\ in BCE/CE scale.

## See also

Other event tools:
[`activity()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/activity.md),
[`occurrence()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/occurrence.md),
[`tempo()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/tempo.md)

## Author

N. Frerebeau

## Examples

``` r
## Coerce to events
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)

## Elapsed origin
eve_elapse <- elapse(eve, origin = 4)
plot(eve_elapse)
```
