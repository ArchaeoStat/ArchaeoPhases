# The Names of an Object

Get or set the names of an object.

## Usage

``` r
# S4 method for class 'MCMC'
names(x)

# S4 method for class 'MCMC'
names(x) <- value

# S4 method for class 'PhasesMCMC'
names(x)

# S4 method for class 'PhasesMCMC'
names(x) <- value
```

## Arguments

- x:

  An object from which to get or set names.

- value:

  A possible value for the names of `x`.

## Value

An object of the same sort as `x` with the new names assigned.

## See also

Other mutators:
[`sort()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/sort.md),
[`sort.list()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/sort.list.md)

## Author

N. Frerebeau
