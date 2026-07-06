# Coerce to an Array

Coerce to an Array

## Usage

``` r
# S4 method for class 'MCMC'
as.matrix(x, ...)

# S4 method for class 'PhasesMCMC'
as.array(x, ...)
```

## Arguments

- x:

  An R object.

- ...:

  Currently not used.

## Value

An [`array`](https://rdrr.io/r/base/array.html) or a
[`matrix`](https://rdrr.io/r/base/matrix.html).

## Note

June 2026: aperm.default() now copy attributes, this change how apply()
works on classed objects. The simplest "fix" is to add an aperm() S3
method for the class which drops attributes as aperm.default()
previously did. Alternatively, one could also consider providing
as.matrix()/as.array() methods which change to standard matrix/array.

## Author

N. Frerebeau
