# Check for an Original MCMC File

Checks whether or not a file is identical to the one used to create an
object.

## Usage

``` r
is_original(object, ...)

# S4 method for class 'MCMC'
is_original(object, file, download = FALSE)

# S4 method for class 'PhasesMCMC'
is_original(object, file, download = FALSE)

# S4 method for class 'CumulativeEvents'
is_original(object, file, download = FALSE)

# S4 method for class 'ActivityEvents'
is_original(object, file, download = FALSE)

# S4 method for class 'OccurrenceEvents'
is_original(object, file, download = FALSE)
```

## Arguments

- object:

  An object (typically an
  [`MCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/MCMC-class.md)
  object).

- ...:

  Currently not used.

- file:

  Either a path to a CSV file or a connection.

- download:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should the
  remote file be downloaded and hashed locally?

## Value

A [`logical`](https://rdrr.io/r/base/logical.html): `TRUE` if the files
match, `FALSE` otherwise.

## See also

[`digest::digest()`](https://eddelbuettel.github.io/digest/man/digest.html)

Other read methods:
[`as_coda()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/as_coda.md),
[`as_events()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/as_events.md),
[`as_phases()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/as_phases.md),
[`read_bcal()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/read_bcal.md),
[`read_chronomodel`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/read_chronomodel.md),
[`read_oxcal()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/read_oxcal.md)

## Author

T. S. Dye, N. Frerebeau

## Examples

``` r
if (FALSE) { # \dontrun{
## Import OxCal Output
path_output <- system.file("oxcal/ksarakil/MCMC_Sample.csv", package = "ArchaeoData")
url_output <- paste0("https://raw.githubusercontent.com/ArchaeoStat/ArchaeoData/master/",
                     "inst/oxcal/ksarakil/MCMC_Sample.csv")

oxcal <- read_oxcal(path_output)

## Check md5 sum
is_original(oxcal, path_output) # Same as local file? TRUE
is_original(oxcal, url_output, download = FALSE) # Same as remote file? FALSE
is_original(oxcal, url_output, download = TRUE) # Same as remote file? TRUE
} # }
```
