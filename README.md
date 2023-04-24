
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ArchaeoPhases

<!-- badges: start -->

[![R-CMD-check](https://github.com/ArchaeoStat/ArchaeoPhases/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ArchaeoStat/ArchaeoPhases/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/ArchaeoStat/ArchaeoPhases/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ArchaeoStat/ArchaeoPhases)
[![Dependencies](https://tinyverse.netlify.com/badge/ArchaeoPhases)](https://cran.r-project.org/package=ArchaeoPhases)

<a href="https://ArchaeoStat.r-universe.dev" class="pkgdown-devel"><img
src="https://ArchaeoStat.r-universe.dev/badges/ArchaeoPhases"
alt="r-universe" /></a>
<a href="https://cran.r-project.org/package=ArchaeoPhases"
class="pkgdown-release"><img
src="http://www.r-pkg.org/badges/version/ArchaeoPhases"
alt="CRAN Version" /></a> <a
href="https://cran.r-project.org/web/checks/check_results_ArchaeoPhases.html"
class="pkgdown-release"><img
src="https://badges.cranchecks.info/worst/ArchaeoPhases.svg"
alt="CRAN checks" /></a>
<a href="https://cran.r-project.org/package=ArchaeoPhases"
class="pkgdown-release"><img
src="http://cranlogs.r-pkg.org/badges/ArchaeoPhases"
alt="CRAN Downloads" /></a>

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

[![DOI
JSS](https://img.shields.io/badge/JSS-10.18637/jss.v093.c01-brightgreen)](https://doi.org/10.18637/jss.v093.c01)
<!-- badges: end -->

## Overview

Statistical analysis of archaeological dates and groups of dates.
**ArchaeoPhases** allows to post-process Markov Chain Monte Carlo (MCMC)
simulations from [ChronoModel](https://chronomodel.com) (Lanos et al.
2020), [Oxcal](https://c14.arch.ox.ac.uk/oxcal.html) (Bronk Ramsey 2009)
or [BCal](https://bcal.shef.ac.uk) (Buck, Christen, and James 1999).
This package provides functions for the study of rhythms of the long
term from the posterior distribution of a series of dates (tempo and
activity plot). It also allows the estimation and visualization of time
ranges from the posterior distribution of groups of dates
(e.g. duration, transition and hiatus between successive phases).

**Version 2.0 of ArchaeoPhases introduces a complete rewrite of the
package: all functions have been renamed (see
`news(Version >= "2.0", package = "ArchaeoPhases")`).**


    To cite ArchaeoPhases in publications use:

      Philippe A, Vibet M (2020). "Analysis of Archaeological Phases Using
      the R Package ArchaeoPhases." _Journal of Statistical Software, Code
      Snippets_, *93*(1). doi:10.18637/jss.v093.c01
      <https://doi.org/10.18637/jss.v093.c01>.

    Philippe A, Vibet M, Dye TS, Frerebeau N (2023). _ArchaeoPhases:
    Post-Processing of Markov Chain Monte Carlo Simulations for
    Chronological Modelling_. Université de Nantes, Nantes, France. R
    package version 2.0.0, <https://ArchaeoStat.github.io/ArchaeoPhases>.

    To see these entries in BibTeX format, use 'print(<citation>,
    bibtex=TRUE)', 'toBibtex(.)', or set
    'options(citation.bibtex.max=999)'.

## Installation

You can install the released version of **ArchaeoPhases** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ArchaeoPhases")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ArchaeoStat/ArchaeoPhases")
```

You can install the 1.x releases from the CRAN archives:

``` r
# install.packages("devtools")
devtools::install_version("ArchaeoPhases", version = "1.8")
```

## Usage

These examples use data available through the
[**fasti**](https://packages.tesselle.org/fasti/) package which is
available in a [separate repository](https://tesselle.r-universe.dev).
**fasti** provides MCMC outputs from ChronoModel, OxCal and BCal.

``` r
## Install the latest version
install.packages("fasti", repos = "https://tesselle.r-universe.dev")
```

``` r
## Load package
library(ArchaeoPhases)
```

Import a CSV file containing a sample from the posterior distribution:

``` r
## Read output from ChronoModel
path <- "chronomodel/ksarakil/"

## Events
path_events <- system.file(path, "Chain_all_Events.csv", package = "fasti")
(chrono_events <- read_chronomodel_events(path_events))
#> <EventsMCMC>
#> - Number of events: 16
#> - Time scale: CE

## Phases
path_phases <- system.file(path, "Chain_all_Phases.csv", package = "fasti")
(chrono_phases <- read_chronomodel_phases(path_phases))
#> <PhasesMCMC>
#> - Number of phases: 4
#> - Time scale: CE
```

### Analysis of a series of dates

``` r
## Plot the first event
plot(chrono_events[[1]], interval = "hpdi")

## Plot all events
plot(chrono_events)
```

<img src="man/figures/README-events-plot-1.png" width="50%" /><img src="man/figures/README-events-plot-2.png" width="50%" />

``` r
## Tempo plot
tp <- tempo(chrono_events, level = 0.95)
plot(tp)

## Activity plot
ac <- activity(chrono_events)
plot(ac)
```

<img src="man/figures/README-tempo-plot-1.png" width="50%" /><img src="man/figures/README-tempo-plot-2.png" width="50%" />

### Analysis of a group of dates (phase)

``` r
boundaries(chrono_phases, level = 0.95)
#>              start      stop
#> EPI      -28978.53 -26969.82
#> UP       -38570.37 -29368.75
#> Ahmarian -42168.47 -37433.31
#> IUP      -43240.37 -41161.00
```

``` r
plot(chrono_phases)
```

<img src="man/figures/README-phases-plot-1.png" style="display: block; margin: auto;" />

``` r
plot(chrono_phases[, c("UP", "EPI"), ], range = "hiatus")
```

<img src="man/figures/README-succession-plot-1.png" style="display: block; margin: auto;" />

``` r
plot(chrono_phases[, c("UP", "EPI"), ], range = "transition")
```

<img src="man/figures/README-succession-plot-2.png" style="display: block; margin: auto;" />

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-bronkramsey2009" class="csl-entry">

Bronk Ramsey, Christopher. 2009. “Bayesian Analysis of Radiocarbon
Dates.” *Radiocarbon* 51 (1): 337–60.
<https://doi.org/10.1017/S0033822200033865>.

</div>

<div id="ref-buck1999" class="csl-entry">

Buck, C. E., J. A. Christen, and G. E. James. 1999. “BCal: An on-Line
Bayesian Radiocarbon Calibration Tool.” *Internet Archaeology* 7.
<https://doi.org/10.11141/ia.7.1>.

</div>

<div id="ref-lanos2020" class="csl-entry">

Lanos, Ph., A. Philippe, H. Lanos, and Ph. Dufresne. 2020. *Chronomodel:
Chronological Modeling of Archaeological Data Using Bayesian
Statistics*. CNRS. <https://chronomodel.com>.

</div>

</div>
