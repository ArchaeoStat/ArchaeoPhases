
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ArchaeoPhases

<!-- badges: start -->

[![R-CMD-check](https://github.com/nfrerebeau/ArchaeoPhases/workflows/R-CMD-check/badge.svg)](https://github.com/nfrerebeau/ArchaeoPhases/actions)

[![CRAN
Version](http://www.r-pkg.org/badges/version/ArchaeoPhases)](https://cran.r-project.org/package=ArchaeoPhases)
[![CRAN
checks](https://cranchecks.info/badges/worst/ArchaeoPhases)](https://cran.r-project.org/web/checks/check_results_ArchaeoPhases.html)
[![CRAN
Downloads](http://cranlogs.r-pkg.org/badges/ArchaeoPhases)](https://cran.r-project.org/package=ArchaeoPhases)

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

[![DOI
JSS](https://img.shields.io/badge/JSS-10.18637/jss.v093.c01-brightgreen)](https://doi.org/10.18637/jss.v093.c01)
<!-- badges: end -->

**This repository contains a heavily modified fork of the
*ArchaeoPhases* package. You might be interested in the [original
package](https://github.com/ArchaeoStat/ArchaeoPhases).**

## Overview

Statistical analysis of archaeological dates and groups of dates.
**ArchaeoPhases** allows to post-process Markov Chain Monte Carlo (MCMC)
simulations from [ChronoModel](https://chronomodel.com),
[Oxcal](https://c14.arch.ox.ac.uk/oxcal.html) or
[BCal](https://bcal.shef.ac.uk). This package provides functions for the
study of rhythms of the long term from the posterior distribution of a
series of dates (tempo and activity plot). It also allows the estimation
and visualization of time ranges from the posterior distribution of
groups of dates (e.g. duration, transition and hiatus between successive
phases).


    To cite ArchaeoPhases in publications use:

      Philippe, Anne & Vibet, Marie-Anne (2020). Analysis of Archaeological
      Phases Using the R Package ArchaeoPhases. Journal of Statistical
      Software, Code Snippets, 93(1), 1--25. DOI 10.18637/jss.v093.c01.

    Une entrée BibTeX pour les utilisateurs LaTeX est

      @Article{,
        title = {Analysis of Archaeological Phases Using the {R} Package {ArchaeoPhases}},
        author = {Anne Philippe and Marie-Anne Vibet},
        year = {2020},
        journal = {Journal of Statistical Software, Code Snippets},
        volume = {93},
        number = {1},
        page = {1--25},
        doi = {10.18637/jss.v093.c01},
      }

## Installation

You can install the released version of **ArchaeoPhases** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ArchaeoPhases")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("nfrerebeau/ArchaeoPhases")
```

## Usage

``` r
## Load packages
library(ArchaeoPhases)
```

Import a CSV file containing a sample from the posterior distribution:

``` r
## Read output from ChronoModel
output_zip <- system.file("extdata/chronomodel.zip", package = "ArchaeoPhases")
output_csv <- utils::unzip(output_zip, exdir = tempdir())

(chrono_events <- read_chronomodel(output_csv[[1]], phases = FALSE))
#> <EventsMCMC>
#> - Number of events: 16
#> - Calendar: CE
(chrono_phases <- read_chronomodel(output_csv[[2]], phases = TRUE))
#> <PhasesMCMC>
#> - Modelled phases: EPI UP Ahmarian IUP
#> - Calendar: CE
```

**ArchaeoPhases** uses
[**ggplot2**](https://github.com/tidyverse/ggplot2) for plotting
information. This makes it easy to customize diagrams (e.g. using themes
and scales).

### Analysis of a series of dates

``` r
plot(chrono_events)
```

<img src="man/figures/README-events-plot-1.png" width="100%" />

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
#>           start    end
#> EPI      -28979 -26970
#> UP       -38570 -29369
#> Ahmarian -42136 -37401
#> IUP      -43240 -41161
```

``` r
plot(chrono_phases)
```

<img src="man/figures/README-phases-plot-1.png" width="100%" />

``` r
## Set chronological order
## (from the oldest to the youngest phase)
set_order(chrono_phases) <- c("IUP", "Ahmarian", "UP", "EPI")
get_order(chrono_phases)
#> [1] "IUP"      "Ahmarian" "UP"       "EPI"
```

``` r
plot(chrono_phases, level = 0.95)
```

<img src="man/figures/README-succession-plot-1.png" width="100%" />
