
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ArchaeoPhases

<!-- badges: start -->
<!-- badges: end -->

**This repository contains a heavily modified fork of the
*ArchaeoPhases* package. You might be interested in the [original
package](https://github.com/ArchaeoStat/ArchaeoPhases).**

## Overview

Statistical analysis of archaeological dates and groups of dates.
**ArchaeoPhases** allows to post-process Markov Chain Monte Carlo (MCMC)
simulations from [ChronoModel](https://chronomodel.com),
[Oxcal](https://c14.arch.ox.ac.uk/oxcal.html) or
[BCal](https://bcal.shef.ac.uk). It provides functions for the
estimation and visualization of time ranges from the posterior
distribution of a series of dates (e.g. transition and hiatus between
successive phases).

To cite **ArchaeoPhases** in publications please use:

> Philippe, A. & Vibet, M.-A. (2020). Analysis of Archaeological Phases
> Using the R Package ArchaeoPhases. *Journal of Statistical Software,
> Code Snippets*, 93(1), 1-25. DOI
> [10.18637/jss.v093.c01](https://doi.org/10.18637/jss.v093.c01).

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

## Usage

``` r
## Load packages
library(ArchaeoPhases)

library(ggplot2)
library(magrittr)
```

Import a CSV file containing a sample from the posterior distribution:

``` r
## Read output from ChronoModel
path_zip <- system.file("extdata/chronomodel.zip", package = "ArchaeoPhases")
path_csv <- utils::unzip(path_zip, exdir = tempdir())

chrono_events <- read_chronomodel(path_csv[[1]])
chrono_phases <- read_chronomodel(path_csv[[2]])
```

**ArchaeoPhases** uses
[**ggplot2**](https://github.com/tidyverse/ggplot2) for plotting
information. This makes it easy to customize diagrams (e.g. using themes
and scales).

### Analysis of a series of dates

``` r
plot(chrono_events) +
  ggplot2::theme_bw()
```

![](man/figures/README-events-plot-1.png)<!-- -->

``` r
## Tempo plot
tp <- tempo(chrono_events, level = 0.95)
plot(tp) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")

## Activity plot
ac <- activity(chrono_events)
plot(ac) +
  ggplot2::theme_bw()
```

![](man/figures/README-tempo-plot-1.png)![](man/figures/README-tempo-plot-2.png)

### Analysis of a series of phases

``` r
## Get phases
phases <- as_phases(chrono_phases)
names(phases) <- c("EPI", "UP", "Ahmarian", "IUP")

plot(phases) +
  ggplot2::theme_bw()
```

![](man/figures/README-phases-plot-1.png)<!-- -->

``` r
## Set chronological order
set_order(phases) <- c("IUP", "Ahmarian", "UP", "EPI")
get_order(phases)
#> [1] IUP      Ahmarian UP       EPI     
#> Levels: IUP < Ahmarian < UP < EPI
```

``` r
plot(phases, level = 0.95) +
  ggplot2::theme_bw()
```

![](man/figures/README-succession-plot-1.png)<!-- -->
