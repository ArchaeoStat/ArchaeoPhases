# Changelog

## ArchaeoPhases 2.1.1.9000

## ArchaeoPhases 2.1.1

CRAN release: 2026-07-01

### Bugfixes & changes

- Add [`as.matrix()`](https://rdrr.io/r/base/matrix.html) for `MCMC`
  class.
- Add
  [`as.array()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/as.array.md)
  for `PhasesMCMC` class.

## ArchaeoPhases 2.1.0

CRAN release: 2025-09-26

### Enhancements

- Translate into French.
- [`bury()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/bury.md)
  gained new `span` and `degree` arguments.

### Bug fixes & changes

- Add default calendar to
  [`as_phases()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/as_phases.md).

### Internals

- Use `aion::TimeIntervals-class` for time ranges representation.

## ArchaeoPhases 2.0.0

**ArchaeoPhases** v2.0 brings a comprehensive package rewrite. You can
install the 1.x releases from the CRAN archives:

``` r

# install.packages("remotes")
remotes::install_version("ArchaeoPhases", version = "1.8")
```

### Bug fixes & changes

- Use [`stats::density()`](https://rdrr.io/r/stats/density.html) instead
  of `hdrcde::hdr()` for HDPI estimation.

### Internals

- Use **aion** for internal date representation.

### Breaking changes

- Full rewrite in S4 (see below).
- Remove the Shiny application.

| ArchaeoPhases 1.x | ArchaeoPhases 2.0 |
|:---|:---|
| [`AgeDepth()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`bury()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/bury.md) |
| [`CreateMinMaxGroup()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | `phase()`, [`as_phases()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/as_phases.md) |
| [`CredibleInterval()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md), [`credible_interval()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`interval_credible()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/interval_credible.md) |
| [`DatesHiatus()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md), [`dates_hiatus()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`hiatus()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/hiatus.md) |
| [`estimate_range()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`sensitivity()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/sensitivity.md) |
| [`MarginalPlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md), [`marginal_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`plot()`](https://packages.tesselle.org/aion/reference/plot.html) |
| [`MarginalProba()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`older()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/older.md) |
| [`MarginalStatistics()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md), [`marginal_statistics()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md), [`multi_marginal_statistics()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`summary()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/summary.md) |
| [`MultiCredibleInterval()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md), [`multi_credible_interval()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`interval_credible()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/interval_credible.md) |
| [`MultiDatesPlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md), [`multi_dates_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`plot()`](https://packages.tesselle.org/aion/reference/plot.html) |
| [`MultiHPD()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md), [`multi_hpd()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`interval_hdr()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/interval_hdr.md) |
| [`MultiMarginalPlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md), [`multi_marginal_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`plot()`](https://packages.tesselle.org/aion/reference/plot.html) |
| [`MultiPhasePlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`plot()`](https://packages.tesselle.org/aion/reference/plot.html) |
| [`MultiPhaseTimeRange()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`boundaries()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/boundaries.md) |
| [`MultiPhasesGap()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`hiatus()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/hiatus.md) |
| [`MultiPhasesTransition()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`transition()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/transition.md) |
| [`MultiSuccessionPlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`plot()`](https://packages.tesselle.org/aion/reference/plot.html) |
| [`OccurrencePlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md), [`occurrence_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`occurrence()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/occurrence.md) + [`plot()`](https://packages.tesselle.org/aion/reference/plot.html) |
| [`PhaseDurationPlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`duration()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/duration.md) + [`plot()`](https://packages.tesselle.org/aion/reference/plot.html) |
| [`PhasePlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`plot()`](https://packages.tesselle.org/aion/reference/plot.html) |
| [`PhaseStatistics()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`summary()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/summary.md) |
| [`PhaseTimeRange()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`boundaries()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/boundaries.md) |
| [`PhasesGap()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md), [`phases_gap()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`hiatus()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/hiatus.md) |
| [`PhasesTransition()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`transition()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/transition.md) |
| [`SuccessionPlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`plot()`](https://packages.tesselle.org/aion/reference/plot.html) |
| [`TempoActivityPlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md), [`tempo_activity_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`activity()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/activity.md) + [`plot()`](https://packages.tesselle.org/aion/reference/plot.html) |
| [`TempoPlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md), [`tempo_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`tempo()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/tempo.md) + [`plot()`](https://packages.tesselle.org/aion/reference/plot.html) |
| [`undated_sample()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md) | [`interpolate()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/interpolate.md) |

## ArchaeoPhases 1.6.0

### New functions

- New functions for Allen’s interval algebra:
  [`allen_analyze()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_analyze.md),
  [`allen_joint_concurrency()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_joint_concurrency.md),
  [`allen_observe_frequency()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_observe_frequency.md),
  [`allen_illustrate()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_illustrate.md),
  [`allen_observe()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_observe.md).

### Bugfixes & changes

- Fixed a bug that failed to export the `reproduce()` function.
- Added a vignette on object reproducibility.

## ArchaeoPhases 1.5.0

### New functions

- New read functions for MCMC data:
  [`read_bcal()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/read_bcal.md),
  [`read_oxcal()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/read_oxcal.md),
  [`read_chronomodel()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/read_chronomodel.md).
  - The new read functions return S3 objects that can identify the file
    that produced them.
  - The new read functions are built on `read_csv()`, which can read
    data from a file, connection, or the clipboard.
- New plot functions:
  [`multi_dates_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md),
  [`tempo_activity_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md),
  [`tempo_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md),
  [`marginal_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md),
  [`multi_marginal_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md),
  and
  [`occurrence_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md).
  - The new plot functions are functional replacements for the originals
    with camelCase names, e.g.,
    [`TempoPlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md)
    -\>
    [`tempo_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md).
  - The new plot functions return S3 objects with
    [`plot()`](https://packages.tesselle.org/aion/reference/plot.html)
    and `reproduce()` methods.
  - The S3 objects inherit from `data.frame` and can be passed to
    appropriate statistical functions to summarize the data in the plot.
- New statistical functions:
  [`credible_interval()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md),
  [`multi_credible_interval()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md),
  [`multi_hpd()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md),
  [`dates_hiatus()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md),
  [`phases_gap()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md),
  [`marginal_statistics()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md),
  and `phase_statistics()`.
  - The new statistical functions are functional replacements for the
    originals with camelCase names, e.g.,
    [`CredibleInterval()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md)
    -\>
    [`credible_interval()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md).
  - The new statistical functions return lists in canonical R fashion.
  - The `phase_statistics()` function is augmented with a `round_to`
    parameter.
- New statistical function
  [`multi_marginal_statistics()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md).
- New statistical function `estimate_ranges()` that can be used to
  estimate the sensitivity of calibration results to different model
  parameters.

### Bugfixes & changes

- Fixed a bug in
  [`MultiHPD()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md)
  that ignored the `roundingOfValue` parameter.
- Fixed a bug in
  [`MarginalStatistics()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md)
  that triggered an error if the function was passed a constant MCMC
  chain.

## ArchaeoPhases 1.4.0

### Bugfixes & changes

- Includes an update of the
  [`TempoPlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md):
  optimization of the credible intervals as already done in
  [`OccurrencePlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md).
- Includes a minor update of the vignettes.
- Includes an update of the function
  [`MarginalPlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md)
  and adds a new function :
  [`MultiMarginalPlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md).
- Includes an update of the function
  [`MarginalStatistics()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md)
  and adds a new function : `MultiMarginalMarginalStatistics()`.
- Includes an update of the shiny web application (called by function
  `app_ArchaeoPhases()`) that did not work in the previous version.

## ArchaeoPhases 1.3.0

### New functions

- Includes a new function :
  [`OccurrencePlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md).

### Bugfixes & changes

- Includes an update of the function `ImportCSV()` and a new function
  for ‘BCal’ users called `ImportCSV.BCal()`.
- Includes an update of the
  [`MultiDatesPlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md).
  The graphic is now done with **ggplot2**.
- Includes an update of the
  [`TempoPlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md)
  and
  [`TempoActivityPlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md)
  functions.
- Includes an update of the vignette that describes the main functions
  of the package.
- Includes an update of the shiny web application (called by function
  `app_ArchaeoPhases()`).

## ArchaeoPhases 1.2.0

### Bugfixes & changes

- Includes an update of the vignette that describes the main functions
  of the package.
- Includes an update of the shiny web application (called by function
  `app_ArchaeoPhases()`).

## ArchaeoPhases 1.1.0

### Bugfixes & changes

- Includes a vignette that describes the main functions of the package.
- Includes an update of the `ImportCSV()` function in order to import
  the raw MCMC generated by ‘BCal’ and to convert the MCMC samples from
  the date format cal BP (in years before 1950) to the date format
  BC/AD.
- Includes a new dataset of MCMC samples generated by ‘BCal’:
  `Fishpond.RData()`.
- Includes an update of the `coda.mcmc()` function.
- Includes an update of the
  [`TempoPlot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/ArchaeoPhases-defunct.md)
  function using the package **ggplot2**.
- Includes an update of all graphic functions: it is now possible to
  export all graphics and to choose the colors associated to the
  characteristics of groups of dates.
- Includes an update of the shiny web application (called by function
  `app_ArchaeoPhases()`).

## ArchaeoPhases 1.0.0

- Initial release.
- Includes all functions of **RChronoModel** and their updates.
- Includes a function `coda.mcm()` that creates a MCMC_list in order to
  use the package **coda**.
- Includes a shiny application and the function `app_ArchaeoPhases()` to
  call it from R.
- **RChronoModel** is now obsolete and replaced by **ArchaeoPhases**.
