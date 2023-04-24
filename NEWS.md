# ArchaeoPhases 2.0.0.9000

## New classes and methods
* Add `bind()` to bind multiple `MCMC` objects.
* Add `roc()` to compute the rate of change from a tempo/activity plot (returns a `RateOfChange` object).

## Enhancements
* Support for CE, BP and b2k time scales.

## Bug fixes & changes
* Use `stats::density()` instead of `hdrcde::hdr()` for HDPI estimation.

## Breaking changes
* Full rewrite in S4 (see below).
* Remove the Shiny application.

| ArchaeoPhases 1.x | ArchaeoPhases 2.0 |
|:----|:----|
| `AgeDepth()` | `bury()` |
| `CreateMinMaxGroup()` | `phase(<MCMC>)`, `as_phases(<matrix>)` |
| `CredibleInterval()`, `credible_interval()` | `credible(<numeric>, <numeric>)` |
| `DatesHiatus()`, `dates_hiatus()` | `hiatus(<numeric>, <numeric>)` |
| `estimate_range()` | `sensitivity(<MCMC>)` |
| `MarginalPlot()`, `marginal_plot()` | `plot(<MCMC>)` |
| `MarginalProba()` | `test_older(<numeric>, <numeric>)` |
| `MarginalStatistics()`, `marginal_statistics()`, `multi_marginal_statistics()` | `summary(<MCMC>)` |
| `MultiCredibleInterval()`, `multi_credible_interval()` | `credible(<MCMC>)` |
| `MultiDatesPlot()`, `multi_dates_plot()` | `plot(<MCMC>)` |
| `MultiHPD()`, `multi_hpd()` | `hpdi(<MCMC>)` |
| `MultiMarginalPlot()`, `multi_marginal_plot()` | `plot(<MCMC>)` |
| `MultiPhasePlot()` | `plot(<PhasesMCMC>)` |
| `MultiPhaseTimeRange()` | `boundaries(<PhasesMCMC>)` |
| `MultiPhasesGap()` | `hiatus(<PhasesMCMC>)` |
| `MultiPhasesTransition()` | `transition(<PhasesMCMC>)` |
| `MultiSuccessionPlot()` | `plot(<PhasesMCMC>)` |
| `OccurrencePlot()`, `occurrence_plot()` | `occurrence(<MCMC>)` + `plot(<OccurrenceEvents>)` |
| `PhaseDurationPlot()` | `duration(<PhasesMCMC>)` + `plot(<MCMC>)` |
| `PhasePlot()` | `plot(<PhasesMCMC>)` |
| `PhaseStatistics()` | `summary(<PhasesMCMC>)` |
| `PhaseTimeRange()` | `boundaries(<numeric>, <numeric>)` |
| `PhasesGap()`, `phases_gap()` | `hiatus(<numeric>, <numeric>)` |
| `PhasesTransition()` | `transition(<numeric>, <numeric>)` |
| `SuccessionPlot()` | `plot(<PhasesMCMC>)` |
| `TempoActivityPlot()`, `tempo_activity_plot()` | `activity(<MCMC>)` + `plot(<ActivityEvents>)` |
| `TempoPlot()`, `tempo_plot()` | `tempo(<MCMC>)` + `plot(<CumulativeEvents>)` |
| `undated_sample()` | `interpolate()` |

# ArchaeoPhases 1.6

## New functions
* New functions for Allen's interval algebra: `allen_analyze()`, `allen_joint_concurrency()`, `allen_observe_frequency()`, `allen_illustrate()`, `allen_observe()`.

## Bugfixes & changes
* Fixed a bug that failed to export the `reproduce()` function.
* Added a vignette on object reproducibility.

# ArchaeoPhases 1.5

## New functions
* New read functions for MCMC data: `read_bcal()`, `read_oxcal()`, `read_chronomodel()`.
  * The new read functions return S3 objects that can identify the file that produced them.
  * The new read functions are built on `read_csv()`, which can read data from a file, connection, or the clipboard.
* New plot functions: `multi_dates_plot()`, `tempo_activity_plot()`, `tempo_plot()`, `marginal_plot()`, `multi_marginal_plot()`, and `occurrence_plot()`.
  * The new plot functions are functional replacements for the originals with camelCase names, e.g., `TempoPlot()` -> `tempo_plot()`.
  * The new plot functions return S3 objects with `plot()` and `reproduce()` methods.
  * The S3 objects inherit from `data.frame` and can be passed to appropriate statistical functions to summarize the data in the plot.
* New statistical functions: `credible_interval()`, `multi_credible_interval()`, `multi_hpd()`, `dates_hiatus()`, `phases_gap()`, `marginal_statistics()`, and `phase_statistics()`.
  * The new statistical functions are functional replacements for the originals with camelCase names, e.g., `CredibleInterval()` -> `credible_interval()`.
  * The new statistical functions return lists in canonical R fashion.
  * The `phase_statistics()` function is augmented with a `round_to` parameter.
* New statistical function `multi_marginal_statistics()`.
* New statistical function `estimate_ranges()` that can be used to estimate the sensitivity of calibration results to different model parameters.

## Bugfixes & changes
* Fixed a bug in `MultiHPD()` that ignored the `roundingOfValue` parameter.
* Fixed a bug in `MarginalStatistics()` that triggered an error if the function was passed a constant MCMC chain.

# ArchaeoPhases 1.4

## Bugfixes & changes
* Includes an update of the `TempoPlot()`: optimization of the credible intervals as already done in `OccurrencePlot()`.
* Includes a minor update of the vignettes.
* Includes an update of the function `MarginalPlot()` and adds a new function : `MultiMarginalPlot()`.
* Includes an update of the function `MarginalStatistics()` and adds a new function : `MultiMarginalMarginalStatistics()`.
* Includes an update of the shiny web application (called by function `app_ArchaeoPhases()`) that did not work in the previous version.

# ArchaeoPhases 1.3

## New functions
* Includes a new function : `OccurrencePlot()`.

## Bugfixes & changes
* Includes an update of the function `ImportCSV()` and a new function for 'BCal' users called `ImportCSV.BCal()`.
* Includes an update of the `MultiDatesPlot()`. The graphic is now done with **ggplot2**.
* Includes an update of the `TempoPlot()` and `TempoActivityPlot()` functions.
* Includes an update of the vignette that describes the main functions of the package.
* Includes an update of the shiny web application (called by function `app_ArchaeoPhases()`).

# ArchaeoPhases 1.2

## Bugfixes & changes
* Includes an update of the vignette that describes the main functions of the package.
* Includes an update of the shiny web application (called by function `app_ArchaeoPhases()`).

# ArchaeoPhases 1.1

## Bugfixes & changes
* Includes a vignette that describes the main functions of the package.
* Includes an update of the `ImportCSV()` function in order to import the raw MCMC generated by 'BCal' and to convert the MCMC samples from the date format cal BP (in years before 1950) to the date format BC/AD.
* Includes a new dataset of MCMC samples generated by 'BCal': `Fishpond.RData()`.
* Includes an update of the `coda.mcmc()` function.
* Includes an update of the `TempoPlot()` function using the package **ggplot2**.
* Includes an update of all graphic functions: it is now possible to export all graphics and to choose the colors associated to the characteristics of groups of dates.
* Includes an update of the shiny web application (called by function `app_ArchaeoPhases()`).

# ArchaeoPhases 1.0

* Initial release.
* Includes all functions of **RChronoModel** and their updates.
* Includes a function `coda.mcm()` that creates a MCMC_list in order to use the package **coda**.
* Includes a shiny application and the function `app_ArchaeoPhases()` to call it from R.
* **RChronoModel** is now obsolete and replaced by **ArchaeoPhases**.
