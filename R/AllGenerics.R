# GENERIC METHODS
#' @include AllClasses.R
NULL

# Coerce =======================================================================
#' Coerce to a Data Frame
#'
#' @param x An object.
#' @param calendar A [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [calendar()]).
#' @param ... Further parameters to be passed to [data.frame()].
#' @return
#'  A [`data.frame`] with an extra `time` column giving the (decimal) years at
#'  which the time series was sampled.
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name data.frame
#' @rdname data.frame
NULL

#' Coerce to Coda
#'
#' Extracts parallel chains from an [`MCMC-class`] object to create an
#' `mcmc.list` object for use with \pkg{coda} diagnostic tools.
#' @param from from An object to be coerced.
#' @param chains An [`integer`] specifying the number of parallel chains
#'  (defaults to \eqn{1}).
#' @param ... Currently not used.
#' @return
#'  An [`coda::mcmc.list`] object.
#' @example inst/examples/ex-coda.R
#' @seealso [coda::mcmc()], [coda::mcmc.list()]
#' @author A. Philippe, M.-A. Vibet
#' @family read methods
#' @docType methods
#' @aliases as_coda-method
setGeneric(
  name = "as_coda",
  def = function(from, ...) standardGeneric("as_coda")
)

# Tools ========================================================================
## Mutators --------------------------------------------------------------------
# Get or Set Parts of an Object
#
# Getters and setters to extract or replace parts of an object.
# @param x An object from which to get or set element(s).
# @param value A possible value for the element(s) of `x`.
# @return
#  An object of the same sort as `x` with the new values assigned.
# @example inst/examples/ex-mutator.R
# @author N. Frerebeau
# @docType methods
# @family mutators
# @name mutators
# @rdname mutators
# @aliases get set
NULL

#' The Names of an Object
#'
#' Get or set the names of an object.
#' @param x An object from which to get or set names.
#' @param value A possible value for the names of `x`.
#' @return
#'  An object of the same sort as `x` with the new names assigned.
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name names
#' @rdname names
NULL

## Subset ----------------------------------------------------------------------
#' Extract or Replace Parts of an Object
#'
#' Operators acting on objects to extract or replace parts.
#' @param x An object from which to extract element(s) or in which to replace
#'  element(s).
#' @param i,j,k Indices specifying elements to extract or replace.
#' @param drop A [`logical`] scalar: should the result be coerced to
#'  the lowest possible dimension? This only works for extracting elements,
#'  not for the replacement.
# @param value A possible value for the element(s) of `x`.
#' @param ... Currently not used.
#' @return
#'  A subsetted object.
#' @example inst/examples/ex-subset.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name subset
#' @rdname subset
NULL

## Bind ------------------------------------------------------------------------
#' Combine two MCMC Objects
#'
#' @param x,y An [`MCMC-class`] object.
#' @return
#'  An [`MCMC-class`] object.
#' @example inst/examples/ex-subset.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name bind
#' @rdname bind
NULL

## Sort ------------------------------------------------------------------------
#' Ordering Permutation of an MCMC Object
#'
#' Returns a permutation which rearranges an object into ascending or descending
#' temporal order.
#' @param x An [`MCMC-class`] object.
#' @param decreasing A [`logical`] scalar: should the sort order be decreasing?
#' @return
#'  An [`integer`] vector.
#' @example inst/examples/ex-subset.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name sort.list
#' @rdname sort.list
NULL
setGeneric("sort.list")

#' Sort an MCMC Object
#'
#' Sort (or order) an object into ascending or descending temporal order.
#' @param x An [`MCMC-class`] object.
#' @param decreasing A [`logical`] scalar: should the sort order be decreasing?
#' @return
#'  An object of the same sort as `x`.
#' @example inst/examples/ex-subset.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name sort
#' @rdname sort
NULL

# Modeling =====================================================================
## Age-Depth Modeling ----------------------------------------------------------
#' Age-Depth Modeling
#'
#' Computes the age-depth curve from the output of the MCMC algorithm and the
#' known depth of each dated samples.
#' @param object An [`EventsMCMC-class`] object.
#' @param x An [`AgeDepthModel-class`] object.
#' @param depth A [`numeric`] vector giving of the depths of the dated samples.
#' @param newdata A [`numeric`] vector giving the depths at which ages will be
#'  predicted. If `missing`, the original data points are used.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param calendar A [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [calendar()]).
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x
#'  and y axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param ... Other [graphical parameters][graphics::par] may also be passed as
#'  arguments to this function, particularly, `border`, `col`, `lwd`, `lty` or
#'  `pch`.
#' @details
#'  We assume it exists a function \eqn{f} relating the age and the depth
#'  \eqn{age = f(depth)}. We estimate the function using local regression
#'  (also called local polynomial regression): \eqn{f = loess(age ~ depth)}.
#'  This estimated function \eqn{f} depends on the unknown dates. However,
#'  from the posterior distribution of the age/date sequence, we can evaluate
#'  the posterior distribution of the age function for each desired depth.
#' @return
#'  * `bury()` returns an [`AgeDepthModel-class`] object.
#'  * `predict()` returns an [`EventsMCMC-class`] object.
#'  * `plot()` is called it for its side-effects: it results in a graphic being
#'  displayed (invisibly returns `x`).
#' @references
#'  Jha, D. K., Sanyal, P. & Philippe, A. (2020). Multi-Proxy Evidence of Late
#'  Quaternary Climate and Vegetational History of North-Central India:
#'  Implication for the Paleolithic to Neolithic Phases. *Quaternary Science
#'  Reviews*, 229: 106121. \doi{10.1016/j.quascirev.2019.106121}.
#'
#'  Ghosh, S., Sanyal, P., Roy, S., Bhushan, R., Sati, S. P., Philippe, A. &
#'  Juyal, N. (2020). Early Holocene Indian Summer Monsoon and Its Impact on
#'  Vegetation in the Central Himalaya: Insight from dD and d13C Values of Leaf
#'  Wax Lipid. *The Holocene*, 30(7): 1063-1074. \doi{10.1177/0959683620908639}.
#' @example inst/examples/ex-bury.R
#' @author A. Philippe
#' @family age-depth modeling tools
#' @docType methods
#' @aliases bury-method
setGeneric(
  name = "bury",
  def = function(object, depth, ...) standardGeneric("bury"),
  valueClass = "AgeDepthModel"
)

## Interpolation ---------------------------------------------------------------
#' Interpolate Between Two Dates
#'
#' @param x A [`numeric`] vector giving the output of the MCMC algorithm for the
#'  first parameter.
#' @param y A [`numeric`] vector giving the output of the MCMC algorithm for the
#'  second parameter.
#' @param e1,e2 An [`integer`] specifying.
#' @param ... Currently not used.
#' @details
#'  For a given output of MCMC algorithm, this function interpolates between
#'  to events \eqn{x} and \eqn{y} (assuming \eqn{x < y}).
#' @example inst/examples/ex-interpolate.R
#' @author N. Frerebeau
#' @family age-depth modeling tools
#' @docType methods
#' @aliases interpolate-method
setGeneric(
  name = "interpolate",
  def = function(x, y, ...) standardGeneric("interpolate")
)

# Events =======================================================================
## Coerce ----------------------------------------------------------------------
#' Coerce to Events
#'
#' @param from from An object to be coerced.
#' @param calendar A [`aion::TimeScale-class`] object specifying the source
#'  calendar (see [calendar()]).
#' @param iteration A length-one [`numeric`] vector specifying the index of the
#'  iteration column.
#' @param ... Currently not used.
#' @return
#'  An [`EventsMCMC-class`] object.
#' @example inst/examples/ex-events.R
#' @author A. Philippe, M.-A. Vibet, N. Frerebeau
#' @family read methods
#' @docType methods
#' @aliases as_events-method
setGeneric(
  name = "as_events",
  def = function(from, ...) standardGeneric("as_events"),
  valueClass = "EventsMCMC"
)

## Tempo -----------------------------------------------------------------------
#' Tempo Plot
#'
#' A statistical graphic designed for the archaeological study of rhythms of the
#' long term that embodies a theory of archaeological evidence for the
#' occurrence of events.
#' @param object An [`EventsMCMC-class`] object.
#' @param from A length-one [`numeric`] vector giving the earliest date to
#'  estimate for (expressed in *[rata die][aion::RataDie-class]*).
#' @param to A length-one [`numeric`] vector giving the latest date to estimate
#'  for (expressed in *[rata die][aion::RataDie-class]*).
#' @param grid A length-one [`numeric`] vector specifying the number of equally
#'  spaced points of the temporal grid.
#' @param x A [`CumulativeEvents-class`] object or an [`EventsMCMC-class`]
#'  object.
#' @param calendar A [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [calendar()]).
#' @param interval A [`character`] string specifying the confidence interval to
#'  be drawn. It must be one of "`credible`" (credible interval) or "`gauss`"
#'  (Gaussian approximation of the credible interval). Any unambiguous substring
#'  can be given.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param count A [`logical`] scalar: should the counting process be a number
#'  or a probability (the default)?
#' @param credible A [`logical`] scalar: should the credible interval be
#'  computed/displayed?
#' @param gauss A [`logical`] scalar: should the Gaussian approximation of the
#'  credible interval be computed/displayed?
#' @param col.tempo,col.interval A specification for the plotting colors.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x
#'  and y axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param ... Other [graphical parameters][graphics::par] may also be passed as
#'  arguments to this function.
#' @details
#'  The tempo plot is one way to measure change over time: it estimates the
#'  cumulative occurrence of archaeological events in a Bayesian calibration.
#'  The tempo plot yields a graphic where the slope of the plot directly
#'  reflects the pace of change: a period of rapid change yields a steep slope
#'  and a period of slow change yields a gentle slope. When there is no change,
#'  the plot is horizontal. When change is instantaneous, the plot is vertical.
#' @return
#'  * `tempo()` returns an [`CumulativeEvents-class`] object.
#'  * `plot()` is called it for its side-effects: it results in a graphic being
#'    displayed (invisibly returns `x`).
#' @references
#'  Dye, T. S. (2016). Long-term rhythms in the development of Hawaiian social
#'  stratification. *Journal of Archaeological Science*, 71: 1-9.
#'  \doi{10.1016/j.jas.2016.05.006}.
#' @example inst/examples/ex-tempo.R
#' @author A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau
#' @family event tools
#' @docType methods
#' @aliases tempo-method
setGeneric(
  name = "tempo",
  def = function(object, ...) standardGeneric("tempo"),
  valueClass = "CumulativeEvents"
)

## Activity --------------------------------------------------------------------
#' Activity Plot
#'
#' Plots the first derivative of the [`tempo`] plot Bayesian estimate.
#' @param object An [`EventsMCMC-class`] or a [`CumulativeEvents-class`] object.
#' @param from A length-one [`numeric`] vector giving the earliest date to
#'  estimate for (expressed in *[rata die][aion::RataDie-class]*).
#' @param to A length-one [`numeric`] vector giving the latest date to estimate
#'  for (expressed in *[rata die][aion::RataDie-class]*.
#' @param grid A length-one [`numeric`] vector specifying the number of equally
#'  spaced points of the temporal grid.
#' @param x An [`ActivityEvents-class`] object.
#' @param calendar A [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [calendar()]).
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x
#'  and y axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param ... Other [graphical parameters][graphics::par] may also be passed as
#'  arguments to this function, particularly, `border`, `col`, `lwd` or `lty`.
#' @return
#'  * `activity()` returns an [`ActivityEvents-class`] object.
#'  * `plot()` is called it for its side-effects: it results in a graphic being
#'    displayed (invisibly returns `x`).
#' @references
#'  Dye, T. S. (2016). Long-term rhythms in the development of Hawaiian social
#'  stratification. *Journal of Archaeological Science*, 71: 1-9.
#'  \doi{10.1016/j.jas.2016.05.006}.
#' @example inst/examples/ex-tempo.R
#' @author A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau
#' @family event tools
#' @docType methods
#' @aliases activity-method
setGeneric(
  name = "activity",
  def = function(object, ...) standardGeneric("activity"),
  valueClass = "ActivityEvents"
)

## Occurrence ------------------------------------------------------------------
#' Occurrence Plot
#'
#' A statistical graphic designed for the archaeological study of when
#' events of a specified kind occurred.
#' @param object An [`EventsMCMC-class`] object.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param ... Currently not used.
#' @details
#'  If we have \eqn{k} events, then we can estimate the calendar date \eqn{t}
#'  corresponding to the smallest date such that the number of events observed
#'  before \eqn{t} is equal to \eqn{k}.
#'
#'  The `occurrence()` estimates these occurrences and gives the credible
#'  interval or the highest posterior density (HPD) region for a given `level`
#'  of confidence.
#' @return
#'  An [`OccurrenceEvents-class`] object.
#' @return An [`OccurrenceEvents-class`] object.
#' @example inst/examples/ex-occurrence.R
#' @author A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau
#' @family event tools
#' @docType methods
#' @aliases occurrence-method
setGeneric(
  name = "occurrence",
  def = function(object, ...) standardGeneric("occurrence"),
  valueClass = "OccurrenceEvents"
)

## Time Scale ------------------------------------------------------------------
#' Elapsed Time Scale
#'
#' @param object An object (typically an [`MCMC-class`] object).
#' @param origin An [`integer`] giving the position of the column corresponding
#'  to the event from which elapsed time is calculated.
#' @param ... Currently not used.
#' @return
#'  Returns an object of the same class as `object` with an elapsed
#' @return
#'  An object of the same sort as `object` with a new time scale.
#' @note
#'  There is no year \eqn{0} in BCE/CE scale.
#' @example inst/examples/ex-elapse.R
#' @author N. Frerebeau
#' @family event tools
#' @docType methods
#' @aliases elapse-method
setGeneric(
  name = "elapse",
  def = function(object, ...) standardGeneric("elapse")
)

# Interval =====================================================================
## CI --------------------------------------------------------------------------
#' Bayesian Credible Interval
#'
#' Computes the shortest credible interval of the output of the MCMC algorithm
#' for a single parameter.
#' @param x An [`MCMC-class`] object containing the output of the MCMC algorithm.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param calendar A [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [calendar()]).
#' @param ... Currently not used.
#' @details
#'  A \eqn{(100 \times level)}{(100 * level)} % credible interval is an interval
#'  that keeps \eqn{N \times (1 - level)}{N * (1 - level)} elements of the
#'  sample outside the interval.
#'
#'  The \eqn{(100 \times level)}{(100 * level)} % credible interval is the
#'  shortest of all those intervals.
#'
#'  For instance, the 95% credible interval is the central portion of the
#'  posterior distribution that contains 95% of the values.
#' @return
#'  Returns a [`list`] of `numeric` [`matrix`].
#' @example inst/examples/ex-interval.R
#' @seealso [arkhe::interval_credible()]
#' @author A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau
#' @family statistics
#' @docType methods
#' @aliases interval_credible-method
setGeneric(
  name = "interval_credible",
  def = getGeneric("interval_credible", package = "arkhe")
)

## HPDI ------------------------------------------------------------------------
#' Bayesian HPD Regions
#'
#' @param x An [`MCMC-class`] object containing the output of the MCMC algorithm.
#' @param y Currently not used.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param calendar A [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [calendar()]).
#' @param ... Extra arguments to be passed to [stats::density()].
#' @return
#'  Returns a [`list`] of `numeric` [`matrix`].
#' @references
#'  Hyndman, R. J. (1996). Computing and graphing highest density regions.
#'  *American Statistician*, 50: 120-126. \doi{10.2307/2684423}.
#' @example inst/examples/ex-interval.R
#' @seealso [stats::density()], [arkhe::interval_hdr()]
#' @author A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau
#' @family statistics
#' @docType methods
#' @aliases interval_hdr-method
setGeneric(
  name = "interval_hdr",
  def = getGeneric("interval_hdr", package = "arkhe")
)

# Phase ========================================================================
## Build -----------------------------------------------------------------------
#' Compute Phases
#'
#' Constructs the minimum and maximum for a group of events (phase).
#' @param x An [`EventsMCMC-class`].
#' @param groups A [`list`] of (named) vector of names or indexes of columns in
#'  `x` (see [phases()]).
#' @param ... Currently not used.
#' @return
#'  A [`PhasesMCMC-class`] object.
#' @note
#'  The default value of `start` or `end` corresponds to a CSV file exported
#'  from [ChronoModel](https://chronomodel.com/).
#' @example inst/examples/ex-phases.R
#' @author A. Philippe, M.-A. Vibet, N. Frerebeau
#' @family phase tools
#' @docType methods
#' @aliases phases-method
setGeneric(
  name = "phases",
  def = function(x, groups, ...) standardGeneric("phases"),
  valueClass = "PhasesMCMC"
)

## Coerce ----------------------------------------------------------------------
#' Coerce to Phases
#'
#' @param from from An object to be coerced.
#' @param start An [`integer`] vector specifying the index of the columns
#'  corresponding to the beginning of the phases. If missing, every other column
#'  is used starting from the first column (after deleting the `iteration`
#'  column, if any).
#' @param stop An [`integer`] vector specifying the index of the columns
#'  corresponding to the end of the phases. If missing, every other column
#'  is used starting from the second column (after deleting the `iteration`
#'  column, if any).
#' @param names A [`character`] vector giving the names of the phases.
#' @param calendar A [`aion::TimeScale-class`] object specifying the source
#'  calendar (see [calendar()]).
#' @param iteration A length-one [`numeric`] vector specifying the index of the
#'  iteration column.
#' @param ... Currently not used.
#' @return
#'  A [`PhasesMCMC-class`] object.
#' @example inst/examples/ex-phases.R
#' @author A. Philippe, M.-A. Vibet, N. Frerebeau
#' @family read methods
#' @docType methods
#' @aliases as_phases-method
setGeneric(
  name = "as_phases",
  def = function(from, ...) standardGeneric("as_phases"),
  valueClass = "PhasesMCMC"
)

## Duration --------------------------------------------------------------------
#' Phase Duration
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, `x` must be an
#'  [`PhasesMCMC-class`] object.
#' @param ... Currently not used.
#' @example inst/examples/ex-duration.R
#' @author A. Philippe, M.-A. Vibet, N. Frerebeau
#' @family phase tools
#' @docType methods
#' @aliases duration-method
setGeneric(
  name = "duration",
  def = function(x, y, ...) standardGeneric("duration")
)

# Plot =========================================================================
#' Plot Events
#'
#' Plots credible intervals or HPD regions of a series of events.
#' @param x An [`MCMC-class`] object.
#' @param calendar A [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [calendar()]).
#' @param density A [`logical`] scalar: should estimated density be plotted?
#' @param interval A [`character`] string specifying the confidence interval to
#'  be drawn. It must be one of "`credible`" (credible interval) or "`hdr`"
#'  (highest posterior density interval). Any unambiguous substring can be
#'  given. If `NULL` (the default) no interval is computed.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param sort A [`logical`] scalar: should the data be sorted?
#' @param decreasing A [`logical`] scalar: should the sort order be decreasing?
#'  Only used if `sort` is `TRUE`.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x
#'  and y axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param col.density,col.interval A specification for the plotting colors.
#' @param ... Extra parameters to be passed to [stats::density()].
#' @return
#'   `plot()` is called it for its side-effects: it results in a graphic being
#'   displayed (invisibly returns `x`).
#' @example inst/examples/ex-summary.R
#' @seealso [`stats::density()`]
#' @author A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau
#' @family plot methods
#' @docType methods
#' @name plot_events
#' @rdname plot_events
NULL

#' Plot Phases
#'
#' Plots the characteristics of a group of events (phase).
#' @param x A [`PhasesMCMC-class`] object.
#' @param calendar A [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [calendar()]).
#' @param density A [`logical`] scalar: should estimated density be plotted?
#' @param range A [`logical`] scalar: should phase time range be plotted
#'  (see [boundaries()])?
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param sort A [`logical`] scalar: should the data be sorted?
#' @param decreasing A [`logical`] scalar: should the sort order be decreasing?
#'  Only used if `sort` is `TRUE`.
#' @param succession A [`character`] string specifying the additional time range
#'  to be displayed. It must be one of "`hiatus`" or "`transition`".
#'  If `NULL` (the default), no additional time ranges are displayed.
#' @param legend A [`logical`] scalar: should a legend be displayed?
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x
#'  and y axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param col.density,col.range,col.succession A specification for the plotting
#'  colors.
#' @param ... Extra parameters to be passed to [stats::density()].
#' @return
#'   `plot()` is called it for its side-effects: it results in a graphic being
#'   displayed (invisibly returns `x`).
#' @example inst/examples/ex-summary.R
#' @seealso [`stats::density()`]
#' @author A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau
#' @family plot methods
#' @docType methods
#' @name plot_phases
#' @rdname plot_phases
NULL

# Time Ranges ==================================================================
## Boundaries ------------------------------------------------------------------
#' Phase Time Range
#'
#' Computes the shortest interval that satisfies
#' \eqn{P(PhaseMin < IntervalInf < IntervalSup < PhaseMax | M) = level}
#' for each phase.
#' @param x,y A [`numeric`] vector. If `y` is missing, `x` must be an
#'  [`PhasesMCMC-class`] object.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param ... Currently not used.
#' @return
#'  The endpoints of the shortest time range (at a given `level`).
#' @example inst/examples/ex-range.R
#' @author A. Philippe, M.-A. Vibet, N. Frerebeau
#' @family time ranges
#' @docType methods
#' @aliases boundaries-method
setGeneric(
  name = "boundaries",
  def = function(x, y, ...) standardGeneric("boundaries")
)

## Hiatus ----------------------------------------------------------------------
#' Hiatus Between Two Dates
#'
#' Tests for the existence of a hiatus between two parameters.
#' @param x,y A [`numeric`] vector. If `y` is missing, `x` must be an
#'  [`PhasesMCMC-class`] or an [`EventsMCMC-class`] object.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param ... Currently not used.
#' @details
#'  Finds if a gap exists between two dates and returns the longest interval
#'  that satisfies \eqn{P(x < HiatusInf < HiatusSup < y | M) = level}
#'
#'  The hiatus between two successive phases is the longest interval that
#'  satisfies
#'  \eqn{P(Phase1Max < IntervalInf < IntervalSup < Phase2Min | M) = level}
#'  (this assumes that the phases are in temporal order constraint).
#' @return
#'  The endpoints of the hiatus between successive events/phases
#'  (at a given `level`).
#' @example inst/examples/ex-test.R
#' @author A. Philippe, M.-A. Vibet, N. Frerebeau
#' @family time ranges
#' @docType methods
#' @aliases hiatus-method
setGeneric(
  name = "hiatus",
  def = function(x, y, ...) standardGeneric("hiatus")
)

## Transition ------------------------------------------------------------------
#' Transition Range Between Successive Phases
#'
#' Estimates the transition endpoints between two phases.
#' @param x,y A [`numeric`] vector. If `y` is missing, `x` must be an
#'  [`PhasesMCMC-class`] object.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param ... Currently not used.
#' @details
#'  The transition is the shortest interval that satisfies
#'  \eqn{P(IntervalInf < Phase1Max < Phase2Min < IntervalSup | M) = level}.
#'
#'  This assumes that the phases are in temporal order constraint.
#' @return
#'  The endpoints of the transition interval for each pair of successive phases
#'  (at a given `level`).
#' @example inst/examples/ex-range.R
#' @author A. Philippe, M.-A. Vibet, N. Frerebeau
#' @family time ranges
#' @docType methods
#' @aliases transition-method
setGeneric(
  name = "transition",
  def = function(x, y, ...) standardGeneric("transition")
)

# Test =========================================================================
## Anteriority -----------------------------------------------------------------
#' Bayesian Test for Anteriority/Posteriority
#'
#' A Bayesian test for checking the following assumption: "event `x` is older
#' than event `y`".
#' @param x A [`numeric`] vector giving the output of the MCMC algorithm for the
#'  first parameter, or an [`EventsMCMC-class`] object.
#' @param y A [`numeric`] vector giving the output of the MCMC algorithm for the
#'  second parameter.
#' @param ... Currently not used.
#' @details
#'  For a given output of MCMC algorithm, this function estimates the posterior
#'  probability of the event \eqn{x < y} by the relative frequency of the event
#'  "the value of event `x` is less than the value of event `y`" in the
#'  simulated Markov chain.
#' @example inst/examples/ex-test.R
#' @author A. Philippe, M.-A. Vibet, N. Frerebeau
#' @family tests
#' @docType methods
#' @aliases older-method
setGeneric(
  name = "older",
  def = function(x, y, ...) standardGeneric("older")
)

# Read =========================================================================
#' Read OxCal Output
#'
#' Reads MCMC output.
#' @param file A [`character`] string giving the name of the CSV file which the
#'  data are to be read from.
#' @param calendar A [`aion::TimeScale-class`] object specifying the calendar
#'  (see [calendar()]). It should be [CE()] unless you change the
#'  default settings in 'OxCal'.
#' @param ... Currently not used.
#' @return
#'  An [`EventsMCMC-class`] object.
#' @references
#'  Bronk Ramsey, C. (2009). Bayesian Analysis of Radiocarbon Dates.
#'  *Radiocarbon*, 51(1), 337-360. \doi{10.1017/S0033822200033865}.
#' @example inst/examples/ex-read_oxcal.R
#' @seealso [utils::read.table()]
#' @author T. S. Dye, N. Frerebeau
#' @family read methods
#' @docType methods
#' @aliases read_oxcal-method
setGeneric(
  name = "read_oxcal",
  def = function(file, ...) standardGeneric("read_oxcal"),
  valueClass = "MCMC"
)

#' Read BCal Output
#'
#' Reads MCMC output.
#' @param file A [`character`] string giving the name of the CSV file which the
#'  data are to be read from.
#' @param bin_width The bin width specified for the
#'  [BCal](https://bcal.shef.ac.uk/) calibration. Defaults to the BCal
#'  default of 1.
#' @param calendar A [`aion::TimeScale-class`] object specifying the calendar
#'  (see [calendar()]). It should be [BP()] unless you change the
#'  default settings in 'BCal'.
#' @param ... Currently not used.
#' @return
#'  An [`EventsMCMC-class`] object.
#' @references
#'  Buck C. E., Christen J. A. & James G. N. (1999). BCal: an on-line Bayesian
#'  radiocarbon calibration tool. *Internet Archaeology*, 7.
#'  \doi{10.11141/ia.7.1}.
#' @example inst/examples/ex-read_bcal.R
#' @seealso [utils::read.table()]
#' @author T. S. Dye, N. Frerebeau
#' @family read methods
#' @docType methods
#' @aliases read_bcal-method
setGeneric(
  name = "read_bcal",
  def = function(file, ...) standardGeneric("read_bcal"),
  valueClass = "MCMC"
)

#' Read ChronoModel Output
#'
#' Reads MCMC output.
#' @param file A [`character`] string giving the name of the CSV file which the
#'  data are to be read from.
#' @param calendar A [`aion::TimeScale-class`] object specifying the calendar
#'  (see [calendar()]). It should be [CE()] unless you change the
#'  default settings in 'ChronoModel'.
#' @param sep A [`character`] string specifying the field separator character
#'  (see [utils::read.table()]).
#' @param dec A [`character`] string specifying the character used in the file
#'  for decimal points (see [utils::read.table()]).
#' @param ... Currently not used.
#' @return
#'  An [`EventsMCMC-class`] or a [`PhasesMCMC-class`] object.
#' @references
#'  Lanos, Ph., Philippe, A. & Dufresne, Ph. (2015). Chronomodel:
#'  Chronological Modeling of Archaeological Data using Bayesian Statistics.
#'  URL: <https://chronomodel.com/>.
#' @example inst/examples/ex-read_chronomodel.R
#' @seealso [utils::read.table()]
#' @author T. S. Dye, N. Frerebeau
#' @family read methods
#' @docType methods
#' @name read_chronomodel
#' @rdname read_chronomodel
#' @aliases read_chronomodel-method
NULL

#' @rdname read_chronomodel
#' @aliases read_chronomodel_events-method
setGeneric(
  name = "read_chronomodel_events",
  def = function(file, ...) standardGeneric("read_chronomodel_events")
)

#' @rdname read_chronomodel
#' @aliases read_chronomodel_phases-method
setGeneric(
  name = "read_chronomodel_phases",
  def = function(file, ...) standardGeneric("read_chronomodel_phases")
)

# @rdname read_chronomodel
# @aliases read_chronomodel_model-method
# setGeneric(
#   name = "read_chronomodel_model",
#   def = function(file, ...) standardGeneric("read_chronomodel_model")
# )

#' Check for an Original MCMC File
#'
#' Checks whether or not a file is identical to the one used to create
#' an object.
#' @param object An object (typically an [`MCMC-class`] object).
#' @param file Either a path to a CSV file or a connection.
#' @param download A [`logical`] scalar: should the remote file be downloaded
#'  and hashed locally?
#' @param ... Currently not used.
#' @return
#'  A [`logical`]: `TRUE` if the files match, `FALSE` otherwise.
#' @example inst/examples/ex-read.R
#' @seealso [digest::digest()]
#' @author T. S. Dye, N. Frerebeau
#' @family read methods
#' @docType methods
#' @name check
#' @rdname check
NULL

#' @rdname check
#' @aliases is_original-method
setGeneric(
  name = "is_original",
  def = function(object, ...) standardGeneric("is_original")
)

# Sensitivity ==================================================================
#' Sensitivity
#'
#' Calculates the ranges of summary statistics from the output of two or more
#' runs of the MCMC algorithm.
#' @param ... Any [`EventsMCMC-class`] object.
#' @param positions A [`numeric`] vector specifying the positions of the columns
#'  corresponding to the MCMC chains of interest, or a [`character`] vector of
#'  column names.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @details
#' This function is useful for estimating the sensitivity of calibration results
#' to different model parameters.
#' @return
#'  A [`data.frame`].
#' @seealso [summary()]
#' @example inst/examples/ex-sensitivity.R
#' @author T. S. Dye, N. Frerebeau
#' @family statistics
#' @docType methods
#' @aliases sensitivity-method
setGeneric(
  name = "sensitivity",
  def = function(...) standardGeneric("sensitivity")
)

# Summary ======================================================================
#' Marginal Summary Statistics for Multiple MCMC Chains
#'
#' Calculates summary statistics of the output of the MCMC algorithm for
#' multiple parameters. Results are given in calendar years (BC/AD).
#' @param object An [`MCMC-class`] or a [`PhasesMCMC-class`] object.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param calendar A [`aion::TimeScale-class`] object specifying the target calendar
#'  (see [calendar()]).
#' @return
#'  A [`data.frame`] where the rows correspond to the chains of interest and
#'  columns to the following statistics:
#'  \describe{
#'   \item{mean}{The mean of the MCMC chain.}
#'   \item{sd}{The standard deviation of the MCMC chain.}
#'   \item{min}{Minimum value of the MCMC chain.}
#'   \item{q1}{First quantile of the MCMC chain.}
#'   \item{median}{Median of the MCMC chain.}
#'   \item{q3}{Third quantile of the MCMC chain.}
#'   \item{max}{Maximum value of the MCMC chain.}
#'   \item{lower}{Lower boundary of the [credible interval][interval_credible()]
#'    of the MCMC chain at `level`.}
#'   \item{upper}{Upper boundary of the [credible interval][interval_credible()]
#'    of the MCMC chain at `level`.}
#'  }
#' @example inst/examples/ex-summary.R
#' @author A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau
#' @family statistics
#' @docType methods
#' @name summary
#' @rdname summary
NULL

# Interval Analysis ============================================================
## Allen Interval Algebra ------------------------------------------------------
#' Allen Relation Between Definite Intervals
#'
#' @param x,y A [`numeric`] vector giving the lower and upper boundaries of the
#'  time intervals, respectively. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param ... Currently not used.
#' @details
#' \tabular{lrlr}{
#'  **Relation** \tab     \tab     \tab  **Converse** \cr
#'  precedes     \tab (p) \tab (P) \tab   preceded by \cr
#'  meets        \tab (m) \tab (M) \tab        met by \cr
#'  overlaps     \tab (o) \tab (O) \tab overlapped by \cr
#'  finished by  \tab (F) \tab (f) \tab      finishes \cr
#'  contains     \tab (D) \tab (d) \tab        during \cr
#'  starts       \tab (s) \tab (S) \tab    started by \cr
#'  equals       \tab (e) \tab     \tab               \cr
#' }
#' @return
#'  A [`character`] matrix specifying the Allen relations.
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#'
#'  Alspaugh, T. (2019). Allen's Interval Algebra.
#'  URL: \url{https://thomasalspaugh.org/pub/fnd/allen.html}.
#' @example inst/examples/ex-allen.R
#' @author T. S. Dye, N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_relation-method
setGeneric(
  name = "allen_relation",
  def = function(x, y, ...) standardGeneric("allen_relation")
)

#' Complement of an Allen Relation
#'
#' @param x A [`character`] vector or matrix of Allen relations (typically
#'  returned by [allen_relation()]).
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector or matrix (same as `x`).
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author T. S. Dye, N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_complement-method
setGeneric(
  name = "allen_complement",
  def = function(x, ...) standardGeneric("allen_complement")
)

#' Converse of an Allen Relation
#'
#' @param x A [`character`] vector or matrix of Allen relations (typically
#'  returned by [allen_relation()]).
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector or matrix (same as `x`).
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author T. S. Dye, N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_converse-method
setGeneric(
  name = "allen_converse",
  def = function(x, ...) standardGeneric("allen_converse")
)

#' Composition of Allen Relations
#'
#' @param x,y A [`character`] vector of Allen relations.
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector.
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author T. S. Dye, N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_composition-method
setGeneric(
  name = "allen_composition",
  def = function(x, y, ...) standardGeneric("allen_composition")
)

#' Intersection of Allen Relations
#'
#' @param x,y A [`character`] vector of Allen relations.
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector.
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author T. S. Dye, N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_intersect-method
setGeneric(
  name = "allen_intersect",
  def = function(x, y, ...) standardGeneric("allen_intersect")
)

#' Union of Allen Relations
#'
#' @param x,y A [`character`] vector of Allen relations.
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector.
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author T. S. Dye, N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_union-method
setGeneric(
  name = "allen_union",
  def = function(x, y, ...) standardGeneric("allen_union")
)

#' Observe the Relation Between two Phases
#'
#' Plots an empirical Nökel lattice.
#' @param x An [`EventsMCMC-class`] or a [`PhasesMCMC-class`] object containing
#'  the output of the MCMC algorithm.
#' @param groups A [`list`] of (named) vector of names or indexes of columns in
#'  `x` (see [phases()]).
#' @param converse A [`logical`] scalar: should converse relations be observed?
#' @param ... Further arguments to be passed to internal methods.
#' @return
#'  `allen_observe()` is called it for its side-effects: it results in a graphic
#'  being displayed (invisibly returns `x`).
#' @example inst/examples/ex-allen-observe.R
#' @author T. S. Dye, N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_observe-method
setGeneric(
  name = "allen_observe",
  def = function(x, groups, ...) standardGeneric("allen_observe")
)

#' Observed Frequency of an Allen Set
#'
#' Creates a matrix of observed frequencies of a given Allen set among two or
#' more groups of chains from the MCMC output of a Bayesian calibration.
#' @param x An [`EventsMCMC-class`] or a [`PhasesMCMC-class`] object containing
#'  the output of the MCMC algorithm.
#' @param groups A [`list`] of (named) vector of names or indexes of columns in
#'  `x` (see [phases()]).
#' @param set A [`character`] string representation of an Allen set.
#' @param ... Currently not used.
#' @return
#'  A square [`matrix`] of observed frequencies.
#' @example inst/examples/ex-allen-observe.R
#' @author T. S. Dye, N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_observe_frequency-method
setGeneric(
name = "allen_observe_frequency",
def = function(x, groups, ...) standardGeneric("allen_observe_frequency")
)

#' Joint Concurrence of Two or More Observed Intervals
#'
#' Estimates the age of an undated context based on the known depositional
#' history of associated artifacts.
#' @param x An [`EventsMCMC-class`] object containing
#'  the output of the MCMC algorithm.
#' @param groups A [`list`] of (named) vector of names or indexes of columns in
#'  `x` (see [phases()]).
#' @param ... Currently not used.
#' @return
#'  A [`PhasesMCMC-class`] object.
#' @author T. S. Dye
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_joint_concurrency-method
setGeneric(
  name = "allen_joint_concurrency",
  def = function(x, groups, ...) standardGeneric("allen_joint_concurrency")
)
