# GENERIC METHODS
#' @include AllClasses.R
NULL

# S4 dispatch to base S3 generic ===============================================
if (!isGeneric("plot"))
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))
if (!isGeneric("summary"))
  setGeneric("summary", function(object, ...) standardGeneric("summary"))

# Coerce =======================================================================
#' Coda
#'
#' Extracts parallel chains from a [`MCMC-class`]
#' object to create an [`mcmc.list`] object for use with \pkg{coda}
#' diagnostic tools.
#' @param from from An object to be coerced.
#' @param chains An [`integer`] specifying the number of parallel chains
#'  (defaults to \eqn{1}).
#' @param ... Currently not used.
#' @return
#'  An [`coda::mcmc.list`] object.
#' @example inst/examples/ex-coda.R
#' @seealso [coda::mcmc()], [coda::mcmc.list()]
#' @author A. Philippe, M.-A. Vibet
#' @family read
#' @docType methods
#' @name coda
#' @rdname coda
NULL

#' @rdname coda
#' @aliases as_coda-method
setGeneric(
  name = "as_coda",
  def = function(from, ...) standardGeneric("as_coda")
)

#' Coerce
#'
#' @param from from An object to be coerced.
#' @param start An [`integer`] vector specifying the index of the columns
#'  corresponding to the beginning of the phases. If missing, every other column
#'  is used starting from the first column (after deleting the `iteration`
#'  column, if any).
#' @param end An [`integer`] vector specifying the index of the columns
#'  corresponding to the end of the phases. If missing, every other column
#'  is used starting from the second column (after deleting the `iteration`
#'  column, if any).
#' @param BP A [`logical`] scalar: should the data be converted from BP to
#'  BC/AD? This should not be `TRUE` unless you change the default settings in
#'  'OxCal' or 'ChronoModel'.
#' @param iteration An [`integer`] specifying the index of the iteration column
#'  to be removed.
#' @param ... Currently not used.
#' @return
#'  An [`MCMC-class`] or a [`PhasesMCMC-class`] object.
#' @note
#'  If `start` or `end` is missing, the default value of corresponds to a CSV
#'  file exported from '\href{https://chronomodel.com/}{ChronoModel}'.
#' @example inst/examples/ex-coerce.R
#' @author A. Philippe, M.-A. Vibet, N. Frerebeau
#' @family read
#' @docType methods
#' @name coerce
#' @rdname coerce
NULL

#' @rdname coerce
#' @aliases as_mcmc-method
setGeneric(
  name = "as_mcmc",
  def = function(from, ...) standardGeneric("as_mcmc")
)

#' @rdname coerce
#' @aliases as_phases-method
setGeneric(
  name = "as_phases",
  def = function(from, ...) standardGeneric("as_phases")
)

# Extract ======================================================================
## Mutators --------------------------------------------------------------------
#' Get or Set Parts of an Object
#'
#' Getters and setters to extract or replace parts of an object.
#' @param x An object from which to get or set element(s).
#' @param value A possible value for the element(s) of `x`.
#' @return
#'  An object of the same sort as \code{object} with the new values assigned.
# @example inst/examples/ex-mutator.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name mutator
#' @rdname mutator
#' @aliases get set
NULL

#' @rdname mutator
#' @aliases get_calendar-method
setGeneric(
  name = "get_calendar",
  def = function(x) standardGeneric("get_calendar")
)

#' @rdname mutator
#' @aliases get_hash-method
setGeneric(
  name = "get_hash",
  def = function(x) standardGeneric("get_hash")
)

## Subset ----------------------------------------------------------------------
#' Extract or Replace Parts of an Object
#'
#' Operators acting on objects to extract or replace parts.
#' @param x An object from which to extract element(s) or in which to replace
#'  element(s).
#' @param i,j Indices specifying elements to extract or replace.
#' @param drop A [`logical`] scalar: should the result be coerced to
#'  the lowest possible dimension? This only works for extracting elements,
#'  not for the replacement.
#' @param value A possible value for the element(s) of `x`.
#' @param ... Currently not used.
#' @return
#'  A subsetted object.
# @example inst/examples/ex-mutator.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name subset
#' @rdname subset
NULL

# Time Scale ===================================================================
#' Convert Time Scale
#'
#' Converts between BP and BC/AD time scales.
#' @param object A [`numeric`] vector or an [`MCMC-class`] object.
#' @return
#'  An object of the same sort as `object` with a new time scale.
#' @note
#'  There is no year 0 in BC/AD scale.
#' @author N. Frerebeau
#' @family time scale
#' @docType methods
#' @name convert
#' @rdname convert
NULL

#' @rdname convert
#' @aliases BP_to_BCAD-method
setGeneric(
  name = "BP_to_BCAD",
  def = function(object) standardGeneric("BP_to_BCAD")
)

#' @rdname convert
#' @aliases BCAD_to_BP-method
setGeneric(
  name = "BCAD_to_BP",
  def = function(object) standardGeneric("BCAD_to_BP")
)

# Events =======================================================================
## Tempo -----------------------------------------------------------------------
#' Tempo Plot
#'
#' A statistical graphic designed for the archaeological study of rhythms of the
#' long term that embodies a theory of archaeological evidence for the
#' occurrence of events.
#' @param object An [`MCMC-class`] or [`CumulativeEvents-class`] object.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param count A [`logical`] scalar: should the counting process be a number
#'  (the default) or a probability?
#' @param gauss A [`logical`] scalar: should the Gaussian approximation of the
#'  credible interval be used?
#' @param elapsed A [`logical`] scalar: should an elapsed time scale be used?
#' @param origin A length-one [`numeric`] vector giving the position of the
#'  column corresponding to the event from which elapsed time is calculated.
#'  Only used if `elapsed` is `TRUE`.
#' @param time A length-two [`numeric`] vector giving the earliest and latest
#'  date to estimate for, in BC/AD years. Ignored if `elapsed` is `TRUE`.
#' @param n An [`integer`] specifying the number of equally spaced points at
#'  which the cumulative distribution is to be estimated.
#' @param x A [`CumulativeEvents-class`] object.
#' @param calendar A [`character`] string specifying whether the dates
#'  should be displayed in BP or BC/AD. It must be one of "`BCAD`" (the default)
#'  or "`BP`". Any unambiguous substring can be given.
#' @param ... Currently not used.
#' @details
#'  The tempo plot is one way to measure change over time: it estimates the
#'  cumulative occurrence of archaeological events in a Bayesian calibration.
#'  The tempo plot yields a graphic where the slope of the plot directly
#'  reflects the pace of change: a period of rapid change yields a steep slope
#'  and a period of slow change yields a gentle slope. When there is no change,
#'  the plot is horizontal. When change is instantaneous, the plot is vertical.
#' @references
#'  Dye, T. S. (2016). Long-term rhythms in the development of Hawaiian social
#'  stratification. *Journal of Archaeological Science*, 71: 1-9.
#'  \doi{10.1016/j.jas.2016.05.006}.
#' @example inst/examples/ex-tempo.R
#' @author A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau
#' @family events
#' @docType methods
#' @name tempo
#' @rdname tempo
NULL

#' @rdname tempo
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
#' @param object An [`MCMC-class`] or [`CumulativeEvents-class`] object.
#' @param elapsed A [`logical`] scalar: should an elapsed time scale be used?
#' @param origin A length-one [`numeric`] vector giving the position of the
#'  column corresponding to the event from which elapsed time is calculated.
#'  Only used if `elapsed` is `TRUE`.
#' @param time A length-two [`numeric`] vector giving the earliest and latest
#'  date to estimate for, in BC/AD years. Ignored if `elapsed` is `TRUE`.
#' @param n An [`integer`] specifying the number of equally spaced points at
#'  which the density is to be estimated.
#' @param x An [`ActivityEvents-class`] object.
#' @param calendar A [`character`] string specifying whether the dates
#'  should be displayed in BP or BC/AD. It must be one of "`BCAD`" (the default)
#'  or "`BP`". Any unambiguous substring can be given.
#' @param ... Currently not used.
#' @references
#'  Dye, T. S. (2016). Long-term rhythms in the development of Hawaiian social
#'  stratification. *Journal of Archaeological Science*, 71: 1-9.
#'  \doi{10.1016/j.jas.2016.05.006}.
#' @example inst/examples/ex-activity.R
#' @author A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau
#' @family events
#' @docType methods
#' @name activity
#' @rdname activity
NULL

#' @rdname activity
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
#' @param object An [`MCMC-class`] or [`OccurrenceEvents-class`] object.
#' @param interval A [`character`] string specifying the confidence interval to
#'  be drawn. It must be one of "`ci`" (credible interval; the default)
#'  or "`hpdi`" (highest posterior density interval). Any unambiguous substring
#'  can be given.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param elapsed A [`logical`] scalar: should an elapsed time scale be used?
#' @param origin A length-one [`numeric`] vector giving the position of the
#'  column corresponding to the event from which elapsed time is calculated.
#'  Only used if `elapsed` is `TRUE`.
#' @param x A [`OccurrenceEvents-class`] object.
#' @param calendar A [`character`] string specifying whether the dates
#'  should be displayed in BP or BC/AD. It must be one of "`BCAD`" (the default)
#'  or "`BP`". Any unambiguous substring can be given.
#' @param ... Currently not used.
#' @details
#'  If we have \eqn{k} events, then we can estimate the calendar date \eqn{t}
#'  corresponding to the smallest date such that the number of events observed
#'  before \eqn{t} is equal to \eqn{k}.
#'
#'  The `occurrence()` estimates these occurrences and gives the credible
#'  interval or the highest posterior density (HPD) region for a given `level`
#'  of confidence.
#' @return An [`OccurrenceEvents-class`] object.
#' @example inst/examples/ex-occurrence.R
#' @author A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau
#' @family events
#' @docType methods
#' @name occurrence
#' @rdname occurrence
NULL

#' @rdname occurrence
#' @aliases occurrence-method
setGeneric(
  name = "occurrence",
  def = function(object, ...) standardGeneric("occurrence"),
  valueClass = "OccurrenceEvents"
)

# Interval =====================================================================
## CI --------------------------------------------------------------------------
#' Bayesian Credible Interval
#'
#' Computes the shortest credible interval of the output of the MCMC algorithm
#' for a single parameter.
#' @param object A [`numeric`] vector or an [`MCMC-class`] object containing the
#'  output of the MCMC algorithm for the parameter.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param ... Currently not used.
#' @details
#'  A \eqn{(100 \times level)}{(100 * level)}\% credible interval is an interval
#'  that keeps \eqn{N \times (1 - level)}{N * (1 - level)} elements of the
#'  sample outside the interval.
#'
#'  The \eqn{(100 \times level)}{(100 * level)}\% credible interval is the
#'  shortest of all those intervals.
#' @return
#'  A two columns `numeric` [`matrix`] giving the lower and upper boundaries of
#'  the credible interval.
#' @example inst/examples/ex-interval.R
#' @author A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau
#' @family statistics
#' @docType methods
#' @name ci
#' @rdname ci
NULL

#' @rdname ci
#' @aliases interval_credible-method
setGeneric(
  name = "interval_credible",
  def = function(object, ...) standardGeneric("interval_credible")
)

## HPDI ------------------------------------------------------------------------
#' Bayesian HPD Regions
#'
#' @param object A [`numeric`] vector or an [`MCMC-class`] object containing the
#'  output of the MCMC algorithm for the parameter.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param ... Extra arguments to be passed to [hdrcde::hdr()].
#' @return
#'  A [`list`] of two columns `numeric` [`matrix`] giving the lower and upper
#'  boundaries of the HPD interval.
#' @references
#'  Hyndman, R. J. (1996). Computing and graphing highest density regions.
#'  *American Statistician*, 50: 120-126. \doi{10.2307/2684423}.
#' @example inst/examples/ex-interval.R
#' @seealso [hdrcde::hdr()]
#' @author A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau
#' @family statistics
#' @docType methods
#' @name hpdi
#' @rdname hpdi
NULL

#' @rdname hpdi
#' @aliases interval_hpd-method
setGeneric(
  name = "interval_hpd",
  def = function(object, ...) standardGeneric("interval_hpd")
)

# Dates ========================================================================
#' Hiatus Between Two Dates
#'
#' Tests for the existence of a hiatus between two parameters.
#' @param x,y A [`numeric`] vector.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param ... Currently not used.
#' @details
#'  Finds if a gap exists between two dates and returns the longest interval
#'  that satisfies: \eqn{P(x < HiatusInf < HiatusSup < y | M) = level}
#' @author A. Philippe, M.-A. Vibet, N. Frerebeau
#' @family dates
#' @docType methods
#' @name lapse
#' @rdname lapse
NULL

#' @rdname lapse
#' @aliases lapse-method
setGeneric(
  name = "lapse",
  def = function(x, y, ...) standardGeneric("lapse")
)

## Anteriority -----------------------------------------------------------------
#' Bayesian Test for Anteriority/Posteriority Between Two Parameters
#'
#' This function estimates the posterior probability that event `x` is older
#' than event `b` using the output of the MCMC algorithm. This provides a
#' Bayesian test for checking the following assumption: "Event `x` is older than
#' event `y`".
#' @param x A [`numeric`] vector giving the output of the MCMC algorithm for the
#'  first parameter.
#' @param y A [`numeric`] vector giving the output of the MCMC algorithm for the
#'  second parameter.
#' @param ... Currently not used.
#' @details
#'  For a given output of MCMC algorithm, this function estimates the posterior
#'  probability of the event \eqn{x < y} by the relative frequency of the event
#'  "the value of event `x` is less than the value of event `y`" in the
#'  simulated Markov chain.
#' @return A [`numeric`] vector (the posterior probability of the assumption:
#'  "event `x` is older than event `y`").
#' @author A. Philippe, M.-A. Vibet, N. Frerebeau
#' @family tests
#' @docType methods
#' @name older
#' @rdname older
NULL

#' @rdname older
#' @aliases test_older-method
setGeneric(
  name = "test_older",
  def = function(x, y, ...) standardGeneric("test_older")
)

# Phase ========================================================================
## Boundaries ------------------------------------------------------------------
#' Phase
#'
#' Constructs the minimum and maximum for a group of events (phase).
#' @param x An [`MCMC-class`] or a [`PhasesMCMC-class`] object.
#' @param groups A [`list`].
#' @param ordered A [`logical`] scalar: should the `groups` be regarded as
#'  ordered (in the order given)?
#' @param value A possible value for the element(s) of `x`.
#' @param ... Currently not used.
#' @return
#'  A [`PhasesMCMC-class`] object.
#' @example inst/examples/ex-phase.R
#' @author A. Philippe, M.-A. Vibet, N. Frerebeau
#' @family phases
#' @docType methods
#' @name phase
#' @rdname phase
NULL

#' @rdname phase
#' @aliases phase-method
setGeneric(
  name = "phase",
  def = function(x, groups, ...) standardGeneric("phase"),
  valueClass = "PhasesMCMC"
)

#' @rdname phase
#' @aliases get_phases-method
setGeneric(
  name = "get_phases",
  def = function(x) standardGeneric("get_phases")
)

#' @rdname phase
#' @aliases get_order-method
setGeneric(
  name = "get_order",
  def = function(x, value) standardGeneric("get_order")
)

#' @rdname phase
#' @aliases set_order-method
setGeneric(
  name = "set_order<-",
  def = function(x, value) standardGeneric("set_order<-")
)

#' @rdname phase
#' @aliases as_ordered-method
setGeneric(
  name = "as_ordered",
  def = function(x) standardGeneric("as_ordered")
)

#' @rdname phase
#' @aliases is_ordered-method
setGeneric(
  name = "is_ordered",
  def = function(x) standardGeneric("is_ordered")
)

## Range -----------------------------------------------------------------------
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
#'  A [`data.frame`] containing the endpoints (in years BC/AD) of the shortest
#'  time range (at a given `level`).
#' @example inst/examples/ex-phase.R
#' @author A. Philippe, M.-A. Vibet, N. Frerebeau
#' @family phases
#' @docType methods
#' @rdname boundaries
#' @aliases boundaries-method
setGeneric(
  name = "boundaries",
  def = function(x, y, ...) standardGeneric("boundaries")
)

## Duration --------------------------------------------------------------------
#' Phase Duration
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, `x` must be an
#'  [`PhasesMCMC-class`] object.
#' @param ... Currently not used.
#' @example inst/examples/ex-phase.R
#' @author A. Philippe, M.-A. Vibet, N. Frerebeau
#' @family phases
#' @docType methods
#' @rdname duration
#' @aliases duration-method
setGeneric(
  name = "duration",
  def = function(x, y, ...) standardGeneric("duration")
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
#'  A [`data.frame`] containing the endpoints (in years BC/AD) of the
#'  transition interval for each pair of successive phases (at a given `level`).
#' @example inst/examples/ex-phase.R
#' @author A. Philippe, M.-A. Vibet, N. Frerebeau
#' @family phases
#' @docType methods
#' @rdname transition
#' @aliases transition-method
setGeneric(
  name = "transition",
  def = function(x, y, ...) standardGeneric("transition")
)

## Hiatus ----------------------------------------------------------------------
#' Hiatus Between Successive Phases
#'
#' Finds, if it exists, a hiatus between two successive phases.
#' @param x,y A [`numeric`] vector. If `y` is missing, `x` must be an
#'  [`PhasesMCMC-class`] object.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param ... Currently not used.
#' @details
#'  The hiatus is the longest interval that satisfies
#'  \eqn{P(Phase1Max < IntervalInf < IntervalSup < Phase2Min | M) = level}
#'
#'  This assumes that the phases are in temporal order constraint.
#' @return
#'  A [`data.frame`] containing the endpoints (in years BC/AD) of the hiatus
#'  between each pair of successive phases (at a given `level`).
#' @example inst/examples/ex-phase.R
#' @author A. Philippe, M.-A. Vibet, N. Frerebeau
#' @family phases
#' @docType methods
#' @rdname hiatus
#' @aliases hiatus-method
setGeneric(
  name = "hiatus",
  def = function(x, y, ...) standardGeneric("hiatus")
)

# Plot =========================================================================
#' Plot
#'
#' @param x An [`MCMC-class`] or a [`PhasesMCMC-class`] object.
#' @param calendar A [`character`] string specifying whether the dates
#'  should be displayed in BP or BC/AD. It must be one of "`BCAD`" (the default)
#'  or "`BP`". Any unambiguous substring can be given.
#' @param density A [`logical`] scalar: should estimated density be plotted?
#' @param n An [`integer`] specifying the number of equally spaced points at
#'  which the density is to be estimated (should be a power of two). Only used
#'  if `density` is `TRUE`.
#' @param interval A [`character`] string specifying the confidence interval to
#'  be drawn. It must be one of "`ci`" (credible interval; the default)
#'  or "`hpdi`" (highest posterior density interval). Any unambiguous substring
#'  can be given.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param decreasing A [`logical`] scalar: should the sort order be decreasing?
#' @param elapsed A [`logical`] scalar: should an elapsed time scale be used?
#' @param origin A length-one [`numeric`] vector giving the position of the
#'  column corresponding to the event from which elapsed time is calculated.
#'  Only used if `elapsed` is `TRUE`.
#' @param succession A [`logical`] scalar: should time ranges be plotted instead
#'  of densities?
#' @param facet A [`logical`] scalar: should a matrix of panels defined by phase
#'  be drawn?
#' @param ... Extra parameters to be passed to [`stats::density()`].
#' @example inst/examples/ex-summary.R
#' @seealso [`stats::density()`]
#' @author A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau
#' @family plot
#' @docType methods
#' @name plot
#' @rdname plot
#' @aliases plot-method
NULL

# Read =========================================================================
#' Read MCMC Output
#'
#' @inheritParams utils::read.table
#' @param BP A [`logical`] scalar: should the data be converted from BP to
#'  BC/AD? This should not be `TRUE` unless you change the default settings in
#'  'OxCal' or 'ChronoModel'.
#' @param bin_width The bin width specified for the
#'  '\href{https://bcal.shef.ac.uk/}{BCal}' calibration. Defaults to the 'BCal'
#'  default of 1.
#' @return
#'  An [`MCMC-class`] object.
#' @references
#'  Bronk Ramsey, C. (2009). Bayesian Analysis of Radiocarbon Dates.
#'  *Radiocarbon*, 51(1), 337-360. \doi{10.1017/S0033822200033865}.
#'
#'  Buck C. E., Christen J. A. & James G. N. (1999). BCal: an on-line Bayesian
#'  radiocarbon calibration tool. *Internet Archaeology*, 7.
#'  \doi{10.11141/ia.7.1}.
#'
#'  Lanos, Ph., Philippe, A., Lanos, H. & Dufresne, Ph. (2015). Chronomodel:
#'  Chronological Modeling of Archaeological Data using Bayesian Statistics.
#'  Version 2.0.18. URL: \url{https://www.chronomodel.fr}.
#' @example inst/examples/ex-read.R
#' @seealso [utils::read.table()]
#' @author T. S. Dye, N. Frerebeau
#' @family read
#' @docType methods
#' @name read
#' @rdname read
NULL

#' @rdname read
#' @aliases read_oxcal-method
setGeneric(
  name = "read_oxcal",
  def = function(file, ...) standardGeneric("read_oxcal"),
  valueClass = "MCMC"
)

#' @rdname read
#' @aliases read_bcal-method
setGeneric(
  name = "read_bcal",
  def = function(file, ...) standardGeneric("read_bcal"),
  valueClass = "MCMC"
)

#' @rdname read
#' @aliases read_chronomodel-method
setGeneric(
  name = "read_chronomodel",
  def = function(file, ...) standardGeneric("read_chronomodel"),
  valueClass = "MCMC"
)

#' Check for an Original MCMC File
#'
#' Checks whether or not a file is identical to the one used to create
#' a [`MCMC-class`] object.
#' @param object An [`MCMC-class`] object.
#' @param file Either a path to a CSV file or a connection.
#' @param ... Currently not used.
#' @return
#'  `TRUE` if the files match, `FALSE` otherwise.
#' @example inst/examples/ex-read.R
#' @author T. S. Dye, N. Frerebeau
#' @family read
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

# Summary ======================================================================
#' Marginal Summary Statistics for Multiple MCMC Chains
#'
#' Calculates summary statistics of the output of the MCMC algorithm for
#' multiple parameters. Results are given in calendar years (BC/AD).
#' @param object An [`MCMC-class`] or a [`PhasesMCMC-class`] object.
#' @param level A length-one [`numeric`] vector giving the confidence level.
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
#'   \item{CI_lower}{Lower credible interval of the MCMC chain at `level`.}
#'   \item{CI_upper}{Upper credible interval of the MCMC chain at `level`.}
#'  }
#' @example inst/examples/ex-summary.R
#' @author A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau
#' @family statistics
#' @docType methods
#' @name summary
#' @rdname summary
NULL
