# CLASSES DEFINITION AND INITIALIZATION
NULL

# MCMC =========================================================================
#' MCMC
#'
#' An S4 class to represent the output of a MCMC algorithm.
#' @slot events A [`character`] vector specifying the name of the events.
#' @slot depth A [`numeric`] vector giving the sample depth.
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`CE`", "`BP`" or "`b2k`").
#' @slot hash A [`character`] string giving the 32-byte MD5 hash of the
#'  original data file.
#' @section Subset:
#'  In the code snippets below, `x` is a `MCMC` object.
#'  \describe{
#'   \item{`x[[i]]`}{Extracts a single event (one chain) selected by subscript
#'   `i`. `i` is a length-one [`numeric`] or [`character`] vector.}
#'  }
#' @note
#'  This class inherits from [`matrix`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases MCMC-class
#' @keywords internal
.MCMC <- setClass(
  Class = "MCMC",
  slots = c(
    events = "character",
    depth = "numeric",
    calendar = "character",
    hash = "character"
  ),
  contains = "matrix"
)

## Events ----------------------------------------------------------------------
#' MCMC Events
#'
#' S4 classes to represent a collection of events.
#' @note
#'  These classes inherit from [`MCMC-class`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases EventsMCMC-class
#' @keywords internal
.EventsMCMC <- setClass(Class = "EventsMCMC", contains = "MCMC")

## Duration --------------------------------------------------------------------
#' MCMC Duration
#'
#' S4 classes to represent the output of a MCMC algorithm.
#' @note
#'  This class inherits from [`MCMC-class`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases DurationsMCMC-class
#' @keywords internal
.DurationsMCMC <- setClass(Class = "DurationsMCMC", contains = "MCMC")

## Phases ----------------------------------------------------------------------
#' MCMC Phases
#'
#' S4 classes to represent a collection of phases.
#' @slot phases A [`character`] vector specifying the name of the phases.
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`CE`", "`BP`" or "`b2k`").
#' @slot hash A [`character`] string giving the 32-byte MD5 hash of the
#'  original data file.
#' @section Coerce:
#'  In the code snippets below, `x` is a `PhasesMCMC` object.
#'  \describe{
#'   \item{`as.list(x)`}{Coerces to a [`list`].}
#'  }
#' @section Subset:
#'  In the code snippets below, `x` is a `PhasesMCMC` object.
#'  \describe{
#'   \item{`x[[i]]`}{Extracts a single phase (two chains) selected by subscript
#'   `i`. `i` is a length-one [`numeric`] or [`character`] vector.}
#'  }
#' @note
#'  Theses classes inherit from [`array`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases PhasesMCMC-class
#' @keywords internal
.PhasesMCMC <- setClass(
  Class = "PhasesMCMC",
  slots = c(
    phases = "character",
    calendar = "character",
    hash = "character"
  ),
  contains = "array"
)

# Time Range ===================================================================
#' Cumulative Events
#'
#' An S4 class to represent time ranges.
#' @slot start,stop A `numeric` [`matrix`] giving the lower and upper
#'  boundaries.
#' @slot names A [`character`] vector specifying the name of the events/phases.
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`CE`", "`BP`" or "`b2k`").
#' @slot hash A [`character`] string giving the 32-byte MD5 hash of the
#'  original data file.
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases TimeRange-class
#' @keywords internal
.TimeRange <- setClass(
  Class = "TimeRange",
  slots = c(
    start = "matrix",
    stop = "matrix",
    names = "matrix",
    calendar = "character",
    hash = "character"
  )
)

# Tempo ========================================================================
#' Cumulative Events
#'
#' An S4 class to store the result of a [`tempo`] plot.
#' @slot years A [`numeric`] vector giving the time points at which the
#'  cumulative distribution is estimated.
#' @slot estimate A [`numeric`] vector giving the estimation of the cumulative
#'  distribution.
#' @slot lower A [`numeric`] vector giving the lower boundaries of the
#'  credibility interval.
#' @slot upper A [`numeric`] vector giving the upper boundaries of the
#'  credibility interval.
#' @slot level A length-one [`numeric`] vector giving the confidence level.
#' @slot gauss A [`logical`] scalar indicating if the Gaussian approximation of
#'  the credible interval was used.
#' @slot counts A [`logical`] scalar.
#' @slot events An [`integer`] scalar giving the number of events included in
#'  the tempo plot.
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`CE`", "`BP`" or "`b2k`").
#' @slot hash A [`character`] string giving the 32-byte MD5 hash of the
#'  original data file.
#' @section Coerce:
#'  In the code snippets below, `x` is a `CumulativeEvents` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CumulativeEvents-class
#' @keywords internal
.CumulativeEvents <- setClass(
  Class = "CumulativeEvents",
  slots = c(
    years = "numeric",
    estimate = "numeric",
    credible = "matrix",
    gauss = "matrix",
    level = "numeric",
    counts = "logical",
    events = "integer",
    calendar = "character",
    hash = "character"
  )
)

# Activity =====================================================================
#' Activity
#'
#' An S4 class to store the result of an [`activity`] plot.
#' @slot years A [`numeric`] vector giving the time points at which the
#'  distribution is estimated.
#' @slot estimate A [`numeric`] vector giving the estimation of the
#'  distribution.
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`CE`", "`BP`" or "`b2k`").
#' @slot hash A [`character`] string giving the 32-byte MD5 hash of the
#'  original data file.
#' @section Coerce:
#'  In the code snippets below, `x` is an `ActivityEvents` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases ActivityEvents-class
#' @keywords internal
.ActivityEvents <- setClass(
  Class = "ActivityEvents",
  slots = c(
    years = "numeric",
    estimate = "numeric",
    calendar = "character",
    hash = "character"
  )
)

# Occurrence ===================================================================
#' Occurrence
#'
#' An S4 class to store the result of an [`occurrence`] plot.
#' @slot events An [`integer`] vector giving the occurrence.
#' @slot start A [`numeric`] vector giving the lower boundaries of the
#'  credibility interval.
#' @slot stop A [`numeric`] vector giving the upper boundaries of the
#'  credibility interval.
#' @slot level A length-one [`numeric`] vector giving the confidence level.
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`CE`", "`BP`" or "`b2k`").
#' @slot hash A [`character`] string giving the 32-byte MD5 hash of the
#'  original data file.
#' @section Coerce:
#'  In the code snippets below, `x` is an `OccurrenceEvents` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases OccurrenceEvents-class
#' @keywords internal
.OccurrenceEvents <- setClass(
  Class = "OccurrenceEvents",
  slots = c(
    events = "integer",
    start = "numeric",
    stop = "numeric",
    level = "numeric",
    calendar = "character",
    hash = "character"
  )
)

# Rate of Change ===============================================================
#' Rate of Change
#'
#' An S4 class to store the result of a [`rate of change`][`roc`] estimation.
#' @slot years A [`numeric`] vector giving the time points at which the
#'  rate fo change is estimated.
#' @slot estimate A [`numeric`] vector giving the estimation of the
#'  rate of change.
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`CE`", "`BP`" or "`b2k`").
#' @slot hash A [`character`] string giving the 32-byte MD5 hash of the
#'  original data file.
#' @section Coerce:
#'  In the code snippets below, `x` is an `RateOfChange` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases RateOfChange-class
#' @keywords internal
.RateOfChange <- setClass(
  Class = "RateOfChange",
  slots = c(
    years = "numeric",
    estimate = "numeric",
    calendar = "character",
    hash = "character"
  )
)

# Age-Depth Model ==============================================================
#' Age-Depth Model
#'
#' An S4 class to represents an age-depth model.
#' @slot depth A [`numeric`] vector giving the depth of the samples.
#' @slot model A [`list`] of local polynomial regressions
#'  (see [stats::loess()]).
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`CE`", "`BP`" or "`b2k`").
#' @slot hash A [`character`] string giving the 32-byte MD5 hash of the
#'  original data file.
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases AgeDepthModel-class
#' @keywords internal
.AgeDepthModel <- setClass(
  Class = "AgeDepthModel",
  slots = c(
    depth = "numeric",
    model = "list",
    calendar = "character",
    hash = "character"
  )
)
