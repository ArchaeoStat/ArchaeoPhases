# CLASSES DEFINITION AND INITIALIZATION

# Import classes ===============================================================
#' @importClassesFrom aion RataDie
#' @importClassesFrom aion TimeSeries
#' @importClassesFrom aion TimeIntervals
NULL

# MCMC =========================================================================
#' MCMC
#'
#' An S4 class to represent the output of a MCMC algorithm.
#' @slot .Data A [`numeric`] `matrix` giving the MCMC samples expressed in
#'  *[rata die][aion::RataDie-class]*.
#' @slot labels A [`character`] vector specifying the name of the events.
#' @slot depth A [`numeric`] vector giving the sample depth.
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
    labels = "character",
    depth = "numeric",
    iteration = "integer",
    hash = "character"
  ),
  contains = "matrix"
)

## Events ----------------------------------------------------------------------
#' MCMC Events
#'
#' An S4 class to represent a collection of events.
#' @note
#'  This class inherits from [`MCMC-class`].
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
#' An S4 class to represent a collection of phases.
#' @slot labels A [`character`] vector specifying the name of the phases.
#' @slot hash A [`character`] string giving the 32-byte MD5 hash of the
#'  original data file.
#' @details
#'  A phase object is an\eqn{n x m x 2}{n \times m \times 2} array, with
#'  \eqn{n} being the number of iterations, \eqn{m} being the number of phases
#'  and with the 2 columns of the third dimension containing the boundaries of
#'  the phases expressed in *[rata die][aion::RataDie-class]*.
#' @section Subset:
#'  In the code snippets below, `x` is a `PhasesMCMC` object.
#'  \describe{
#'   \item{`x[[i]]`}{Extracts a single phase (two chains) selected by subscript
#'   `i`. `i` is a length-one [`numeric`] or [`character`] vector.}
#'  }
#' @note
#'  This class inherits from [`array`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases PhasesMCMC-class
#' @keywords internal
.PhasesMCMC <- setClass(
  Class = "PhasesMCMC",
  slots = c(
    labels = "character",
    iteration = "integer",
    hash = "character"
  ),
  contains = "array"
)

# Time Range ===================================================================
#' Time Range
#'
#' An S4 class to represent time ranges.
#' @slot hash A [`character`] string giving the 32-byte MD5 hash of the
#'  original data file.
#' @section Coerce:
#'  In the code snippets below, `x` is a `TimeRange` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @section Plot:
#'  In the code snippets below, `x` is a `TimeRange` object.
#'  \describe{
#'   \item{`plot(x)`}{Results in a graphic being displayed
#'   (invisibly returns `x`).}
#'  }
#' @note
#'  This class inherits from [`aion::TimeIntervals-class`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases TimeRange-class
#' @keywords internal
.TimeRange <- setClass(
  Class = "TimeRange",
  slots = c(
    hash = "character"
  ),
  contains = "TimeIntervals"
)

# Tempo ========================================================================
#' Cumulative Events
#'
#' An S4 class to store the result of a [tempo][tempo()] plot.
#' @slot lower A [`numeric`] vector giving the lower boundaries of the
#'  credibility interval expressed in *[rata die][aion::RataDie-class]*.
#' @slot upper A [`numeric`] vector giving the upper boundaries of the
#'  credibility interval expressed in *[rata die][aion::RataDie-class]*.
#' @slot level A length-one [`numeric`] vector giving the confidence level.
#' @slot gauss A [`logical`] scalar indicating if the Gaussian approximation of
#'  the credible interval was used.
#' @slot counts A [`logical`] scalar.
#' @slot events An [`integer`] scalar giving the number of events included in
#'  the tempo plot.
#' @slot hash A [`character`] string giving the 32-byte MD5 hash of the
#'  original data file.
#' @section Coerce:
#'  In the code snippets below, `x` is a `CumulativeEvents` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @note
#'  This class inherits from [`aion::TimeSeries-class`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CumulativeEvents-class
#' @keywords internal
.CumulativeEvents <- setClass(
  Class = "CumulativeEvents",
  slots = c(
    credible = "matrix",
    gauss = "matrix",
    level = "numeric",
    counts = "logical",
    events = "integer",
    hash = "character"
  ),
  contains = "TimeSeries"
)

# Activity =====================================================================
#' Activity
#'
#' An S4 class to store the result of an [activity][activity()] plot.
#' @slot hash A [`character`] string giving the 32-byte MD5 hash of the
#'  original data file.
#' @section Coerce:
#'  In the code snippets below, `x` is an `ActivityEvents` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @note
#'  This class inherits from [`aion::TimeSeries-class`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases ActivityEvents-class
#' @keywords internal
.ActivityEvents <- setClass(
  Class = "ActivityEvents",
  slots = c(
    hash = "character"
  ),
  contains = "TimeSeries"
)

# Occurrence ===================================================================
#' Occurrence
#'
#' An S4 class to store the result of an [occurrence][occurrence()] plot.
#' @slot events An [`integer`] vector giving the occurrence.
#' @slot level A length-one [`numeric`] vector giving the confidence level.
#' @slot hash A [`character`] string giving the 32-byte MD5 hash of the
#'  original data file.
#' @section Coerce:
#'  In the code snippets below, `x` is an `OccurrenceEvents` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @section Plot:
#'  In the code snippets below, `x` is a `OccurrenceEvents` object.
#'  \describe{
#'   \item{`plot(x)`}{Results in a graphic being displayed
#'   (invisibly returns `x`).}
#'  }
#' @note
#'  This class inherits from [`aion::TimeIntervals-class`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases OccurrenceEvents-class
#' @keywords internal
.OccurrenceEvents <- setClass(
  Class = "OccurrenceEvents",
  slots = c(
    events = "integer",
    level = "numeric",
    hash = "character"
  ),
  contains = "TimeIntervals"
)

# Age-Depth Model ==============================================================
#' Age-Depth Model
#'
#' An S4 class to represents an age-depth model.
#' @slot depth A [`numeric`] vector giving the depth of the samples.
#' @slot model A [`list`] of local polynomial regressions
#'  (see [stats::loess()]).
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
    hash = "character"
  )
)
