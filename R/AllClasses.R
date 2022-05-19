# CLASSES DEFINITION AND INITIALIZATION
NULL

# MCMC =========================================================================
## Events ----------------------------------------------------------------------
#' MCMC Events
#'
#' S4 classes to represent the output of a MCMC algorithm.
#' @slot events A [`character`] vector specifying the name of the events.
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`BP`", "`CE`" or "`b2k`").
#' @slot hash A [`character`] string giving the 32-byte MD5 hash of the
#'  original data file.
#' @section Subset:
#'  In the code snippets below, `x` is a `*MCMC` object.
#'  \describe{
#'   \item{`x[[i]]`}{Extracts a single date/event (one chain) selected by
#'   subscript `i`. `i` is a length-one [`numeric`] or [`character`] vector.}
#'  }
#' @note
#'  These classes inherit from [`matrix`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @name MCMC
#' @rdname MCMC
NULL

#' @rdname MCMC
#' @aliases MCMC-class
.MCMC <- setClass(
  Class = "MCMC",
  slots = c(
    events = "character",
    calendar = "character",
    hash = "character"
  ),
  prototype = methods::prototype(
    events = character(0),
    calendar = "CE",
    hash = character(0)
  ),
  contains = "matrix"
)

#' @rdname MCMC
#' @aliases EventsMCMC-class
.EventsMCMC <- setClass(
  Class = "EventsMCMC",
  contains = "MCMC"
)

## Phases ----------------------------------------------------------------------
#' MCMC Phases
#'
#' An S4 class to represent the output of a MCMC algorithm.
#' @slot phases A [`character`] vector specifying the name of the phases.
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`BP`" or "`CE`").
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
#'  This class inherits from [`array`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @rdname PhasesMCMC
#' @aliases PhasesMCMC-class
.PhasesMCMC <- setClass(
  Class = "PhasesMCMC",
  slots = c(
    phases = "character",
    ordered = "logical",
    calendar = "character",
    hash = "character"
  ),
  prototype = methods::prototype(
    phases = character(0),
    ordered = FALSE,
    calendar = "CE",
    hash = character(0)
  ),
  contains = "array"
)

# Tempo ========================================================================
#' Cumulative Events
#'
#' An S4 class to store the result of a [`tempo`] plot.
#' @slot year A [`numeric`] vector giving the time points at which the
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
#'  (either "`BP`" or "`CE`").
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
#' @rdname CumulativeEvents
#' @aliases CumulativeEvents-class
.CumulativeEvents <- setClass(
  Class = "CumulativeEvents",
  slots = c(
    year = "numeric",
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
#' @slot year A [`numeric`] vector giving the time points at which the
#'  distribution is estimated.
#' @slot estimate A [`numeric`] vector giving the estimation of the
#'  distribution.
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`BP`" or "`CE`").
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
#' @rdname ActivityEvents
#' @aliases ActivityEvents-class
.ActivityEvents <- setClass(
  Class = "ActivityEvents",
  slots = c(
    year = "numeric",
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
#' @slot lower A [`numeric`] vector giving the lower boundaries of the
#'  credibility interval.
#' @slot upper A [`numeric`] vector giving the upper boundaries of the
#'  credibility interval.
#' @slot level A length-one [`numeric`] vector giving the confidence level.
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`BP`" or "`CE`").
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
#' @rdname OccurrenceEvents
#' @aliases OccurrenceEvents-class
.OccurrenceEvents <- setClass(
  Class = "OccurrenceEvents",
  slots = c(
    events = "integer",
    lower = "numeric",
    upper = "numeric",
    level = "numeric",
    calendar = "character",
    hash = "character"
  )
)

# Rate of Change ===============================================================
#' Rate of Change
#'
#' An S4 class to store the result of a [`rate of change`][`roc`] estimation.
#' @slot year A [`numeric`] vector giving the time points at which the
#'  rate fo change is estimated.
#' @slot estimate A [`numeric`] vector giving the estimation of the
#'  rate of change.
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`BP`" or "`CE`").
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
#' @rdname RateOfChange
#' @aliases RateOfChange-class
.RateOfChange <- setClass(
  Class = "RateOfChange",
  slots = c(
    year = "numeric",
    estimate = "numeric",
    calendar = "character",
    hash = "character"
  )
)

# Proxy Record =================================================================
#' Proxy Record
#'
#' An S4 class to store proxy records.
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @rdname ProxyRecord
#' @aliases ProxyRecord-class
.ProxyRecord <- setClass(
  Class = "ProxyRecord",
  slots = c(
    depth = "numeric",
    proxy = "numeric",
    proxy_error = "numeric",
    time = "numeric",
    time_error = "numeric",
    time_grid = "numeric",
    calendar = "character",
    density = "matrix",
    samples = "matrix"
  )
)
