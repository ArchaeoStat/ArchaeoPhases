# CLASSES DEFINITION AND INITIALIZATION
NULL

# MCMC =========================================================================
#' MCMC
#'
#' S4 classes to represent the output of a MCMC algorithm.
#' @slot start An [`integer`] vector containing the column number corresponding
#'  to the minimum of each phase.
#' @slot end An [`integer`] vector containing the column number corresponding to
#'  the maximum of each phase (set in the same order as in `start`).
#' @slot phases A [`factor`] vector specifying the name of the phases.
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`BP`" or "`BCAD`").
#' @slot hash A [`character`] string giving the cryptographical hash of the
#'  original data file.
#' @section Coerce:
#'  In the code snippets below, `x` is a `PhasesMCMC` object.
#'  \describe{
#'   \item{`as.list(x)`}{Coerces to a [`list`].}
#'  }
#' @section Subset:
#'  In the code snippets below, `x` is a `*MCMC` object.
#'  \describe{
#'   \item{`x[[i]]`}{Extracts a date/event (single chain) or a phase (two
#'   chains) selected by subscript `i`. `i` is a length-one [`numeric`] or
#'   [`character`] vector.}
#'  }
#' @note
#'  These classes inherit from [`matrix`].
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @name MCMC
#' @rdname MCMC
NULL

#' @rdname MCMC
#' @aliases MCMC-class
.MCMC <- setClass(
  Class = "MCMC",
  slots = c(
    calendar = "character",
    hash = "character"
  ),
  prototype = methods::prototype(
    calendar = "BCAD",
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

#' @rdname MCMC
#' @aliases PhasesMCMC-class
.PhasesMCMC <- setClass(
  Class = "PhasesMCMC",
  slots = c(
    start = "integer",
    end = "integer",
    phases = "factor"
  ),
  prototype = methods::prototype(
    start = integer(0),
    end = integer(0),
    phases = factor()
  ),
  contains = "MCMC"
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
#'  (either "`BP`" or "`BCAD`").
#' @slot hash A [`character`] string giving the cryptographical hash of the
#'  original data file.
#' @section Coerce:
#'  In the code snippets below, `x` is a `CumulativeEvents` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @name CumulativeEvents
#' @rdname CumulativeEvents
NULL

#' @rdname CumulativeEvents
#' @aliases CumulativeEvents-class
.CumulativeEvents <- setClass(
  Class = "CumulativeEvents",
  slots = c(
    year = "numeric",
    estimate = "numeric",
    lower = "numeric",
    upper = "numeric",
    gauss = "logical",
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
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`BP`" or "`BCAD`").
#' @slot hash A [`character`] string giving the cryptographical hash of the
#'  original data file.
#' @section Coerce:
#'  In the code snippets below, `x` is an `ActivityEvents` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @name ActivityEvents
#' @rdname ActivityEvents
NULL

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
#'  (either "`BP`" or "`BCAD`").
#' @slot hash A [`character`] string giving the cryptographical hash of the
#'  original data file.
#' @section Coerce:
#'  In the code snippets below, `x` is an `OccurrenceEvents` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @name OccurrenceEvents
#' @rdname OccurrenceEvents
NULL

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
