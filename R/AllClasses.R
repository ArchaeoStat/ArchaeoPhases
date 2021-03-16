# CLASSES DEFINITION AND INITIALIZATION
NULL

# MCMC =========================================================================
#' MCMC
#'
#' S4 classes to represent the output of a MCMC algorithm.
#' @slot start An [`integer`] vector containing the column number
#' corresponding to the minimum of the events included in each phase.
#' @slot end An [`integer`] vector containing the column number
#' corresponding to the maximum of the phases set in the same order as
#' in `start`.
#' @slot ordered A [`logical`] scalar: should the phases be regarded as ordered
#'  (in the order given in `phases`)?
#' @slot phases A [`character`] vector specifying the name of the phases.
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`BP`" or "`BCAD`").
#' @slot hash A [`character`] string specifying the SHA256 hash of the csv file.
#' @note
#'  This class inherits from [`matrix`].
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
#' @aliases PhasesMCMC-class
.PhasesMCMC <- setClass(
  Class = "PhasesMCMC",
  slots = c(
    start = "integer",
    end = "integer",
    ordered = "logical",
    phases = "character"
  ),
  prototype = methods::prototype(
    start = integer(0),
    end = integer(0),
    ordered = FALSE,
    phases = character(0)
  ),
  contains = "MCMC"
)

# Tempo ========================================================================
#' Cumulative Events
#'
#' An S4 class to store the result of a [`tempo`] plot.
#' @slot level A length-one [`numeric`] vector giving the confidence level.
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`BP`" or "`BCAD`").
#' @slot hash A [`character`] string giving the a cryptographical hash of the
#'  original data file.
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
#' @slot hash A [`character`] string giving the a cryptographical hash of the
#'  original data file.
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
#' @slot level A length-one [`numeric`] vector giving the confidence level.
#' @slot calendar A [`character`] string specifying the chronological scale
#'  (either "`BP`" or "`BCAD`").
#' @slot hash A [`character`] string giving the a cryptographical hash of the
#'  original data file.
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
