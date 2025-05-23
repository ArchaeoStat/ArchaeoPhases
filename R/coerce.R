# COERCE
#' @include AllGenerics.R
NULL

# To data.frame ================================================================
#' @export
#' @method as.data.frame CumulativeEvents
as.data.frame.CumulativeEvents <- function(x, ..., calendar = get_calendar()) {
  tmp <- data.frame(
    time = aion::time(x, calendar = calendar),
    estimate = x[, 1, 1, drop = TRUE]
  )
  if (nrow(x@credible) > 0) {
    tmp$credible_start <- x@credible[, 1]
    tmp$credible_stop <- x@credible[, 2]
  } else {
    tmp$credible_start <- NA_real_
    tmp$credible_stop <- NA_real_
  }
  if (nrow(x@gauss) > 0) {
    tmp$gauss_start <- x@gauss[, 1]
    tmp$gauss_stop <- x@gauss[, 2]
  } else {
    tmp$gauss_start <- NA_real_
    tmp$gauss_stop <- NA_real_
  }
  tmp
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,CumulativeEvents-method
setMethod("as.data.frame", "CumulativeEvents", as.data.frame.CumulativeEvents)

#' @export
#' @method as.data.frame ActivityEvents
as.data.frame.ActivityEvents <- function(x, ..., calendar = get_calendar()) {
  methods::callNextMethod() # Method for 'TimeSeries'
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,ActivityEvents-method
setMethod("as.data.frame", "ActivityEvents", as.data.frame.ActivityEvents)

#' @export
#' @method as.data.frame OccurrenceEvents
as.data.frame.OccurrenceEvents <- function(x, ..., calendar = get_calendar()) {
  data.frame(
    events = x@events,
    start = aion::start(x, calendar = calendar),
    end = aion::end(x, calendar = calendar)
  )
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,OccurrenceEvents-method
setMethod("as.data.frame", "OccurrenceEvents", as.data.frame.OccurrenceEvents)

#' @export
#' @method as.data.frame TimeRange
as.data.frame.TimeRange <- function(x, ..., calendar = get_calendar()) {
  ## Build a data frame
  data.frame(
    label = labels(x),
    start = aion::start(x, calendar = calendar),
    end = aion::end(x, calendar = calendar),
    duration = aion::span(x, calendar = calendar)
  )
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,TimeRange-method
setMethod("as.data.frame", "TimeRange", as.data.frame.TimeRange)

# To coda ======================================================================
#' @export
#' @rdname as_coda
#' @aliases as_coda,MCMC-method
setMethod(
  f = "as_coda",
  signature = "MCMC",
  definition = function(from, chains = 1) {
    ## Validation
    arkhe::assert_package("coda")

    L <- nrow(from) / chains
    obj <- vector(mode = "list", length = chains)

    for (i in 1:chains){
      index <- seq(from = L * (i - 1) + 1, to = L * i, by = 1)
      obj[[i]] <- coda::mcmc(from[index, , drop = TRUE], start = 1, end = L)
    }

    coda::mcmc.list(obj)
  }
)
