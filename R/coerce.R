# COERCE
#' @include AllGenerics.R
NULL

# To data.frame ================================================================
#' @export
#' @method as.data.frame CumulativeEvents
as.data.frame.CumulativeEvents <- function(x, ..., calendar = getOption("ArchaeoPhases.calendar")) {
  tmp <- data.frame(
    time = aion::time(x, calendar = calendar),
    estimate = x[, 1, drop = TRUE]
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
as.data.frame.ActivityEvents <- function(x, ..., calendar = getOption("ArchaeoPhases.calendar")) {
  methods::callNextMethod() # Method for 'TimeSeries'
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,ActivityEvents-method
setMethod("as.data.frame", "ActivityEvents", as.data.frame.ActivityEvents)

#' @export
#' @method as.data.frame OccurrenceEvents
as.data.frame.OccurrenceEvents <- function(x, ..., calendar = getOption("ArchaeoPhases.calendar")) {
  data.frame(
    events = x@events,
    start = aion::as_year(x@start, calendar = calendar),
    end = aion::as_year(x@end, calendar = calendar)
  )
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,OccurrenceEvents-method
setMethod("as.data.frame", "OccurrenceEvents", as.data.frame.OccurrenceEvents)

#' @export
#' @method as.data.frame TimeRange
as.data.frame.TimeRange <- function(x, ..., calendar = getOption("ArchaeoPhases.calendar")) {

  ok <- !is.na(x@start)
  start <- x@start[ok]
  end <- x@end[ok]
  duration <- abs(end - start)

  ## Change calendar
  if (!is.null(calendar)) {
    start <- aion::as_year(start, calendar = calendar)
    end <- aion::as_year(end, calendar = calendar)
    duration <- aion::as_year(duration, calendar = calendar)
  }

  data.frame(start = start, end = end, duration = duration,
             row.names = x@labels[ok])
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,TimeRange-method
setMethod("as.data.frame", "TimeRange", as.data.frame.TimeRange)

# To list ======================================================================
#' @method as.list PhasesMCMC
#' @export
as.list.PhasesMCMC <- function(x, ...) {
  n <- ncol(x)
  tmp <- vector(mode = "list", length = n)
  names(tmp) <- names(x)

  k <- seq_len(n)
  for (i in k) {
    tmp[[i]] <- x[, i, , drop = TRUE]
  }
  tmp
}

# To coda ======================================================================
#' @export
#' @rdname as_coda
#' @aliases as_coda,MCMC-method
setMethod(
  f = "as_coda",
  signature = "MCMC",
  definition = function(from, chains = 1) {
    ## Validation
    arkhe::needs("coda")

    L <- nrow(from) / chains
    obj <- vector(mode = "list", length = chains)

    for (i in 1:chains){
      index <- seq(from = L * (i - 1) + 1, to = L * i, by = 1)
      obj[[i]] <- coda::mcmc(from[index, , drop = TRUE], start = 1, end = L)
    }

    coda::mcmc.list(obj)
  }
)
