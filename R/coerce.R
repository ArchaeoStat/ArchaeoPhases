# COERCE
#' @include AllGenerics.R
NULL

# To data.frame ================================================================
#' @method as.data.frame CumulativeEvents
#' @export
as.data.frame.CumulativeEvents <- function(x, ..., calendar = getOption("ArchaeoPhases.calendar")) {
  tmp <- data.frame(
    years = chronos::time(x@years, calendar = calendar),
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

#' @method as.data.frame ActivityEvents
#' @export
as.data.frame.ActivityEvents <- function(x, ..., calendar = getOption("ArchaeoPhases.calendar")) {
  data.frame(
    years = chronos::time(x@years, calendar = calendar),
    estimate = x[, 1, drop = TRUE]
  )
}

#' @method as.data.frame OccurrenceEvents
#' @export
as.data.frame.OccurrenceEvents <- function(x, ..., calendar = getOption("ArchaeoPhases.calendar")) {
  data.frame(
    events = x@events,
    start = chronos::time(x@start, calendar = calendar),
    end = chronos::time(x@end, calendar = calendar)
  )
}

#' @method as.data.frame TimeRange
#' @export
as.data.frame.TimeRange <- function(x, ..., calendar = getOption("ArchaeoPhases.calendar")) {

  ok <- !is.na(x@start)
  start <- x@start[ok]
  end <- x@end[ok]
  duration <- abs(end - start)

  ## Change calendar
  if (!is.null(calendar)) {
    start <- chronos::as_year(start, calendar = calendar)
    end <- chronos::as_year(end, calendar = calendar)
    duration <- chronos::as_year(duration, calendar = calendar)
  }

  data.frame(start = start, end = end, duration = duration,
             row.names = x@labels[ok])
}

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
