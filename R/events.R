# EVENTS
#' @include AllGenerics.R
NULL

# To EventsMCMC ================================================================
#' @export
#' @rdname as_events
#' @aliases as_events,matrix-method
setMethod(
  f = "as_events",
  signature = "matrix",
  definition = function(from, calendar, iteration = NULL) {
    ## Remove the iteration column
    iter <- seq_len(NROW(from))
    if (!is.null(iteration)) {
      iter <- from[, iteration]
      from <- from[, -iteration, drop = FALSE]
    }

    ## Event names
    event_names <- colnames(from)
    if (is.null(event_names)) event_names <- paste0("E", seq_len(ncol(from)))

    if (!is.null(calendar)) {
      ## Coerce to vector
      dn <- dimnames(from)
      d <- dim(from)
      dim(from) <- NULL

      ## Convert to rata die
      from <- chronos::fixed(from, calendar = calendar)
      dim(from) <- d
      dimnames(from) <- dn
    }

    ## Return an MCM object
    .EventsMCMC(
      from,
      labels = event_names,
      iteration = as.integer(iter)
    )
  }
)

#' @export
#' @rdname as_events
#' @aliases as_events,data.frame-method
setMethod(
  f = "as_events",
  signature = "data.frame",
  definition = function(from, calendar, iteration = NULL) {
    from <- data.matrix(from)
    methods::callGeneric(from = from, calendar = calendar,
                         iteration = iteration)
  }
)
