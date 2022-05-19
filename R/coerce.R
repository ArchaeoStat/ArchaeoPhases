# COERCE
#' @include AllClasses.R AllGenerics.R
NULL

# To data.frame ================================================================
#' @method as.data.frame CumulativeEvents
#' @export
as.data.frame.CumulativeEvents <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  tmp <- data.frame(
    year = x@year,
    estimate = x@estimate,
    stringsAsFactors = stringsAsFactors
  )
  if (nrow(x@credible) > 0) {
    tmp$credible_lower <- x@credible[, 1]
    tmp$credible_upper <- x@credible[, 2]
  }
  if (nrow(x@gauss) > 0) {
    tmp$gauss_lower <- x@gauss[, 1]
    tmp$gauss_upper <- x@gauss[, 2]
  }
  tmp
}

#' @method as.data.frame ActivityEvents
#' @export
as.data.frame.ActivityEvents <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  data.frame(
    year = x@year,
    estimate = x@estimate,
    stringsAsFactors = stringsAsFactors
  )
}

#' @method as.data.frame OccurrenceEvents
#' @export
as.data.frame.OccurrenceEvents <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  data.frame(
    events = x@events,
    lower = x@lower,
    upper = x@upper,
    stringsAsFactors = stringsAsFactors
  )
}

#' @method as.data.frame RateOfChange
#' @export
as.data.frame.RateOfChange <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  data.frame(
    year = x@year,
    estimate = x@estimate,
    stringsAsFactors = stringsAsFactors
  )
}

# To list ======================================================================
#' @method as.list PhasesMCMC
#' @export
as.list.PhasesMCMC <- function(x, ...) {
  pha <- get_order(x)

  tmp <- vector(mode = "list", length = length(pha))
  names(tmp) <- pha

  k <- seq_along(pha)
  for (i in k) {
    tmp[[i]] <- x[[i]]
  }
  tmp
}

# To coda ======================================================================
#' @export
#' @rdname coda
#' @aliases as_coda,MCMC-method
setMethod(
  f = "as_coda",
  signature = "MCMC",
  definition = function(from, chains = 1) {
    L <- nrow(from) / chains
    obj <- vector(mode = "list", length = chains)

    for (i in 1:chains){
      index <- seq(from = L * (i - 1) + 1, to = L * i, by = 1)
      obj[[i]] <- coda::mcmc(from[index, ], start = 1, end = L)
    }

    coda::mcmc.list(obj)
  }
)

# To EventsMCMC ================================================================
## From matrix -----------------------------------------------------------------
#' @export
#' @rdname coerce
#' @aliases as_events,matrix-method
setMethod(
  f = "as_events",
  signature = "matrix",
  definition = function(from, BP = FALSE, iteration = NULL) {
    ## Remove the iteration column
    if (!is.null(iteration))
      from <- from[, -iteration]

    ## Convert from BP to CE
    if (BP)
      from <- BP_to_CE(from)

    ## Event names
    event_names <- colnames(from)
    if (is.null(event_names)) event_names <- paste0("E", seq_len(ncol(from)))

    .EventsMCMC(from, events = event_names, calendar = "CE")
  }
)

## From data.frame -------------------------------------------------------------
#' @export
#' @rdname coerce
#' @aliases as_events,data.frame-method
setMethod(
  f = "as_events",
  signature = "data.frame",
  definition = function(from, BP = FALSE, iteration = NULL) {
    from <- data.matrix(from)
    methods::callGeneric(from = from, BP = BP, iteration = iteration)
  }
)
