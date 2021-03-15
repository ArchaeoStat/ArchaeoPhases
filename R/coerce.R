# COERCE
#' @include AllClasses.R AllGenerics.R
NULL

# To data.frame ================================================================
#' @method as.data.frame CumulativeEvents
#' @export
as.data.frame.CumulativeEvents <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  data.frame(
    year = x@year,
    estimate = x@estimate,
    lower = x@lower,
    upper = x@upper,
    stringsAsFactors = stringsAsFactors
  )
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
      obj[[i]] <- coda::mcmc(from[index], start = 1, end = L)
    }

    coda::mcmc.list(obj)
  }
)

# To MCMC ======================================================================
## From matrix -----------------------------------------------------------------
#' @export
#' @rdname coerce
#' @aliases as_mcmc,matrix-method
setMethod(
  f = "as_mcmc",
  signature = "matrix",
  definition = function(from, BP = FALSE, iteration = NULL) {
    ## Remove the iteration column
    if (!is.null(iteration))
      from <- from[, -iteration]

    ## Convert from BP to BC/AD
    if (BP)
      from <- BP_to_BCAD(from)

    .MCMC(from, calendar = "BCAD")
  }
)

#' @export
#' @rdname coerce
#' @aliases as_phases,matrix-method
setMethod(
  f = "as_phases",
  signature = "matrix",
  definition = function(from, start, end, BP = FALSE, iteration = NULL) {
    ## Remove the iteration column
    if (!is.null(iteration))
      from <- from[, -iteration]

    if (missing(start)) {
      start <- seq(from = 1, to = ncol(from), by = 2)
    }
    if (missing(end)) {
      end <- start + 1
    }

    ## Convert from BP to BC/AD
    if (BP)
      from <- BP_to_BCAD(from)

    .PhasesMCMC(
      from,
      start = as.integer(start),
      end = as.integer(end),
      ordered = FALSE,
      phases = paste0("phase_", seq_along(start)),
      calendar = "BCAD"
    )
  }
)

## From data.frame -------------------------------------------------------------
#' @export
#' @rdname coerce
#' @aliases as_mcmc,data.frame-method
setMethod(
  f = "as_mcmc",
  signature = "data.frame",
  definition = function(from, BP = FALSE, iteration = NULL) {
    from <- data.matrix(from)
    methods::callGeneric(from = from, BP = BP, iteration = iteration)
  }
)

#' @export
#' @rdname coerce
#' @aliases as_phases,data.frame-method
setMethod(
  f = "as_phases",
  signature = "data.frame",
  definition = function(from, start, end, BP = FALSE, iteration = NULL) {
    from <- data.matrix(from)
    methods::callGeneric(from, start, end, BP = BP, iteration = iteration)
  }
)
