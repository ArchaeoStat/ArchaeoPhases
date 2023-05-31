# ACCESSORS
#' @include AllClasses.R
NULL

# Extract ======================================================================
## [ ---------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [,MCMC-method
setMethod(
  f = "[",
  signature = c(x = "MCMC"),
  function(x, i, j, ..., drop = FALSE) {
    z <- methods::callNextMethod()
    if (isTRUE(drop)) return(z)

    lab <- x@labels
    itr <- x@iteration
    dep <- x@depth
    if (!missing(i)) {
      if (is.character(i)) i <- match(i, lab)
      itr <- itr[i]
    }
    if (!missing(j)) {
      if (is.character(j)) j <- match(j, lab)
      lab <- lab[j]
      dep <- dep[j]
    }
    methods::initialize(x, z, labels = lab, depth = dep, iteration = itr)
  }
)

#' @export
#' @rdname subset
#' @aliases [,PhasesMCMC-method
setMethod(
  f = "[",
  signature = c(x = "PhasesMCMC"),
  function(x, i, j, ..., drop = FALSE) {
    z <- methods::callNextMethod()
    if (isTRUE(drop)) return(z)

    lab <- x@labels
    if (!missing(j)) {
      if (is.character(j)) j <- match(j, lab)
      lab <- lab[j]
    }
    methods::initialize(x, z, labels = lab)
  }
)
