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
    if (is.null(dim(z))) z <- matrix(z, ncol = 1)

    lab <- x@labels
    itr <- x@iteration
    dep <- x@depth
    if (!missing(i)) {
      if (is.character(i)) i <- match(i, rownames(x))
      rownames(z) <- rownames(x)[i]
      itr <- itr[i]
    }
    if (!missing(j)) {
      if (is.character(j)) j <- match(j, lab)
      colnames(z) <- colnames(x)[j]
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
  function(x, i, j, k, drop = FALSE) {
    z <- x@.Data
    lab <- x@labels
    itr <- x@iteration

    z <- z[i, j, k, drop = drop]
    if (!missing(i)) {
      if (is.character(i)) i <- match(i, dimnames(x)[1L])
      # itr <- itr[i]
    }
    if (!missing(j)) {
      if (is.character(j)) j <- match(j, lab)
      lab <- lab[j]
    }

    if (isTRUE(drop)) return(z)
    methods::initialize(x, z, labels = lab)
  }
)
