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
    z <- methods::callNextMethod()

    if (is.null(dim(z)) || isTRUE(drop)) return(z)
    if (length(dim(z)) == 2) {
      n <- dim(z)[1L]
      m <- if (!missing(j)) length(j) else dim(x)[2L]
      p <- if (!missing(k)) length(k) else dim(x)[3L]
      z <- array(z, dim = c(n, m, p))
    }

    lab <- x@labels
    if (!missing(i)) {
      if (is.character(i)) i <- match(i, dimnames(x)[[1L]])
      if (!is.null(dimnames(x)[[1L]])) dimnames(z)[[1L]] <- dimnames(x)[[1L]][i]
    }
    if (!missing(j)) {
      if (is.character(j)) j <- match(j, lab)
      if (!is.null(dimnames(x)[[2L]])) dimnames(z)[[2L]] <- dimnames(x)[[2L]][j]
      lab <- lab[j]
    }
    if (!missing(k)) {
      if (is.character(k)) k <- match(k, dimnames(x)[[3L]])
      if (!is.null(dimnames(x)[[3L]])) dimnames(z)[[3L]] <- dimnames(x)[[3L]][k]
    }
    methods::initialize(x, z, labels = lab)
  }
)
