# BOUNDARIES
#' @include AllGenerics.R
NULL

# Time range ===================================================================
#' @export
#' @describeIn boundaries Returns a length-two [`numeric`] vector
#'  (terminal times).
#' @aliases boundaries,numeric,numeric-method
setMethod(
  f = "boundaries",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, level = 0.95) {
    ## Validation
    arkhe::assert_length(y, length(x))

    # no_bound <- c(start = NA, stop = NA)
    # if (older(x, y) < 0.5) {
    #   warning("Events do not seem to be in chronological order; ",
    #           "NAs introduced.", call. = FALSE)
    #   return(no_bound)
    # }

    epsilon <- seq(from = 0, to = 1 - level, by = 0.001)
    p <- periode(epsilon, x, y, level = level)

    ## Compute the length of all intervals
    inter <- p[2, ] - p[1, ]

    ## Find the shortest interval
    short <- which.min(inter)
    endpoints <- p[, short]

    ## Return the endpoints of the shortest interval
    bound <- c(start = endpoints[[1]], end = endpoints[[2]])
    bound
  }
)

#' @export
#' @describeIn boundaries Returns a [`TimeRange-class`] object.
#' @aliases boundaries,PhasesMCMC,missing-method
setMethod(
  f = "boundaries",
  signature = c(x = "PhasesMCMC", y = "missing"),
  definition = function(x, level = 0.95) {
    ## Get phases
    n <- ncol(x)
    z <- names(x)

    ## Matrix of results
    result <- matrix(nrow = n, ncol = 2)
    start <- end <- matrix(data = NA_real_, nrow = n, ncol = n,
                           dimnames = list(z, z))

    k <- seq_len(n)
    for (i in k) {
      a <- x[, i, 1, drop = TRUE]
      b <- x[, i, 2, drop = TRUE]
      result[i, ] <- boundaries(a, b, level = level)
    }

    pha <- paste(rep(z, n), rep(z, each = n), sep = "-")
    dim(pha) <- c(n, n)
    diag(pha) <- z
    diag(start) <- result[, 1]
    diag(end) <- result[, 2]

    .TimeRange(
      start = start,
      end = end,
      labels = pha,
      hash = get_hash(x)
    )
  }
)
