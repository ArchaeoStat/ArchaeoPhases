# DURATION
#' @include AllGenerics.R
NULL

# Duration =====================================================================
#' @export
#' @rdname duration
#' @aliases duration,numeric,numeric-method
setMethod(
  f = "duration",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    ## Validation
    arkhe::assert_length(y, length(x))

    abs(y - x)
  }
)

#' @export
#' @rdname duration
#' @aliases duration,PhasesMCMC,missing-method
setMethod(
  f = "duration",
  signature = c(x = "PhasesMCMC", y = "missing"),
  definition = function(x) {
    ## Get phases
    pha <- names(x)

    # Matrix of results
    result <- matrix(nrow = nrow(x), ncol = ncol(x))
    dimnames(result) <- list(rownames(x), pha)

    k <- seq_along(pha)
    for (i in k) {
      a <- x[, i, 1, drop = TRUE]
      b <- x[, i, 2, drop = TRUE]
      result[, i] <- duration(a, b)
    }

    .DurationsMCMC(
      result,
      labels = pha,
      hash = get_hash(x)
    )
  }
)
