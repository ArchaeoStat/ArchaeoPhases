# TRANSITION
#' @include AllGenerics.R
NULL

# Transition ===================================================================
#' @export
#' @describeIn transition Returns a length-two [`numeric`] vector
#'  (terminal times of the transition interval).
#' @aliases transition,numeric,numeric-method
setMethod(
  f = "transition",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, level = 0.95) {
    boundaries(x, y, level = level)
  }
)

#' @export
#' @describeIn transition Returns a [`TimeRange-class`] object.
#' @aliases transition,PhasesMCMC,missing-method
setMethod(
  f = "transition",
  signature = c(x = "PhasesMCMC", y = "missing"),
  definition = function(x, level = 0.95) {
    ## Get phases
    n <- ncol(x)
    z <- names(x)

    ## Matrix of results
    start <- stop <- phase <- matrix(nrow = n, ncol = n, dimnames = list(z, z))

    k <- seq_len(n)
    for (i in k) {
      for (j in k) {
        if (i != j) {
          h <- boundaries(x[, i, 2, drop = TRUE], x[, j, 1, drop = TRUE], level = level)
          start[i, j] <- h["start"]
          stop[i, j] <- h["end"]
        }
        phase[i, j] <- paste(z[i], z[j], sep = "-")
      }
    }

    ## Remove false results
    drop <- start > stop
    stop[drop] <- NA
    start[drop] <- NA

    .TimeRange(
      start = start,
      end = stop,
      labels = phase,
      hash = get_hash(x)
    )
  }
)
