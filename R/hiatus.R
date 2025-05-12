# HIATUS
#' @include AllGenerics.R
NULL

#' @export
#' @describeIn hiatus Returns a length-three [`numeric`] vector (terminal times
#'  and hiatus duration, if any).
#' @aliases hiatus,numeric,numeric-method
setMethod(
  f = "hiatus",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, level = 0.95) {
    ## Validation
    arkhe::assert_length(y, length(x))

    no_hiatus <- c(start = NA, stop = NA)

    gamma <- mean(x < y)
    if (gamma < level) return(no_hiatus)

    ind <- which(x < y)
    epsilon <- seq(0, 1 - level / gamma, .001)
    p <- gap(epsilon, x[ind], y[ind], level / gamma)

    ## Compute the length of all intervals
    d <- p[2, ] - p[1, ]
    dd <- d[d > 0]

    if (length(dd) < 1) return(no_hiatus)

    i <- which(d == max(dd))
    i <- i[[1L]]
    endpoints <- p[, i]

    if (p[2, i] == p[1, i]) return(no_hiatus)

    inf <- endpoints[[1]]
    sup <- endpoints[[2]]
    hia <- c(start = inf, end = sup)

    hia
  }
)

#' @export
#' @describeIn hiatus Returns a [`TimeRange-class`] object.
#' @aliases hiatus,EventsMCMC-method
setMethod(
  f = "hiatus",
  signature = c(x = "EventsMCMC", y = "missing"),
  definition = function(x, level = 0.95) {
    ## Get phases
    n <- ncol(x)
    z <- names(x)

    ## Matrix of results
    start <- stop <- event <- matrix(nrow = n, ncol = n, dimnames = list(z, z))

    for (i in 1:n) {
      for (j in 1:n) {
        if (i != j) {
          h <- hiatus(x[, i, drop = TRUE], x[, j, drop = TRUE], level = level)
          start[i, j] <- h["start"]
          stop[i, j] <- h["end"]
        }
        event[i, j] <- paste(z[i], z[j], sep = "-")
      }
    }
    event <- as.character(event)
    start <- as.numeric(start)
    stop <- as.numeric(stop)

    ## Remove wrong results
    keep <- which(start <= stop)
    event <- event[keep]
    start <- start[keep]
    stop <- stop[keep]

    .TimeRange(
      .Id = as.character(event),
      .Start = aion::as_fixed(start),
      .End = aion::as_fixed(stop),
      hash = get_hash(x)
    )
  }
)

#' @export
#' @describeIn hiatus Returns a [`TimeRange-class`] object.
#' @aliases hiatus,PhasesMCMC,missing-method
setMethod(
  f = "hiatus",
  signature = c(x = "PhasesMCMC", y = "missing"),
  definition = function(x, level = 0.95) {
    ## Get phases
    n <- ncol(x)
    z <- names(x)

    ## Matrix of results
    start <- stop <- phase <- matrix(nrow = n, ncol = n, dimnames = list(z, z))

    for (i in 1:n) {
      for (j in 1:n) {
        if (i != j) {
          h <- hiatus(x[, i, 2, drop = TRUE], x[, j, 1, drop = TRUE], level = level)
          start[i, j] <- h["start"]
          stop[i, j] <- h["end"]
        }
        phase[i, j] <- paste(z[i], z[j], sep = "-")
      }
    }
    phase <- as.character(phase)
    start <- as.numeric(start)
    stop <- as.numeric(stop)

    ## Remove wrong results
    keep <- which(start <= stop)
    phase <- phase[keep]
    start <- start[keep]
    stop <- stop[keep]

    .TimeRange(
      .Id = as.character(phase),
      .Start = aion::as_fixed(start),
      .End = aion::as_fixed(stop),
      hash = get_hash(x)
    )
  }
)
