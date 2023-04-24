# TEMPO PLOT
#' @include AllGenerics.R
NULL

## Copy from stat::ecdf
ecdf2 <- function(x) {
  x <- sort(x, method = "radix") # Faster sorting with radix method
  n <- length(x)
  vals <- unique(x)

  rval <- stats::approxfun(
    x = vals,
    y = cumsum(tabulate(match(x, vals))) / n,
    method = "constant",
    yleft = 0,
    yright = 1,
    f = 0,
    ties = "ordered"
  )
  rval
}

#' @export
#' @rdname tempo
#' @aliases tempo,EventsMCMC-method
setMethod(
  f = "tempo",
  signature = "EventsMCMC",
  definition = function(object, level = 0.95, count = FALSE,
                        credible = TRUE, gauss = TRUE,
                        from = min(object), to = max(object),
                        resolution = NULL) {

    n_events <- ncol(object)
    n_grid <- getOption("ArchaeoPhases.grid")
    if (is.null(resolution)) resolution <- ((to - from) / (n_grid - 1))

    ## Empirical cumulative distribution
    data_seq <- seq(from = from, to = to, by = resolution)
    ecd_fun <- apply(X = object, MARGIN = 1, FUN = ecdf2, simplify = FALSE)
    ecd <- lapply(X = ecd_fun, FUN = function(f, x) f(x), x = data_seq)

    ## Build matrix
    distr <- unlist(ecd, use.names = FALSE)
    dim(distr) <- c(length(data_seq), nrow(object))

    ## Probability
    if (count) {
      distr <- distr * n_events
    }

    ## Calendar scale
    if (is_BP(object)) {
      distr <- apply(X = distr, MARGIN = 2, function(x) max(x) - x)
    }

    ## Transpose (column-wise computation is faster than row-wise)
    distr <- t(distr)

    ## Mean estimate
    moy <- colMeans(distr)

    ## Credible interval
    qu <- ga <- matrix(data = 0, nrow = 0, ncol = 2)
    if (credible) {
      qu <- apply(X = distr, MARGIN = 2, FUN = credible,
                  level = level, simplify = FALSE)
      qu <- do.call(rbind, qu)
    }
    if (gauss) {
      ## Standard deviation
      ec <- apply(X = distr, MARGIN = 2, FUN = stats::sd)

      ## Gaussian credible intervals
      alpha <- 1 - level
      z <- stats::qnorm(1 - alpha / 2)
      ga <- cbind(lower = moy - z * ec, upper = moy + z * ec)
      ga[ga <= 0] <- 0
    }

    .CumulativeEvents(
      years = data_seq,
      estimate = moy,
      credible = qu[, -3],
      gauss = ga,
      level = level,
      counts = count,
      events = n_events,
      calendar = get_calendar(object),
      hash = get_hash(object)
    )
  }
)
