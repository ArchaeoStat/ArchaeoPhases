# TEMPO PLOT
#' @include AllClasses.R AllGenerics.R
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
#' @aliases tempo,MCMC-method
setMethod(
  f = "tempo",
  signature = "MCMC",
  definition = function(object, level = 0.95, count = FALSE,
                        credible = TRUE, gauss = TRUE,
                        from = min(object), to = max(object)) {
    n_grid <- getOption("ArchaeoPhases.grid")
    n_events <- ncol(object)

    ## Empirical cumulative distribution
    data_seq <- seq(from = from, to = to, length.out = n_grid)
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
      year = data_seq,
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

#' @export
#' @method autoplot CumulativeEvents
autoplot.CumulativeEvents <- function(object, ..., credible = TRUE,
                                      gauss = TRUE) {
  ## Calendar scale
  gg_x_scale <- scale_calendar(get_calendar(object))

  ## Intervals
  credible <- credible & nrow(object@credible) > 0
  gauss <- gauss & nrow(object@gauss) > 0

  ## Get data
  tmp <- data <- as.data.frame(object)
  tmp$ci <- tmp$estimate
  tmp$Legend <- rep("Bayes estimate", nrow(data))
  aes_tmp <- ggplot2::aes(x = .data$year, y = .data$estimate)

  if (credible) {
    tmp <- data.frame(
      year = c(tmp$year, data$year, rev(data$year)),
      ci = c(tmp$ci, data$credible_lower, rev(data$credible_upper)),
      Legend = c(tmp$Legend, rep("Credible interval", nrow(data) * 2))
    )
  }
  if (gauss) {
    tmp <- data.frame(
      year = c(tmp$year, data$year, rev(data$year)),
      ci = c(tmp$ci, data$gauss_lower, rev(data$gauss_upper)),
      Legend = c(tmp$Legend, rep("Gauss interval", nrow(data) * 2))
    )
  }
  if (credible | gauss) {
    aes_tmp <- ggplot2::aes(
      x = .data$year,
      y = .data$ci,
      colour = .data$Legend,
      linetype = .data$Legend
    )
  }

  ggplot2::ggplot(data = tmp) +
    aes_tmp +
    ggplot2::geom_path() +
    gg_x_scale +
    ggplot2::scale_y_continuous(name = "Cumulative events")
}

#' @export
#' @rdname tempo
#' @aliases autoplot,CumulativeEvents-method
setMethod("autoplot", "CumulativeEvents", autoplot.CumulativeEvents)

#' @export
#' @method plot CumulativeEvents
plot.CumulativeEvents <- function(x, credible = TRUE, gauss = TRUE, ...) {
  gg <- autoplot(object = x, ..., credible = credible, gauss = gauss) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  print(gg)
  invisible(x)
}

#' @export
#' @rdname tempo
#' @aliases plot,CumulativeEvents,missing-method
setMethod("plot", c(x = "CumulativeEvents", y = "missing"), plot.CumulativeEvents)

#' @export
#' @rdname tempo
#' @aliases multiplot,CumulativeEvents-method
setMethod(
  f = "multiplot",
  signature = "CumulativeEvents",
  definition = function(...) {
    ## Get names
    subst <- substitute(list(...))[-1]
    arg_names <- vapply(X = subst, FUN = deparse, FUN.VALUE = character(1))

    ## Get data
    dots <- list(...)
    tmp <- lapply(X = dots, FUN = as.data.frame)
    n <- vapply(X = tmp, FUN = nrow, FUN.VALUE = integer(1))

    ## Bind data
    tmp <- do.call(rbind, tmp)
    tmp$Legend <- rep(arg_names, n)

    ## Calendar scale
    scales <- vapply(X = dots, FUN = get_calendar, FUN.VALUE = character(1))
    cal <- unique(scales)
    if (length(cal) != 1) {
      stop("All object must have the same calendar scale.", call. = FALSE)
    }
    gg_x_scale <- scale_calendar(cal)

    ggplot2::ggplot(data = tmp) +
      ggplot2::aes(x = .data$year, y = .data$estimate, color = .data$Legend) +
      ggplot2::geom_path() +
      gg_x_scale +
      ggplot2::scale_y_continuous(name = "Cumulative events")
  }
)
