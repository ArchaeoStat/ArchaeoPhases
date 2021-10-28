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
  definition = function(object, level = 0.95, count = TRUE, gauss = FALSE,
                        from = min(object), to = max(object),
                        step = (to - from) / (250 - 1)) {
    n_events <- ncol(object)

    ## Empirical cumulative distribution
    data_seq <- seq(from = from, to = to, by = step)
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
    if (gauss == TRUE) {
      ## Standard deviation
      ec <- apply(X = distr, MARGIN = 2, FUN = stats::sd)

      ## Gaussian credible intervals
      alpha <- 1 - level
      z <- stats::qnorm(1 - alpha / 2)
      qu <- cbind(moy - z * ec, moy + z * ec)
    }
    else {
      ## Credible intervals
      qu <- apply(X = distr, MARGIN = 2, FUN = credible, level = level,
                  simplify = FALSE)
      qu <- do.call(rbind, qu)
    }

    .CumulativeEvents(
      year = data_seq,
      estimate = moy,
      lower = qu[, 1],
      upper = qu[, 2],
      gauss = gauss,
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
autoplot.CumulativeEvents <- function(object, ..., ci = TRUE) {
  ## Calendar scale
  gg_x_scale <- scale_calendar(get_calendar(object))

  ## Get data
  data <- as.data.frame(object)
  if (ci) {
    tempo_ci <- data.frame(
      year = c(data$year, data$year, rev(data$year)),
      ci = c(data$estimate, data$lower, rev(data$upper)),
      Legend = c(rep("Bayes estimate", nrow(data)),
                 rep("Credible interval", nrow(data) * 2))
    )
    tempo_aes <- ggplot2::aes(x = .data$year, y = .data$ci,
                              colour = .data$Legend, linetype = .data$Legend)
  } else {
    tempo_ci <- data
    tempo_aes <- ggplot2::aes(x = .data$year, y = .data$estimate)
  }

  ggplot2::ggplot(data = tempo_ci) +
    tempo_aes +
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
plot.CumulativeEvents <- function(x, ci = TRUE, ...) {
  gg <- autoplot(object = x, ..., ci = ci) +
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
