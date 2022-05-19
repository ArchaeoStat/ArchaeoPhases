# AGE-DEPTH MODELING
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname proxy
#' @aliases proxy,numeric-method
setMethod(
  f = "proxy",
  signature = "numeric",
  definition = function(depth, proxy, proxy_error, time, time_error,
                        from = min(time), to = max(time),
                        grid = NULL, resolution = NULL,
                        calendar = c("BP", "CE", "b2k"),
                        density = !samples, samples = TRUE, n = 100) {
    ## Validation
    calendar <- match.arg(calendar, several.ok = FALSE)
    res <- min(diff(time))
    if (is.null(resolution)) resolution <- res
    if (resolution > res) {
      msg <- sprintf("%s must be smaller than or equal to %d.",
                     sQuote("resolution"), res)
      stop(msg, call. = FALSE)
    }

    ## Build a matrix to contain the p(t|zi) densities
    ## Rows will refer to depth
    ## Columns will refer to the time density
    t_grid <- seq(from = from, to = to, by = resolution)
    t_data <- cbind(time - time_error, time + time_error)
    t_dens <- apply(
      X = t_data,
      MARGIN = 1,
      FUN = function(x, g) {
        stats::dunif(x = g, min = x[1], max = x[2])
      },
      g = t_grid
    )
    t_dens <- t(t_dens)

    ## Build a matrix to contain the p(x|zi) densities
    ## Rows will refer to depth
    ## Columns will refer to the proxy density
    x_from <- min(proxy - 2 * proxy_error)
    x_to <- max(proxy + 2 * proxy_error)
    n_grid <- getOption("chronos.grid")
    if (is.null(grid)) grid <- ((x_to - x_from) / (n_grid - 1))
    x_grid <- seq(from = x_from, to = x_to, by = grid)
    x_data <- cbind(proxy, proxy_error)
    x_dens <- apply(
      X = x_data,
      MARGIN = 1,
      FUN = function(x, g) {
        stats::dnorm(x = g, mean = x[1], sd = x[2])
      },
      g = x_grid
    )
    x_dens <- t(x_dens)

    ## Build a matrix to contain the densities
    ## Rows will refer to time
    ## Columns will refer to the proxy measurement densities
    X <- matrix(0, nrow = ncol(t_dens), ncol = ncol(x_dens))

    ## Estimate the weighted average density function
    ## (for a given proxy measurement at a given time)
    ## Eq. 4 of Boers et al. 2017
    z <- length(depth)
    ri <- vapply(
      X = 2:(z - 1),
      FUN = function(x, y) (y[x + 1] - y[x - 1]) / 2,
      FUN.VALUE = numeric(1),
      y = depth
    )
    r <- c(depth[2] - depth[1], ri, depth[z] - depth[z - 1])

    iter <- seq_along(t_grid)
    for (j in iter) {
      X[j, ] <- colSums(r * x_dens * t_dens[, j]) / sum(r * t_dens[, j])
    }
    X[is.na(X)] <- 0 # In case of division by zero

    Y <- matrix(0, nrow = 0, ncol = 0)
    if (samples) {
      Y <- apply(
        X = X,
        MARGIN = 1,
        FUN = function(x, g, n) {
          sample(g, size = n, prob = x, replace = TRUE)
        },
        g = x_grid,
        n = n
      )
      Y <- t(Y)
    }
    if (!density) {
      X <- matrix(0, nrow = 0, ncol = 0)
    }

    .ProxyRecord(
      depth = depth,
      proxy = proxy,
      proxy_error = proxy_error,
      time = time,
      time_error = time_error,
      time_grid = t_grid,
      calendar = calendar,
      density = X,
      samples = Y
    )
  }
)

#' @export
#' @method autoplot ProxyRecord
autoplot.ProxyRecord <- function(object, ..., raw = FALSE, IQR = TRUE) {
  ## Calendar scale
  gg_x_scale <- scale_calendar(object)

  ## Get data
  age <- object@time_grid
  tmp <- summary(object, level = 0.95)

  data <- data.frame(
    Age = age,
    Proxy = if (raw) "x" else "hat(x)(t)",
    Value = if (raw) object@proxy else tmp$mean
  )
  ribbon <- data.frame(
    Age = age,
    Uncertainty = if (IQR) "IQR" else "95% CI",
    Lower = if (IQR) tmp$q1 else tmp$lower,
    Upper = if (IQR) tmp$q3 else tmp$upper
  )

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = ribbon, alpha = 0.75,
      ggplot2::aes(x = .data$Age, ymin = .data$Lower, ymax = .data$Upper,
                   fill = .data$Uncertainty)
    ) +
    ggplot2::geom_path(
      data = data,
      ggplot2::aes(x = .data$Age, y = .data$Value, colour = .data$Proxy)
    ) +
    gg_x_scale
}

#' @export
#' @rdname proxy
#' @aliases autoplot,ProxyRecord-method
setMethod("autoplot", "ProxyRecord", autoplot.ProxyRecord)

#' @export
#' @method plot ProxyRecord
plot.ProxyRecord <- function(x, raw = FALSE, IQR = TRUE, ...) {
  gg <- autoplot(object = x, ..., raw = raw, IQR = IQR) +
    ggplot2::scale_colour_manual(
      values = "steelblue",
      labels = function(text) parse(text = text)
    ) +
    ggplot2::scale_fill_manual(
      values = "grey"
    ) +
    ggplot2::theme_bw()
  print(gg)
  invisible(x)
}

#' @export
#' @rdname proxy
#' @aliases plot,ProxyRecord,missing-method
setMethod("plot", c(x = "ProxyRecord", y = "missing"), plot.ProxyRecord)
