# TEMPO PLOT
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname tempo
#' @aliases tempo,MCMC-method
setMethod(
  f = "tempo",
  signature = "MCMC",
  definition = function(object, level = 0.95, count = TRUE, gauss = FALSE,
                        elapsed = FALSE, origin = 1, time = range(object),
                        n = 50 * ncol(object),
                        progress = getOption("ArchaeoPhases.progress")) {
    ## Elapse
    if (elapsed) {
      if (is.null(origin))
        stop("Elapsed origin must be specified.", call. = FALSE)
      object <- object - object[, origin]
      time <- range(object) # Override user input
    }

    n_iter <- nrow(object)
    n_events <- ncol(object)

    ## Empirical cumulative distribution
    data_seq <- seq(from = time[[1L]], to = time[[2L]], length.out = n)
    distr <- matrix(data = NA_real_, nrow = n, ncol = n_iter)

    iter <- seq_len(n_iter)
    progress_bar <- interactive() && progress # Display a progress bar?
    if (progress_bar) pb <- utils::txtProgressBar(max = n_iter, style = 3)
    for (i in iter) {
      g <- stats::ecdf(object[i, ]) # Returns a function
      distr[, i] <- g(data_seq)
      if (progress_bar) utils::setTxtProgressBar(pb, i)
    }
    if (progress_bar) close(pb)

    ## Probability
    if (count) distr <- distr * n_events

    ## Mean estimate
    moy <- apply(X = distr, MARGIN = 1, FUN = mean)

    ## Credible interval
    if (gauss == TRUE) {
      ## Standard deviation
      ec <- apply(X = distr, MARGIN = 1, FUN = stats::sd)

      ## Gaussian credible intervals
      alpha <- 1 - level
      qu <- cbind(moy - stats::qnorm(1 - alpha / 2) * ec,
                  moy + stats::qnorm(1 - alpha / 2) * ec)
    }
    else {
      ## Credible intervals
      qu <- apply(X = distr, MARGIN = 1, FUN = interval_credible, level = level)
      qu <- t(qu)
    }
    colnames(qu) <- c("lower", "upper")

    .CumulativeEvents(
      year = data_seq,
      estimate = moy,
      lower = qu[, 1],
      upper = qu[, 2],
      gauss = gauss,
      level = level,
      counts = count,
      events = n_events,
      calendar = ifelse(elapsed, "elapsed", get_calendar(object)),
      hash = get_hash(object)
    )
  }
)

#' @export
#' @rdname tempo
#' @aliases plot,CumulativeEvents,missing-method
setMethod(
  f = "plot",
  signature = c(x = "CumulativeEvents", y = "missing"),
  definition = function(x, calendar = c("BCAD", "BP")) {
    ## Validation
    calendar <- match.arg(calendar, several.ok = FALSE)
    data <- as.data.frame(x)

    ## Calendar scale
    if (get_calendar(x) == "elapsed") {
      gg_x_scale <- ggplot2::scale_x_continuous(name = "Elapsed years")
    } else {
      if (calendar == "BCAD") {
        if (get_calendar(x) == "BP") data$year <- BP_to_BCAD(data$year)
        gg_x_scale <- ggplot2::scale_x_continuous(name = "Years BC/AD")
      }
      if (calendar == "BP") {
        if (get_calendar(x) == "BCAD") data$year <- BCAD_to_BP(data$year)
        gg_x_scale <- ggplot2::scale_x_reverse(name = "Years cal BP")
      }
    }

    tempo_ci <- data.frame(
      year = c(data$year, data$year, rev(data$year)),
      ci = c(data$estimate, data$lower, rev(data$upper)),
      Legend = c(rep("Bayes estimate", nrow(data)),
                 rep("Credible interval", nrow(data) * 2))
    )

    ggplot2::ggplot(data = tempo_ci) +
      ggplot2::aes(x = .data$year, y = .data$ci, color = .data$Legend,
                   linetype = .data$Legend) +
      ggplot2::geom_path() +
      gg_x_scale +
      ggplot2::scale_y_continuous(name = "Cumulative events")
  }
)

#' @export
#' @rdname tempo
#' @aliases multiplot,CumulativeEvents-method
setMethod(
  f = "multiplot",
  signature = "CumulativeEvents",
  definition = function(..., calendar = c("BCAD", "BP")) {
    ## Validation
    calendar <- match.arg(calendar, several.ok = FALSE)

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
    cal <- unique(vapply(X = dots, FUN = get_calendar, FUN.VALUE = character(1)))
    if (length(cal) != 1) {
      stop("All object must have the same calendar scale.", call. = FALSE)
    }
    if (cal == "elapsed") {
      gg_x_scale <- ggplot2::scale_x_continuous(name = "Elapsed years")
    } else {
      if (calendar == "BCAD") {
        if (cal == "BP") tmp$year <- BP_to_BCAD(tmp$year)
        gg_x_scale <- ggplot2::scale_x_continuous(name = "Years BC/AD")
      }
      if (calendar == "BP") {
        if (cal == "BCAD") tmp$year <- BCAD_to_BP(tmp$year)
        gg_x_scale <- ggplot2::scale_x_reverse(name = "Years cal BP")
      }
    }

    ggplot2::ggplot(data = tmp) +
      ggplot2::aes(x = .data$year, y = .data$estimate, color = .data$Legend) +
      ggplot2::geom_path() +
      gg_x_scale +
      ggplot2::scale_y_continuous(name = "Cumulative events")
  }
)
