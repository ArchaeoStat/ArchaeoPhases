# PLOT
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @describeIn plot Plots of credible intervals or HPD regions of a series of
#' events.
#' @aliases plot,MCMC,missing-method
setMethod(
  f = "plot",
  signature = c(x = "MCMC", y = "missing"),
  definition = function(x, calendar = c("BCAD", "BP"), density = TRUE, n = 512,
                        interval = c("ci", "hpdi"), level = 0.95,
                        decreasing = TRUE, elapsed = FALSE, origin = 1, ...) {
    ## Validation
    calendar <- match.arg(calendar, several.ok = FALSE)
    interval <- match.arg(interval, several.ok = FALSE)

    ## Calendar scale
    if (elapsed) {
      if (is.null(origin))
        stop("Elapsed origin must be specified.", call. = FALSE)
      x <- x - x[, origin]
      gg_x_scale <- ggplot2::scale_x_continuous(name = "Elapsed years")
    } else {
      if (calendar == "BCAD") {
        if (get_calendar(x) == "BP") x <- BP_to_BCAD(x)
        gg_x_scale <- ggplot2::scale_x_continuous(name = "Years BC/AD")
      }
      if (calendar == "BP") {
        if (get_calendar(x) == "BCAD") x <- BCAD_to_BP(x)
        decreasing <- !decreasing
        gg_x_scale <- ggplot2::scale_x_reverse(name = "Years cal BP")
      }
    }

    ## Compute interval
    fun <- switch (
      interval,
      ci = interval_credible,
      hpdi = interval_hpd,
      stop(sprintf("There is no such interval: %s.", interval), call. = FALSE)
    )
    inter <- fun(x, level = level)

    ## Bind results in case of multiple intervals
    rank_i <- ifelse(calendar == "BP", 2, 1) # Ranking column
    if (interval == "hpdi") {
      k <- vapply(X = inter, FUN = nrow, FUN.VALUE = integer(1))
      low <- vapply(X = inter, FUN = function(x, i) min(x[, i]),
                    FUN.VALUE = numeric(1), i = rank_i)
      ord <- rank(low)
      ord <- rep(ord, times = k)
      id <- rep(names(inter), times = k)

      inter <- do.call(rbind, inter)
    } else {
      ord <- rank(inter[, rank_i])
      id <- rownames(inter)
    }

    ## Reorder
    inter <- as.data.frame(inter)
    inter$rank <- if (decreasing) -ord else ord
    inter$Event <- id
    inter$Interval <- toupper(interval)

    ## Density
    gg_dens <- NULL
    if (density) {
      dens <- apply(
        X = x,
        MARGIN = 2,
        FUN = function(x, n, ...) {
          tmp <- stats::density(x = x, n = n, ...)
          data.frame(x = tmp$x, y = scale_01(tmp$y))
        },
        n = n, ...
      )
      ## Long data frame for ggplot2
      dens <- do.call(rbind, dens)
      dens$Event <- rep(colnames(x), each = n)
      dens <- merge(dens, inter[, c("Event", "rank")], by = "Event")

      aes_dens <- ggplot2::aes(
        x = .data$x,
        ymin = .data$rank,
        ymax = .data$y + .data$rank,
        fill = .data$Event
      )
      gg_dens <- ggplot2::geom_ribbon(mapping = aes_dens, data = dens,
                                      alpha = 0.5)
    }

    ## ggplot2
    ggplot2::ggplot() +
      gg_dens +
      ggplot2::geom_segment(
        mapping = ggplot2::aes(
          x = .data$lower,
          y = .data$rank + 0.1,
          xend = .data$upper,
          yend = .data$rank + 0.1,
          color = .data$Event,
          linetype = .data$Interval
        ),
        data = inter,
        size = 2
      ) +
      gg_x_scale +
      ggplot2::scale_y_discrete(name = "Dates")
  }
)

#' @export
#' @describeIn plot Plots the characteristics of a group of events (phase).
#' @aliases plot,PhasesMCMC,missing-method
setMethod(
  f = "plot",
  signature = c(x = "PhasesMCMC", y = "missing"),
  definition = function(x, calendar = c("BCAD", "BP"), level = 0.95, n = 512,
                        decreasing = TRUE, elapsed = FALSE, origin = 1,
                        succession = is_ordered(x), facet = TRUE, ...) {
    ## Validation
    calendar <- match.arg(calendar, several.ok = FALSE)

    ## Calendar scale
    if (elapsed) {
      if (is.null(origin))
        stop("Elapsed origin must be specified.", call. = FALSE)
      x <- x - x[, origin]
      gg_x_scale <- ggplot2::scale_x_continuous(name = "Elapsed years")
    } else {
      if (calendar == "BCAD") {
        if (get_calendar(x) == "BP") x <- BP_to_BCAD(x)
        gg_x_scale <- ggplot2::scale_x_continuous(name = "Years BC/AD")
      }
      if (calendar == "BP") {
        if (get_calendar(x) == "BCAD") x <- BCAD_to_BP(x)
        decreasing <- !decreasing
        gg_x_scale <- ggplot2::scale_x_reverse(name = "Years cal BP")
      }
    }

    if (succession) {
      gg_phases <- plot_succession(x, level = level, decreasing = decreasing)
    } else {
      gg_phases <- plot_density(x, level = level, decreasing = decreasing,
                                n = n, ..., facet = facet)
    }

    ## ggplot2
    ggplot2::ggplot() +
      gg_phases +
      gg_x_scale
  }
)

#' @param x A [`PhasesMCMC`] object.
#' @return A \pkg{ggplot2} layer.
#' @noRd
plot_succession <- function(x, level = 0.95, decreasing = TRUE,
                            size = 2, fill = "grey50", alpha = 0.5) {
  ## Time range
  duree <- boundaries(x, level = level)
  ord <- rank(duree$start)
  duree$rank <- if (decreasing) -ord else ord
  duree$Phase <- get_order(x)

  gg_trans <- gg_hiatus <- NULL
  if (is_ordered(x)) {
    ## Transition
    trans <- transition(x, level = level)
    trans <- data.frame(
      xmin = trans$start,
      xmax = trans$end,
      ymin = min(duree$rank) - 0.5,
      ymax = max(duree$rank) + 0.5,
      labels = rownames(trans)
    )

    ## Hiatus
    hia <- hiatus(x, level = level)
    hia <- data.frame(
      xmin = hia$start,
      xmax = hia$end,
      ymin = min(duree$rank) - 0.5,
      ymax = max(duree$rank) + 0.5,
      labels = rownames(hia)
    )
    hia <- stats::na.omit(hia)

    ## Layers
    aes_rect <- ggplot2::aes(
      xmin = .data$xmin,
      xmax = .data$xmax,
      ymin = .data$ymin,
      ymax = .data$ymax
    )
    if (nrow(trans) > 0) {
      gg_trans <- ggplot2::geom_rect(
        mapping = aes_rect,
        data = trans,
        fill = fill,
        alpha = alpha
      )
    }
    if (nrow(hia) > 0) {
      gg_hiatus <- ggplot2::geom_rect(
        mapping = aes_rect,
        data = hia,
        fill = fill,
        alpha = alpha + 0.25
      )
    }
  }

  ## Layer
  aes_range <- ggplot2::aes(
    x = .data$start,
    y = .data$rank,
    xend = .data$end,
    yend = .data$rank,
    color = .data$Phase
  )
  gg_range <- ggplot2::geom_segment(
    mapping = aes_range,
    data = duree,
    size = size
  )
  gg_y_scale <- ggplot2::scale_y_continuous(
    name = "Phases",
    breaks = duree$rank,
    labels = duree$Phase
  )
  list(gg_trans, gg_hiatus, gg_range, gg_y_scale)
}

#' @param x A [`PhasesMCMC`] object.
#' @return A \pkg{ggplot2} layer.
#' @noRd
plot_density <- function(x, level = 0.95, decreasing = TRUE, n = 512, ...,
                         facet = TRUE, color = "black", size = 2, alpha = 0.5) {
  ## Density
  phases <- as.list(x)
  dens <- lapply(
    X = phases,
    FUN = function(x, n, ...) {
      a <- stats::density(x[, 1], n = n, ...)
      b <- stats::density(x[, 2], n = n, ...)
      data.frame(
        x = c(a$x, b$x),
        y = c(a$y, b$y),
        z = rep(c("Begin", "End"), each = n)
      )
    },
    n = n, ...
  )

  ## Time range
  duree <- boundaries(x, level = level)
  ord <- rank(duree$start)
  duree$rank <- if (decreasing) -ord else ord
  duree$Phase <- get_order(x)
  # duree$Range <- paste0(round(level * 100, digits = 0), "%")

  ## Adjust y position
  y_max <- vapply(X = dens, FUN = function(x) max(x$y), FUN.VALUE = numeric(1))
  if (facet) {
    duree$y <- y_max * 1.1
  } else {
    y_duree <- max(y_max) * (1 + duree$rank / 10)
    duree$y <- if (decreasing) y_duree + diff(range(y_max)) else y_duree
  }

  ## Bind densities
  dens <- do.call(rbind, dens)
  dens$Phase <- rep(get_order(x), each = 2 * n)
  dens$Boundary <- factor(dens$z, levels = c("Begin", "End"), ordered = TRUE)

  ## Layer
  gg_facet <- NULL
  if (facet) {
    gg_facet <- ggplot2::facet_grid(
      rows = ggplot2::vars(.data$Phase),
      scales = "free_y"
    )
  }
  aes_dens <- ggplot2::aes(
    x = .data$x,
    ymin = 0,
    ymax = .data$y,
    fill = .data$Phase,
    linetype = .data$Boundary
  )
  gg_dens <- ggplot2::geom_ribbon(
    mapping = aes_dens,
    data = dens,
    color = color,
    alpha = alpha
  )
  aes_range <- ggplot2::aes(
    x = .data$start,
    y = .data$y,
    xend = .data$end,
    yend = .data$y,
    color = .data$Phase
  )
  gg_range <- ggplot2::geom_segment(
    mapping = aes_range,
    data = duree,
    size = size
  )
  gg_y_scale <- ggplot2::scale_y_continuous(name = "Density")
  list(gg_dens, gg_range, gg_facet, gg_y_scale)
}
