# PLOT
#' @include AllClasses.R AllGenerics.R
NULL

# MCMC =========================================================================
#' @export
#' @method autoplot MCMC
autoplot.MCMC <- function(object, ..., density = TRUE, n = 512,
                          interval = c("ci", "hpdi"), level = 0.95,
                          decreasing = TRUE) {
  ## Validation
  interval <- match.arg(interval, several.ok = FALSE)

  ## Calendar scale
  gg_x_scale <- scale_calendar(get_calendar(object))

  ## Compute interval
  fun <- switch (
    interval,
    ci = interval_credible,
    hpdi = interval_hpd,
    stop(sprintf("There is no such interval: %s.", interval), call. = FALSE)
  )
  inter <- fun(object, level = level)

  ## Bind results in case of multiple intervals
  rank_i <- ifelse(is_BP(object), 2, 1) # Ranking column
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
      X = object,
      MARGIN = 2,
      FUN = function(x, n, ...) {
        tmp <- stats::density(x = x, n = n, ...)
        data.frame(x = tmp$x, y = scale_01(tmp$y))
      },
      n = n, ...
    )
    ## Long data frame for ggplot2
    dens <- do.call(rbind, dens)
    dens$Event <- rep(colnames(object), each = n)
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

#' @export
#' @rdname plot
#' @aliases autoplot,MCMC-method
setMethod("autoplot", "MCMC", autoplot.MCMC)

#' @export
#' @method plot MCMC
plot.MCMC <- function(x, density = TRUE, n = 512, interval = c("ci", "hpdi"),
                      level = 0.95, decreasing = TRUE, ...) {
  gg <- autoplot(object = x, ..., density = density, n = n,
                 interval = interval, level = level,
                 decreasing = decreasing) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::theme_bw()
  print(gg)
  invisible(x)
}

#' @export
#' @describeIn plot Plots of credible intervals or HPD regions of a series of
#' events.
#' @aliases plot,MCMC,missing-method
setMethod("plot", c(x = "MCMC", y = "missing"), plot.MCMC)

# PhasesMCMC ===================================================================
#' @export
#' @method autoplot PhasesMCMC
autoplot.PhasesMCMC <- function(object, ..., level = 0.95, n = 512,
                                decreasing = TRUE,
                                succession = is_ordered(object), facet = TRUE) {
  ## Calendar scale
  gg_x_scale <- scale_calendar(get_calendar(object))

  if (succession) {
    gg_phases <- plot_succession(object, level = level,
                                 decreasing = decreasing)
  } else {
    gg_phases <- plot_density(object, level = level, decreasing = decreasing,
                              n = n, ..., facet = facet)
  }

  ## ggplot2
  ggplot2::ggplot() +
    gg_phases +
    gg_x_scale
}

#' @export
#' @rdname plot
#' @aliases autoplot,PhasesMCMC-method
setMethod("autoplot", "PhasesMCMC", autoplot.PhasesMCMC)

#' @export
#' @method plot PhasesMCMC
plot.PhasesMCMC <- function(x, level = 0.95, n = 512, decreasing = TRUE,
                            succession = is_ordered(x), facet = TRUE, ...) {
  gg <- autoplot(object = x, ..., level = level, n = n,
                 decreasing = decreasing, succession = succession,
                 facet = facet) +
    ggplot2::theme_bw()
  print(gg)
  invisible(x)
}

#' @export
#' @describeIn plot Plots the characteristics of a group of events (phase).
#' @aliases plot,PhasesMCMC,missing-method
setMethod("plot", c(x = "PhasesMCMC", y = "missing"), plot.PhasesMCMC)

# Helpers ======================================================================
#' @param x A [`PhasesMCMC`] object.
#' @return A \pkg{ggplot2} layer.
#' @noRd
plot_succession <- function(x, level = 0.95, decreasing = TRUE,
                            size = 2, fill = "grey50", alpha = 0.5) {
  ## Time range
  duree <- boundaries(x, level = level)
  ord <- rank(duree$start)
  duree$rank <- if (decreasing) -ord else ord
  pha <- get_order(x)
  duree$Phase <- factor(pha, levels = pha, ordered = TRUE)

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
  ## Get phases
  pha <- as.list(x)

  ## Density
  dens <- lapply(
    X = pha,
    FUN = function(x, n, ...) {
      a <- stats::density(x[, , 1, drop = TRUE], n = n, ...)
      b <- stats::density(x[, , 2, drop = TRUE], n = n, ...)
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
  duree_phase <- rownames(duree)
  duree$Phase <- factor(duree_phase, levels = duree_phase,
                        ordered = is_ordered(x))
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
  dens_phase <- names(pha)
  dens$Phase <- factor(rep(dens_phase, each = 2 * n), levels = dens_phase,
                       ordered = is_ordered(x))
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
