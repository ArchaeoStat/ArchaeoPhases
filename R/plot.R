# PLOT
#' @include AllClasses.R AllGenerics.R
NULL

# MCMC =========================================================================
#' @export
#' @method autoplot MCMC
autoplot.MCMC <- function(object, ..., select = NULL, groups = NULL,
                          density = TRUE, interval = NULL, level = 0.95,
                          decreasing = TRUE) {
  ## Calendar scale
  gg_x_scale <- scale_calendar(object)

  ## Select data
  if (!is.null(select)) {
    object <- object[, select, drop = FALSE]
  }

  ## Group data
  grp <- data.frame(id = character(0), Group = character(0))
  aes_grp <- NULL
  if (!is.null(groups)) {
    if (length(groups) != ncol(object)) {
      msg <- "%s must be of length %d, not %d."
      stop(sprintf(msg, sQuote("groups"), ncol(object), length(groups)),
           call. = FALSE)
    }
    grp <- data.frame(id = names(object), Group = as.character(groups))
    aes_grp <- ggplot2::aes(color = .data$Group, fill = .data$Group)
  }

  ## Reorder data
  decreasing <- ifelse(is_BP(object), !decreasing, decreasing)
  data <- reorder(object, decreasing = decreasing)
  ids <- names(data)

  ## Compute interval
  gg_inter <- NULL
  if (!is.null(interval)) {
    interval <- match.arg(interval, choices = c("credible", "hpdi"))
    fun <- switch (
      interval,
      credible = credible,
      hpdi = hpdi,
      stop(sprintf("There is no such interval: %s.", interval), call. = FALSE)
    )
    inter <- fun(data, level = level)
    inter <- bind_intervals(inter)
    inter$id <- factor(inter$id, levels = unique(inter$id))

    ## Add group (if any)
    inter <- merge(inter, grp, by = "id", all.x = TRUE, all.y = FALSE,
                   sort = FALSE)

    aes_inter <- ggplot2::aes(x = .data$lower, y = .data$id,
                              xend = .data$upper, yend = .data$id)
    gg_inter <- ggplot2::geom_segment(mapping = aes_inter, data = inter,
                                      size = 2)
  }

  ## Density
  gg_dens <- NULL
  if (density) {
    if (ncol(data) > 1) {
      ## Build long table for ggplot2
      col_ids <- rep(ids, each = nrow(data))
      dens <- data.frame(
        row = as.vector(row(data, as.factor = FALSE)),
        column = factor(col_ids, levels = unique(col_ids)),
        # column = as.vector(col(data, as.factor = TRUE)),
        value = as.vector(data),
        stringsAsFactors = FALSE
      )
      ## Keep original ordering
      dens$column <- as.character(dens$column)
      dens$id <- factor(dens$column, levels = unique(dens$column))

      aes_dens <- ggplot2::aes(
        x = .data$value,
        y = .data$id,
        group = .data$id
      )
      gg_dens <- ggridges::geom_density_ridges(
        mapping = aes_dens,
        data = dens,
        panel_scaling = TRUE,
        rel_min_height = 0.01,
        scale = 1.75,
        alpha = 0.5
      )
    } else {
      data <- as.vector(data)
      ## Compute density
      d <- stats::density(data, n = getOption("chronos.grid"))
      dens <- data.frame(
        x = d$x,
        y = d$y
      )

      aes_dens <- ggplot2::aes(
        x = .data$x,
        y = .data$y
      )
      gg_dens <- ggplot2::geom_path(
        mapping = aes_dens,
        data = dens
      )

      if (!is.null(interval)) {
        ## Is credible?
        cred <- is_credible(d$x, inter[, c("lower", "upper")])
        int <- dens[which(cred), , drop = FALSE]
        int$Interval <- paste0(round(level * 100), "%")

        aes_inter <- ggplot2::aes(
          x = .data$x,
          y = .data$y,
          fill = .data$Interval
        )
        gg_inter <- ggplot2::geom_area(
          mapping = aes_inter,
          data = int
        )
      }
    }
  }

  ## ggplot2
  ggplot2::ggplot() +
    aes_grp +
    gg_dens +
    gg_inter +
    gg_x_scale
}

#' @export
#' @rdname plot
#' @aliases autoplot,MCMC-method
setMethod("autoplot", "MCMC", autoplot.MCMC)

#' @export
#' @method plot MCMC
plot.MCMC <- function(x, select = NULL, groups = NULL, density = TRUE,
                      interval = NULL, level = 0.95, decreasing = TRUE, ...) {
  gg <- autoplot(object = x, ..., select = select, groups = groups,
                 density = density, interval = interval, level = level,
                 decreasing = decreasing) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.y = ggplot2::element_blank())
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
autoplot.PhasesMCMC <- function(object, ..., level = 0.95, decreasing = TRUE,
                                range = NULL, facet = TRUE) {
  ## Calendar scale
  gg_x_scale <- scale_calendar(object)

  if (!is.null(range)) {
    gg_phases <- plot_succession(object, level = level, decreasing = decreasing,
                                 range = range, facet = facet)
  } else {
    gg_phases <- plot_density(object, level = level, decreasing = decreasing,
                              ..., facet = facet)
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
plot.PhasesMCMC <- function(x, level = 0.95, decreasing = TRUE,
                            range = NULL, facet = TRUE, ...) {
  gg_fill <- NULL
  if (!is.null(range)) {
    gg_fill <- ggplot2::scale_fill_manual(values = "grey50")
  }
  gg <- autoplot(object = x, ..., level = level, decreasing = decreasing,
                 range = range, facet = facet) +
    gg_fill +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.y = ggplot2::element_blank())
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
                            range = c("transition", "hiatus"), facet = TRUE,
                            size = 2, alpha = 0.5) {
  ## Validation
  range <- match.arg(range, several.ok = FALSE)

  ## Time range
  duree <- boundaries(x, level = level)
  ord <- rank(duree$lower)
  duree$rank <- if (decreasing) -ord else ord
  pha <- rownames(duree)
  duree$Phase <- factor(pha, levels = unique(pha))

  ## Succession
  fun <- switch(
    range,
    hiatus = hiatus,
    transition = transition
  )
  hia <- as.data.frame(fun(x, level = level))
  hia <- data.frame(
    xmin = hia$lower,
    xmax = hia$upper,
    ymin = min(duree$rank) - 0.5,
    ymax = max(duree$rank) + 0.5,
    labels = rownames(hia),
    Range = range
  )
  hia <- stats::na.omit(hia)

  ## Layers
  gg_hiatus <- NULL
  gg_facet <- NULL
  if (nrow(hia) > 0) {
    ## Reorder
    ord <- order(hia$xmin, decreasing = !decreasing)
    hia <- hia[ord, ]
    hia$labels <- factor(hia$labels, levels = hia$labels)

    aes_rect <- ggplot2::aes(
      xmin = .data$xmin,
      xmax = .data$xmax,
      ymin = -Inf,
      ymax = Inf,
      fill = .data$Range
    )
    gg_hiatus <- ggplot2::geom_rect(
      mapping = aes_rect,
      data = hia,
      alpha = alpha
    )
    if (facet) {
      gg_facet <- ggplot2::facet_grid(
        rows = ggplot2::vars(.data$labels),
        scales = "free_y"
      )
    }
  }

  ## Layer
  aes_range <- ggplot2::aes(
    x = .data$lower,
    y = .data$rank,
    xend = .data$upper,
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
  list(gg_hiatus, gg_facet, gg_range, gg_y_scale)
}

#' @param x A [`PhasesMCMC`] object.
#' @return A \pkg{ggplot2} layer.
#' @noRd
plot_density <- function(x, level = 0.95, decreasing = TRUE, ..., facet = TRUE,
                         color = "black", size = 2, alpha = 0.5) {
  ## Get phases
  pha <- as.list(x)

  ## Density
  n <- getOption("chronos.grid")
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
  ord <- rank(duree$lower)
  duree$rank <- if (decreasing) -ord else ord
  duree_phase <- rownames(duree)
  duree$Phase <- factor(duree_phase, levels = unique(duree_phase))
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
  dens$Phase <- factor(rep(dens_phase, each = 2 * n), levels = unique(dens_phase))
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
    x = .data$lower,
    y = .data$y,
    xend = .data$upper,
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
