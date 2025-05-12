# PLOT
#' @include AllGenerics.R
NULL

# MCMC =========================================================================
#' @export
#' @method plot MCMC
plot.MCMC <- function(x, calendar = get_calendar(),
                      density = TRUE, interval = NULL, level = 0.95,
                      sort = TRUE, decreasing = TRUE,
                      main = NULL, sub = NULL,
                      ann = graphics::par("ann"), axes = TRUE,
                      frame.plot = FALSE,
                      panel.first = NULL, panel.last = NULL,
                      col.density = "grey", col.interval = "#77AADD", ...) {
  ## Get data
  n_events <- NCOL(x)

  ## Graphical parameters
  dots <- list(...)
  lty <- get_par(dots, "lty")
  lwd <- get_par(dots, "lwd")
  tcl <- get_par(dots, "tcl")
  if (length(col.density) != n_events)
    col.density <- rep(col.density, length.out = n_events)
  if (length(col.interval) != n_events)
    col.interval <- rep(col.interval, length.out = n_events)
  fill.density <- grDevices::adjustcolor(col.density, alpha.f = 0.5)
  fill.interval <- grDevices::adjustcolor(col.interval, alpha.f = 0.5)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- xlim(x, calendar = calendar)
  ylim <- c(1, n_events + 1.5)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Reorder data
  k <- seq_len(n_events)
  if (sort) {
    k <- sort.list(x, decreasing = decreasing)
    x <- x[, k, drop = FALSE]
    col.density <- col.density[k]
    fill.density <- fill.density[k]
    col.interval <- col.interval[k]
    fill.interval <- fill.interval[k]
  }

  ## Compute interval
  interval_draw <- FALSE
  if (!density & is.null(interval)) {
    interval <- "credible"
  }
  if (!is.null(interval) & !is.null(level)) {
    interval <- match.arg(interval, choices = c("credible", "hdr"))
    fun <- switch(interval, credible = interval_credible, hdr = interval_hdr)
    int <- fun(x, level = level, calendar = calendar)
    interval_draw <- TRUE
  }

  ## Plot
  mcmc <- rev(seq_len(n_events))
  if (density) {
    for (i in mcmc) {
      d <- stats::density(
        x = x[, i, drop = TRUE],
        n = getOption("ArchaeoPhases.grid"),
        ...
      )

      years <- aion::as_year(d$x, calendar = calendar)
      dens <- (d$y - min(d$y)) / max(d$y - min(d$y)) * 1.5
      d0 <- which(dens > 0) # Keep only density > 0
      lb <- if (min(d0) > 1) min(d0) - 1 else min(d0)
      ub <- if (max(d0) < length(years)) max(d0) + 1 else max(d0)
      xi <- c(years[lb], years[d0], years[ub])
      yi <- c(0, dens[d0], 0) + i

      graphics::polygon(x = xi, y = yi,
                        border = NA, col = fill.density[i])

      if (interval_draw) {
        h <- int[[i]]
        for (j in seq_len(nrow(h))) {
          is_in_h <- xi >= min(h[j, c("start", "end")]) &
            xi <= max(h[j, c("start", "end")])

          graphics::polygon(
            x = c(utils::head(xi[is_in_h], 1), xi[is_in_h],
                  utils::tail(xi[is_in_h], 1)),
            y = c(i, yi[is_in_h], i),
            border = NA, col = fill.interval[i]
          )
        }
      }

      graphics::lines(xi, yi, lty = "solid", col = "black")
    }
  }
  if (interval_draw) {
    for (i in mcmc) {
      h <- int[[i]]
      graphics::segments(
        x0 = h[, "start"], x1 = h[, "end"], y0 = i, y1 = i,
        col = if (density) "black" else col.interval[i],
        lty = lty, lwd = lwd, lend = 1
      )
      graphics::segments(
        x0 = c(h[, "start"], h[, "end"]), x1 = c(h[, "start"], h[, "end"]),
        y0 = i, y1 = i + tcl * graphics::strheight("M") * -1,
        col = if (density) "black" else col.interval[i],
        lty = lty, lwd = lwd, lend = 1
      )
    }
  }

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    aion::year_axis(side = 1, format = TRUE, calendar = calendar,
                    current_calendar = calendar)
    graphics::mtext(names(x)[mcmc], side = 2, at = mcmc, las = 2, padj = 0)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- if (is.null(calendar)) expression(italic("rata die")) else aion::format(calendar)
    ylab <- NULL
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  invisible(x)
}

#' @export
#' @rdname plot_events
#' @aliases plot,MCMC,missing-method
setMethod("plot", c(x = "MCMC", y = "missing"), plot.MCMC)

# PhasesMCMC ===================================================================
#' @export
#' @method plot PhasesMCMC
plot.PhasesMCMC <- function(x, calendar = get_calendar(),
                            density = TRUE, range = TRUE, succession = NULL,
                            level = 0.95, sort = TRUE, decreasing = TRUE,
                            legend = TRUE, main = NULL, sub = NULL,
                            ann = graphics::par("ann"), axes = TRUE,
                            frame.plot = FALSE,
                            panel.first = NULL, panel.last = NULL,
                            col.density = "grey", col.range = "black",
                            col.succession = c("#77AADD", "#EE8866"), ...) {
  ## Get data
  n_phases <- dim(x)[2L]
  n_bound <- dim(x)[3L]

  ## Graphical parameters
  dots <- list(...)
  lwd <- get_par(dots, "lwd")
  tcl <- get_par(dots, "tcl")
  if (length(col.density) != n_phases)
    col.density <- rep(col.density, length.out = n_phases)
  if (length(col.range) != n_phases)
    col.range <- rep(col.range, length.out = n_phases)
  col.succession <- rep(col.succession, length.out = 2)
  fill.density <- grDevices::adjustcolor(col.density, alpha.f = 0.5)
  fill.succession <- grDevices::adjustcolor(col.succession, alpha.f = 0.5)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- xlim(x, calendar = calendar)
  ylim <- c(1, n_phases + 1)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Reorder data
  k <- seq_len(n_phases)
  if (sort && n_bound > 1) {
    k <- sort.list(x, decreasing = decreasing)
    x <- x[, k, , drop = FALSE]
    col.density <- col.density[k]
    fill.density <- fill.density[k]
  }

  ## Plot
  ages <- rev(seq_len(n_phases))

  ## Succession
  if (!is.null(succession) && !is.null(level)) {
    if (n_phases != 2)
      stop(tr_("Time ranges can only be displayed with two phases."), call. = FALSE)

    succession <- match.arg(succession, choices = c("transition", "hiatus"),
                            several.ok = TRUE)

    for (s in seq_along(succession)) {
      fun <- match.fun(succession[[s]])
      hia <- as.data.frame(fun(x, level = level), calendar = calendar)

      if (NROW(hia) > 0 ) {
        graphics::rect(
          xleft = hia$start, xright = hia$end,
          ybottom = min(ylim), ytop = max(ylim),
          border = "white",
          col = fill.succession[[s]]
        )
      } else {
        msg <- tr_("Could not find a %s between these two phases.")
        warning(sprintf(msg, succession), call. = FALSE)
      }
    }
  }

  ## Density
  if (density) {
    for (j in ages) {
      for (k in seq_len(n_bound)) {
        p <- x[, j, k, drop = TRUE]
        d <- stats::density(p, n = getOption("ArchaeoPhases.grid"), ...)

        years <- aion::as_year(d$x, calendar = calendar)
        dens <- (d$y - min(d$y)) / max(d$y - min(d$y)) * 0.9
        d0 <- which(dens > 0) # Keep only density > 0
        lb <- if (min(d0) > 1) min(d0) - 1 else min(d0)
        ub <- if (max(d0) < length(years)) max(d0) + 1 else max(d0)
        xi <- c(years[lb], years[d0], years[ub])
        yi <- c(0, dens[d0], 0) + j

        graphics::polygon(xi, yi, border = NA, col = fill.density[j])
        graphics::lines(xi, yi, lty = k, col = "black")
      }
    }
  }

  ## Time range
  if (isTRUE(range) && !is.null(level) && n_bound > 1) {
    bound <- boundaries(x, level = level)
    bound <- as.data.frame(bound, calendar = calendar)
    for (i in ages) {
      h <- bound[i, , drop = FALSE]
      graphics::segments(
        x0 = h[, "start"], x1 = h[, "end"],
        y0 = i, y1 = i,
        col = col.range[i],
        lty = 1,
        lwd = lwd,
        lend = 1
      )
      graphics::segments(
        x0 = c(h[, "start"], h[, "end"]), x1 = c(h[, "start"], h[, "end"]),
        y0 = i, y1 = i + tcl * graphics::strheight("M") * -1,
        col = col.range[i],
        lty = 1,
        lwd = lwd,
        lend = 1
      )
    }
  }

  ## Legend
  if (legend) {
    lab <- c(density, density)
    graphics::legend(
      x = ifelse(decreasing, "topright", "topleft"),
      legend = c(tr_("Phase start"), tr_("Phase end"))[lab],
      lty = c(1, 2)[lab],
      bty = "n"
    )
  }

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    aion::year_axis(side = 1, format = TRUE, calendar = calendar,
                    current_calendar = calendar)
    graphics::mtext(names(x)[ages], side = 2, at = ages, las = 2, padj = 0)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- if (is.null(calendar)) expression(italic("rata die")) else aion::format(calendar)
    ylab <- NULL
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  invisible(x)
}

#' @export
#' @rdname plot_phases
#' @aliases plot,PhasesMCMC,missing-method
setMethod("plot", c(x = "PhasesMCMC", y = "missing"), plot.PhasesMCMC)

# TempoEvents ==================================================================
#' @export
#' @method plot CumulativeEvents
plot.CumulativeEvents <- function(x, calendar = get_calendar(),
                                  interval = c("credible", "gauss"),
                                  col.tempo = "#004488", col.interval = "grey",
                                  main = NULL, sub = NULL, ann = graphics::par("ann"),
                                  axes = TRUE, frame.plot = axes,
                                  panel.first = NULL, panel.last = NULL, ...) {
  ## Graphical parameters
  dots <- list(...)
  lty <- get_par(dots, "lty")
  lwd <- get_par(dots, "lwd")

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- xlim(x, calendar = calendar)
  ylim <- range(x)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  years <- aion::time(x, calendar = calendar)
  interval <- match.arg(interval, several.ok = FALSE)
  if (interval == "credible" && nrow(x@credible) > 0) {
    plot_y_ribbon(x = years, ymin = x@credible[, 1], ymax = x@credible[, 2],
                  col = col.interval, border = NA)
  }
  if (interval == "gauss" && nrow(x@gauss) > 0) {
    plot_y_ribbon(x = years, ymin = x@gauss[, 1], ymax = x@gauss[, 2],
                  col = col.interval, border = NA)
  }
  graphics::lines(x = years, y = x[, 1, 1], col = col.tempo, lty = lty, lwd = lwd)

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    aion::year_axis(side = 1, format = TRUE, calendar = calendar,
                    current_calendar = calendar)
    graphics::axis(side = 2, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- if (is.null(calendar)) expression(italic("rata die")) else aion::format(calendar)
    ylab <- tr_("Cumulative events")
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  invisible(x)
}

#' @export
#' @rdname tempo
#' @aliases plot,CumulativeEvents,missing-method
setMethod("plot", c(x = "CumulativeEvents", y = "missing"), plot.CumulativeEvents)

# ActivityEvents ===============================================================
#' @export
#' @method plot ActivityEvents
plot.ActivityEvents <- function(x, calendar = get_calendar(),
                                main = NULL, sub = NULL,
                                ann = graphics::par("ann"),
                                axes = TRUE, frame.plot = axes,
                                panel.first = NULL, panel.last = NULL, ...) {
  ## Graphical parameters
  dots <- list(...)
  border <- dots$border %||% c("black")
  col <- dots$col %||% c("grey")
  lty <- get_par(dots, "lty")
  lwd <- get_par(dots, "lwd")

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- xlim(x, calendar = calendar)
  ylim <- range(x)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  years <- aion::time(x, calendar = calendar)
  seq_series <- seq_len(NCOL(x))
  for (i in seq_series) {
    plot_y_ribbon(
      x = years,
      ymin = rep(0, length(years)),
      ymax = x[, i, 1, drop = TRUE],
      border = border,
      col = col,
      lty = lty,
      lwd = lwd
    )
  }

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    aion::year_axis(side = 1, format = TRUE, calendar = calendar,
                    current_calendar = calendar)
    graphics::axis(side = 2, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- if (is.null(calendar)) expression(italic("rata die")) else aion::format(calendar)
    ylab <- tr_("Activity")
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  invisible(x)
}

#' @export
#' @rdname activity
#' @aliases plot,ActivityEvents,missing-method
setMethod("plot", c(x = "ActivityEvents", y = "missing"), plot.ActivityEvents)

# OccurrenceEvents =============================================================
#' @export
#' @method plot OccurrenceEvents
plot.OccurrenceEvents <- function(x, calendar = get_calendar(),
                                  main = NULL, sub = NULL,
                                  ann = graphics::par("ann"),
                                  axes = TRUE, frame.plot = axes,
                                  panel.first = NULL, panel.last = NULL, ...) {
  ## Get data
  n_events <- length(x@events)

  ## Graphical parameters
  dots <- list(...)
  col <- get_par(dots, "col")
  lty <- get_par(dots, "lty")
  lwd <- get_par(dots, "lwd")
  cex <- get_par(dots, "cex")
  pch <- dots$pch %||% 16

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  years <- aion::as_fixed(c(x@start, x@end))
  xlim <- xlim(years, calendar = calendar)
  ylim <- range(x@events)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  x_start <- aion::as_year(x@start, calendar = calendar)
  x_end <- aion::as_year(x@end, calendar = calendar)
  graphics::segments(x0 = x_start,  x1 = x_end,
                     y0 = x@events, y1 = x@events,
                     col = col, lty = lty, lwd = lwd)
  graphics::points(x = c(x_start, x_end),
                   y = c(x@events, x@events),
                   pch = pch, col = col, cex = cex)

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    aion::year_axis(side = 1, format = TRUE, calendar = calendar,
                    current_calendar = calendar)
    graphics::axis(side = 2, at = seq_len(n_events), labels = x@events, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- if (is.null(calendar)) expression(italic("rata die")) else aion::format(calendar)
    ylab <- tr_("Occurrence")
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  invisible(x)
}

#' @export
#' @rdname occurrence
#' @aliases plot,OccurrenceEvents,missing-method
setMethod("plot", c(x = "OccurrenceEvents", y = "missing"), plot.OccurrenceEvents)

# AgeDepthModel ================================================================
#' @export
#' @method plot AgeDepthModel
plot.AgeDepthModel <- function(x, level = 0.95,
                               calendar = get_calendar(),
                               main = NULL, sub = NULL,
                               ann = graphics::par("ann"),
                               axes = TRUE, frame.plot = axes,
                               panel.first = NULL, panel.last = NULL, ...) {
  ## Get data
  depth <- x@depth
  n <- length(depth)
  data <- predict(x)
  data <- summary(data, level = level, calendar = NULL)

  ## Graphical parameters
  dots <- list(...)
  border <- dots$border %||% c("grey70")
  col <- dots$col %||% c("grey70")
  lty <- get_par(dots, "lty")
  lwd <- get_par(dots, "lwd")
  pch <- dots$pch %||% 16

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  years <- aion::as_fixed(c(data$median, data$start, data$end))
  xlim <- xlim(years, calendar = calendar)
  ylim <- sort(range(depth), decreasing = TRUE)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  plot_x_ribbon(
    xmin = aion::as_year(data$start, calendar = calendar),
    xmax = aion::as_year(data$end, calendar = calendar),
    y = depth,
    border = border,
    col = col
  )
  graphics::lines(
    x = aion::as_year(data$median, calendar = calendar),
    y = depth,
    lty = lty,
    lwd = lwd
  )
  graphics::points(
    x = aion::as_year(data$median, calendar = calendar),
    y = depth,
    pch = pch
  )

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    aion::year_axis(side = 1, format = TRUE, calendar = calendar,
                    current_calendar = calendar)
    graphics::axis(side = 2, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- if (is.null(calendar)) expression(italic("rata die")) else aion::format(calendar)
    ylab <- tr_("Depth")
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  invisible(x)
}

#' @export
#' @rdname bury
#' @aliases plot,AgeDepthModel,missing-method
setMethod("plot", c(x = "AgeDepthModel", y = "missing"), plot.AgeDepthModel)
