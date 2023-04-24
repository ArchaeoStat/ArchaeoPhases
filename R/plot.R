# PLOT
#' @include AllGenerics.R
NULL

# MCMC =========================================================================
#' @export
#' @method plot MCMC
plot.MCMC <- function(x, density = TRUE, interval = NULL, level = 0.95,
                      decreasing = TRUE,
                      main = NULL, sub = NULL,
                      ann = graphics::par("ann"), axes = TRUE,
                      frame.plot = FALSE,
                      panel.first = NULL, panel.last = NULL, ...) {
  ## Get data
  n_events <- dim(x)[2L]

  ## Graphical parameters
  col <- list(...)$col %||% c("grey")
  lty <- list(...)$lty %||% graphics::par("lty")
  lwd <- list(...)$lwd %||% graphics::par("lwd")
  tcl <- list(...)$tcl %||% graphics::par("tcl")
  if (length(col) != n_events) col <- rep(col, length.out = n_events)
  fill <- grDevices::adjustcolor(col, alpha.f = 0.5)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- range(x)
  xlim <- if (is_CE(x)) c(min(xlim), max(xlim)) else c(max(xlim), min(xlim))
  ylim <- c(1, n_events + 1.5)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Reorder data
  k <- sort.list(x, decreasing = decreasing)
  x <- x[, k, drop = FALSE]
  col <- col[k]
  fill <- fill[k]

  ## Plot
  ages <- rev(seq_len(n_events))
  if (density) {
    for (i in ages) {
      d <- stats::density(x[, i, drop = TRUE], n = getOption("ArchaeoPhases.grid"), ...)

      years <- d$x
      dens <- (d$y - min(d$y)) / max(d$y - min(d$y)) * 1.5
      k <- which(dens > 0) # Keep only density > 0
      lb <- if (min(k) > 1) min(k) - 1 else min(k)
      ub <- if (max(k) < length(years)) max(k) + 1 else max(k)
      xi <- c(years[lb], years[k], years[ub])
      yi <- c(0, dens[k], 0) + i

      graphics::polygon(xi, yi, border = NA, col = fill[i])
      graphics::lines(xi, yi, lty = "solid", col = "black")
    }
  }
  if (!is.null(interval) & !is.null(level)) {
    interval <- match.arg(interval, choices = c("credible", "hpdi"))
    fun <- switch (interval, credible = credible, hpdi = hpdi)
    inter <- fun(x, level = level)
    for (i in ages) {
      h <- inter[[i]]
      graphics::segments(
        x0 = h[, "start"], x1 = h[, "stop"],
        y0 = i, y1 = i,
        col = if (density) "black" else col[i],
        lty = lty, lwd = lwd,
        lend = 1
      )
      graphics::segments(
        x0 = c(h[, "start"], h[, "stop"]), x1 = c(h[, "start"], h[, "stop"]),
        y0 = i, y1 = i + tcl * graphics::strheight("M") * -1,
        col = if (density) "black" else col[i],
        lty = lty, lwd = lwd,
        lend = 1
      )
    }
  }

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    graphics::axis(side = 1)
    graphics::mtext(names(x)[ages], side = 2, at = ages, las = 2, padj = 0)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- time_axis_label(x)
    ylab <- NULL
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  invisible(x)
}

#' @export
#' @describeIn plot Plots of credible intervals or HPD regions of a series of
#' events.
#' @aliases plot,MCMC,missing-method
setMethod("plot", c(x = "MCMC", y = "missing"), plot.MCMC)

# PhasesMCMC ===================================================================
#' @export
#' @method plot PhasesMCMC
plot.PhasesMCMC <- function(x, density = TRUE, boundaries = TRUE, range = NULL,
                            level = 0.95, decreasing = TRUE,
                            main = NULL, sub = NULL,
                            ann = graphics::par("ann"), axes = TRUE,
                            frame.plot = FALSE,
                            panel.first = NULL, panel.last = NULL, ...) {
  ## Get data
  n_phases <- dim(x)[2L]

  ## Graphical parameters
  col <- list(...)$col %||% c("grey")
  lty <- list(...)$lty %||% graphics::par("lty")
  lwd <- list(...)$lwd %||% graphics::par("lwd")
  tcl <- list(...)$tcl %||% graphics::par("tcl")
  if (length(col) != n_phases) col <- rep(col, length.out = n_phases)
  fill <- grDevices::adjustcolor(col, alpha.f = 0.5)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- range(x)
  xlim <- if (is_CE(x)) c(min(xlim), max(xlim)) else c(max(xlim), min(xlim))
  ylim <- c(1, n_phases + 1)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Reorder data
  k <- sort.list(x, decreasing = decreasing)
  x <- x[, k, , drop = FALSE]
  col <- col[k]
  fill <- fill[k]

  ## Plot
  ages <- rev(seq_len(n_phases))
  if (density) {
    ## Density
    for (i in ages) {
      p <- x[, i, ]
      for (j in c(1, 2)) {
        d <- stats::density(p[, j], n = getOption("ArchaeoPhases.grid"), ...)

        years <- d$x
        dens <- (d$y - min(d$y)) / max(d$y - min(d$y)) * 0.9
        k <- which(dens > 0) # Keep only density > 0
        lb <- if (min(k) > 1) min(k) - 1 else min(k)
        ub <- if (max(k) < length(years)) max(k) + 1 else max(k)
        xi <- c(years[lb], years[k], years[ub])
        yi <- c(0, dens[k], 0) + i

        graphics::polygon(xi, yi, border = NA, col = fill[i])
        graphics::lines(xi, yi, lty = "solid", col = "black")
      }
    }
  }

  if (!is.null(level)) {
    ## Time range
    if (boundaries) {
      bound <- boundaries(x, level = level)
      for (i in ages) {
        h <- bound[i, ]
        graphics::segments(
          x0 = h[, "start"], x1 = h[, "stop"],
          y0 = i, y1 = i,
          col = "black",
          lty = lty, lwd = lwd,
          lend = 1
        )
        graphics::segments(
          x0 = c(h[, "start"], h[, "stop"]), x1 = c(h[, "start"], h[, "stop"]),
          y0 = i, y1 = i + tcl * graphics::strheight("M") * -1,
          col = "black",
          lty = lty, lwd = lwd,
          lend = 1
        )
      }
    }

    ## Succession
    if (!is.null(range)) {
      if (n_phases != 2)
        stop("Time ranges can only be displayed with two phases.", call. = FALSE)

      range <- match.arg(range, choices = c("transition", "hiatus"), several.ok = FALSE)
      fun <- switch(range, hiatus = hiatus, transition = transition)
      hia <- as.data.frame(fun(x, level = level))

      graphics::rect(
        xleft = hia$start, xright = hia$stop,
        ybottom = min(ylim), ytop = max(ylim),
        border = "black",
        lty = "dashed"
      )
    }
  }

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    graphics::axis(side = 1)
    graphics::mtext(names(x)[ages], side = 2, at = ages, las = 2, padj = 0)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- time_axis_label(x)
    ylab <- NULL
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  invisible(x)
}

#' @export
#' @describeIn plot Plots the characteristics of a group of events (phase).
#' @aliases plot,PhasesMCMC,missing-method
setMethod("plot", c(x = "PhasesMCMC", y = "missing"), plot.PhasesMCMC)

# TempoEvents ==================================================================
#' @export
#' @method plot CumulativeEvents
plot.CumulativeEvents <- function(x, credible = TRUE, gauss = TRUE, legend = TRUE,
                                  col.tempo = "#004488", lty.tempo = "solid", lwd.tempo = 1,
                                  col.credible = "#BB5566", lty.credible = "dashed", lwd.credible = 1,
                                  col.gauss = "#DDAA33", lty.gauss = "dotted", lwd.gauss = 1,
                                  main = NULL, sub = NULL, ann = graphics::par("ann"),
                                  axes = TRUE, frame.plot = axes,
                                  panel.first = NULL, panel.last = NULL, ...) {
  ## Get data
  years <- x@years

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- range(years)
  xlim <- if (is_CE(x)) c(min(xlim), max(xlim)) else c(max(xlim), min(xlim))
  ylim <- range(x@estimate, x@credible, x@gauss)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  graphics::lines(
    x = years,
    y = x@estimate,
    type = "l",
    col = col.tempo,
    lty = lty.tempo,
    lwd = lwd.tempo
  )

  ## Intervals
  credible <- credible & nrow(x@credible) > 0
  if (credible) {
    plot_y_ribbon(x = years, ymin = x@credible[, 1], ymax = x@credible[, 2],
                border = col.credible, lty = lty.credible, lwd = lwd.credible)
  }
  gauss <- gauss & nrow(x@gauss) > 0
  if (gauss) {
    plot_y_ribbon(x = years, ymin = x@gauss[, 1], ymax = x@gauss[, 2],
                border = col.gauss, lty = lty.gauss, lwd = lwd.gauss)
  }

  ## Legend
  if (legend) {
    lab <- c(TRUE, credible, gauss)
    graphics::legend(
      x = "topleft",
      legend = c("Bayes estimate", "Credible interval", "Gauss interval")[lab],
      col = c(col.tempo, col.credible, col.gauss)[lab],
      lty = c(lty.tempo, lty.credible, lty.gauss)[lab],
      lwd = c(lwd.tempo, lwd.credible, lwd.gauss)[lab]
    )
  }

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    graphics::axis(side = 1)
    graphics::axis(side = 2, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- time_axis_label(x)
    ylab <- "Cumulative events"
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
plot.ActivityEvents <- function(x, main = NULL, sub = NULL,
                                ann = graphics::par("ann"),
                                axes = TRUE, frame.plot = axes,
                                panel.first = NULL, panel.last = NULL, ...) {
  ## Get data
  years <- x@years

  ## Graphical parameters
  border <- list(...)$border %||% c("black")
  col <- list(...)$col %||% c("grey70")
  lwd <- list(...)$lwd %||% graphics::par("lwd")
  lty <- list(...)$lty %||% graphics::par("lty")

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- range(years)
  xlim <- if (is_CE(x)) c(min(xlim), max(xlim)) else c(max(xlim), min(xlim))
  ylim <- range(x@estimate)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  plot_y_ribbon(
    x = years,
    ymin = rep(0, length(years)),
    ymax = x@estimate,
    border = border,
    col = col,
    lty = lty,
    lwd = lwd
  )

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    graphics::axis(side = 1)
    graphics::axis(side = 2, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- time_axis_label(x)
    ylab <- "Activity"
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  invisible(x)
}

#' @export
#' @rdname activity
#' @aliases plot,ActivityEvents,missing-method
setMethod("plot", c(x = "ActivityEvents", y = "missing"), plot.ActivityEvents)

# RateOfChange =================================================================
#' @export
#' @method plot RateOfChange
plot.RateOfChange <- function(x, main = NULL, sub = NULL,
                              ann = graphics::par("ann"),
                              axes = TRUE, frame.plot = axes,
                              panel.first = NULL, panel.last = NULL, ...) {
  ## Get data
  years <- x@years

  ## Graphical parameters
  col <- list(...)$col %||% c("black")
  lwd <- list(...)$lwd %||% graphics::par("lwd")
  lty <- list(...)$lty %||% graphics::par("lty")

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- range(years)
  xlim <- if (is_CE(x)) c(min(xlim), max(xlim)) else c(max(xlim), min(xlim))
  ylim <- range(x@estimate)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  graphics::lines(
    x = years,
    y = x@estimate,
    type = "l",
    col = col,
    lty = lty,
    lwd = lwd
  )
  graphics::abline(h = 0, col = "grey20", lty = "dashed", lwd = 1)

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    graphics::axis(side = 1)
    graphics::axis(side = 2, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- time_axis_label(x)
    ylab <- "Rate of change"
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  invisible(x)
}

#' @export
#' @rdname roc
#' @aliases plot,RateOfChange,missing-method
setMethod("plot", c(x = "RateOfChange", y = "missing"), plot.RateOfChange)

# OccurrenceEvents =============================================================
#' @export
#' @method plot OccurrenceEvents
plot.OccurrenceEvents <- function(x, main = NULL, sub = NULL,
                                  ann = graphics::par("ann"),
                                  axes = TRUE, frame.plot = axes,
                                  panel.first = NULL, panel.last = NULL, ...) {
  ## Get data
  n_events <- length(x@events)

  ## Graphical parameters
  col <- list(...)$col %||% graphics::par("col")
  lty <- list(...)$lty %||% graphics::par("lty")
  lwd <- list(...)$lwd %||% graphics::par("lwd")
  pch <- list(...)$pch %||% 16
  cex <- list(...)$cex %||% graphics::par("cex")

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- range(x@start, x@stop)
  xlim <- if (is_CE(x)) c(min(xlim), max(xlim)) else c(max(xlim), min(xlim))
  ylim <- range(x@events)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  graphics::segments(x0 = x@start,  x1 = x@stop,
                     y0 = x@events, y1 = x@events,
                     col = col, lty = lty, lwd = lwd)
  graphics::points(x = c(x@start, x@stop),
                   y = c(x@events, x@events),
                   pch = pch, col = col, cex = cex)

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    graphics::axis(side = 1)
    graphics::axis(side = 2, at = seq_len(n_events), labels = x@events, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- time_axis_label(x)
    ylab <- "Occurrence"
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
                               main = NULL, sub = NULL,
                               ann = graphics::par("ann"),
                               axes = TRUE, frame.plot = axes,
                               panel.first = NULL, panel.last = NULL, ...) {
  ## Get data
  depth <- x@depth
  n <- length(depth)
  data <- predict(x)
  data <- summary(data, level = level)

  ## Graphical parameters
  border <- list(...)$border %||% c("grey70")
  col <- list(...)$col %||% c("grey70")
  lwd <- list(...)$lwd %||% graphics::par("lwd")
  lty <- list(...)$lty %||% graphics::par("lty")
  pch <- list(...)$pch %||% 16

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- range(data$median, data$start, data$stop)
  xlim <- if (is_CE(x)) c(min(xlim), max(xlim)) else c(max(xlim), min(xlim))
  ylim <- sort(range(depth), decreasing = TRUE)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  plot_x_ribbon(
    xmin = data$start,
    xmax = data$stop,
    y = depth,
    border = border,
    col = col
  )
  graphics::lines(
    x = data$median,
    y = depth,
    lty = lty,
    lwd = lwd
  )
  graphics::points(
    x = data$median,
    y = depth,
    pch = pch
  )

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    graphics::axis(side = 1)
    graphics::axis(side = 2, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- time_axis_label(x)
    ylab <- "Depth"
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  invisible(x)
}

#' @export
#' @rdname bury
#' @aliases plot,AgeDepthModel,missing-method
setMethod("plot", c(x = "AgeDepthModel", y = "missing"), plot.AgeDepthModel)

# Helpers ======================================================================
time_axis_label <- function(x) {
  if (is_CE(x)) return("Year CE")
  if (is_BP(x)) return("Year cal. BP")
  if (is_b2k(x)) return("Year b2k")
  if (x@calendar == "elapsed") return("Elapsed origin")
  stop("Unknown calendar scale.", call. = FALSE)
}
plot_x_ribbon <- function(xmin, xmax, y, ...) {
  graphics::polygon(x = c(xmin, rev(xmax)), y = c(y, rev(y)), ...)
}
plot_y_ribbon <- function(x, ymin, ymax, ...) {
  graphics::polygon(x = c(x, rev(x)), y = c(ymin, rev(ymax)), ...)
}
