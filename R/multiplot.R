# MULTIPLOT
#' @include AllGenerics.R
NULL

#' @export
#' @rdname multiplot
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
    n_tempo <- length(dots)

    ## Calendar scale
    scales <- vapply(X = dots, FUN = get_calendar, FUN.VALUE = character(1))
    if (length(unique(scales)) != 1) {
      stop("All object must have the same calendar scale.", call. = FALSE)
    }

    ## Graphical parameters
    col <- graphics::par("col")
    lty <- graphics::par("lty")
    lwd <- graphics::par("lwd")
    if (length(col) != n_tempo) col <- rep(col, length.out = n_tempo)
    if (length(lty) != n_tempo) lty <- rep(lty, length.out = n_tempo)
    if (length(lwd) != n_tempo) lwd <- rep(lwd, length.out = n_tempo)
    fill <- grDevices::adjustcolor(col, alpha.f = 0.5)

    ## Open new window
    grDevices::dev.hold()
    on.exit(grDevices::dev.flush(), add = TRUE)
    graphics::plot.new()

    ## Set plotting coordinates
    xlim <- vapply(X = dots, FUN = function(x) range(x@years),
                   FUN.VALUE = numeric(2))
    xlim <- if (is_CE(dots[[1]])) c(min(xlim), max(xlim)) else c(max(xlim), min(xlim))
    ylim <- vapply(X = dots, FUN = function(x) range(x@estimate),
                   FUN.VALUE = numeric(2))
    graphics::plot.window(xlim = xlim, ylim = range(ylim))

    ## Evaluate pre-plot expressions
    # panel.first

    seq_tempo <- seq_along(dots)
    for (i in seq_tempo) {
      graphics::lines(
        x = dots[[i]]@years,
        y = dots[[i]]@estimate,
        col = col[i],
        lty = lty[i],
        lwd = lwd[i]
      )
    }

    ## Evaluate post-plot and pre-axis expressions
    # panel.last

    ## Construct Axis
    if (graphics::par("xaxt") != "n") graphics::axis(side = 1)
    if (graphics::par("yaxt") != "n") graphics::axis(side = 2, las = 1)

    ## Plot frame
    if (graphics::par("bty") != "n") graphics::box()

    ## Add annotation
    if (graphics::par("ann")) {
      xlab <- time_axis_label(dots[[1]])
      ylab <- "Cumulative events"
      graphics::title(main = NULL, sub = NULL, xlab = xlab, ylab = ylab)
    }

    invisible(dots)
  }
)

#' @export
#' @rdname multiplot
#' @aliases multiplot,ActivityEvents-method
setMethod(
  f = "multiplot",
  signature = "ActivityEvents",
  definition = function(...) {
    ## Get names
    subst <- substitute(list(...))[-1]
    arg_names <- vapply(X = subst, FUN = deparse, FUN.VALUE = character(1))

    ## Get data
    dots <- list(...)
    n_act <- length(dots)

    ## Calendar scale
    scales <- vapply(X = dots, FUN = get_calendar, FUN.VALUE = character(1))
    if (length(unique(scales)) != 1) {
      stop("All object must have the same calendar scale.", call. = FALSE)
    }

    ## Graphical parameters
    col <- graphics::par("col")
    lty <- graphics::par("lty")
    lwd <- graphics::par("lwd")
    if (length(col) != n_act) col <- rep(col, length.out = n_act)
    if (length(lty) != n_act) lty <- rep(lty, length.out = n_act)
    if (length(lwd) != n_act) lwd <- rep(lwd, length.out = n_act)
    fill <- grDevices::adjustcolor(col, alpha.f = 0.5)

    ## Open new window
    grDevices::dev.hold()
    on.exit(grDevices::dev.flush(), add = TRUE)
    graphics::plot.new()

    ## Set plotting coordinates
    xlim <- vapply(X = dots, FUN = function(x) range(x@years),
                   FUN.VALUE = numeric(2))
    xlim <- if (is_CE(dots[[1]])) c(min(xlim), max(xlim)) else c(max(xlim), min(xlim))
    ylim <- vapply(X = dots, FUN = function(x) range(x@estimate),
                   FUN.VALUE = numeric(2))
    graphics::plot.window(xlim = xlim, ylim = range(ylim))

    ## Evaluate pre-plot expressions
    # panel.first

    seq_act <- seq_along(dots)
    for (i in seq_act) {
      years <- dots[[i]]@years
      plot_y_ribbon(
        x = years,
        ymin = rep(0, length(years)),
        ymax = dots[[i]]@estimate,
        border = col[i],
        col = fill[i],
        lty = lty[i],
        lwd = lwd[i]
      )
    }

    ## Evaluate post-plot and pre-axis expressions
    # panel.last

    ## Construct Axis
    if (graphics::par("xaxt") != "n") graphics::axis(side = 1)
    if (graphics::par("yaxt") != "n") graphics::axis(side = 2, las = 1)

    ## Plot frame
    if (graphics::par("bty") != "n") graphics::box()

    ## Add annotation
    if (graphics::par("ann")) {
      xlab <- time_axis_label(dots[[1]])
      ylab <- "Activity"
      graphics::title(main = NULL, sub = NULL, xlab = xlab, ylab = ylab)
    }

    invisible(dots)
  }
)
