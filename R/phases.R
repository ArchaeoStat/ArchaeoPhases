# PHASES
#' @include AllClasses.R AllGenerics.R
NULL

# Min-Max ======================================================================
#' @export
#' @rdname phase
#' @aliases phase,MCMC,missing-method
setMethod(
  f = "phase",
  signature = c(x = "MCMC", groups = "missing"),
  definition = function(x) {
    groups <- list(seq_len(ncol(x)))
    methods::callGeneric(x = x, groups = groups)
  }
)

#' @export
#' @rdname phase
#' @aliases phase,MCMC,list-method
setMethod(
  f = "phase",
  signature = c(x = "MCMC", groups = "list"),
  definition = function(x, groups, ordered = FALSE) {
    m <- nrow(x)
    k <- seq_along(groups) # Number of phases
    grp <- if (is.null(names(groups))) paste0("phase_", k) else names(groups)

    for (i in k) {
      index <- groups[[i]]
      tmp <- x[, index, drop = FALSE]
      groups[[i]] <- matrix(
        data = c(apply(X = tmp, MARGIN = 1, FUN = min),
                 apply(X = tmp, MARGIN = 1, FUN = max)),
        nrow = m
      )
    }

    min_max <- do.call(cbind, groups)
    rownames(min_max) <- seq_len(nrow(min_max))
    colnames(min_max) <- paste0(rep(grp, each = 2), c("_start", "_end"))

    start <- seq(from = 1L, to = ncol(min_max), by = 2L)
    .PhasesMCMC(
      min_max,
      start = start,
      end = start + 1L,
      ordered = ordered,
      phases = grp,
      calendar = get_calendar(x),
      hash = get_hash(x)
    )
  }
)

#' @export
#' @rdname phase
#' @aliases get_phases,MCMC-method
setMethod(
  f = "get_phases",
  signature = c(x = "MCMC"),
  definition = function(x) {
    ord <- get_order(x)

    phases <- vector(mode = "list", length = length(ord))
    names(phases) <- as.character(ord)
    k <- as.integer(ord)
    for (i in k) {
      phases[[i]] <- x[[i]]
    }
    phases
  }
)

# Phase order ==================================================================
#' @export
#' @rdname phase
#' @aliases set_order,MCMC,character-method
setMethod(
  f = "set_order<-",
  signature = c(x = "MCMC", value = "character"),
  definition = function(x, value) {
    if (is.null(value)) {
      x@ordered <- FALSE
    } else {
      phases <- x@phases
      index <- match(value, phases)

      ## Validation
      if (anyNA(index)) {
        stop("Some phases do not seem to be defined.", call. = FALSE)
      }

      x@start <- x@start[index]
      x@end <- x@end[index]
      x@ordered <- TRUE
      x@phases <- value
    }
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname phase
#' @aliases get_order,MCMC-method
setMethod(
  f = "get_order",
  signature = c(x = "MCMC"),
  definition = function(x) {
    factor(x@phases, levels = x@phases, ordered = x@ordered)
  }
)

#' @export
#' @rdname phase
#' @aliases is_ordered,MCMC-method
setMethod(
  f = "is_ordered",
  signature = c(x = "MCMC"),
  definition = function(x) x@ordered
)

# Time range ===================================================================
#' @export
#' @rdname boundaries
#' @aliases boundaries,numeric,numeric-method
setMethod(
  f = "boundaries",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, level = 0.95) {
    ## Validation
    if (length(x) != length(y)) {
      stop(sprintf("%s and %s must have the same length.",
                   sQuote("x"), sQuote("y")), call. = FALSE)
    }

    epsilon <- seq(from = 0, to = 1 - level, by = 0.001)
    p <- periode(epsilon, x, y, level = level)
    ## Compute the length of all intervals
    inter <- p[2, ] - p[1, ]
    ## Find the shortest interval
    short <- which.min(inter)
    endpoints <- round(p[, short], 0)
    ## Return the endpoints of the shortest interval
    c(start = endpoints[[1]], end = endpoints[[2]])
  }
)

#' @export
#' @rdname boundaries
#' @aliases boundaries,PhasesMCMC,missing-method
setMethod(
  f = "boundaries",
  signature = c(x = "PhasesMCMC", y = "missing"),
  definition = function(x, level = 0.95) {
    if (length(start) != length(end)) {
      stop(sprintf("%s and %s must have the same length.",
                   sQuote("start"), sQuote("end")), call. = FALSE)
    }

    ## Number of phases
    start <- x@start
    end <- x@end
    L <- length(start)

    # Matrix of results
    result <- matrix(nrow = L, ncol = 2)
    rownames(result) <- x@phases
    colnames(result) <- c("start", "end")

    for (i in seq_len(L)) {
      a <- start[[i]]
      b <- end[[i]]
      result[i, ] <- boundaries(x[, a], x[, b], level = level)
    }

    as.data.frame(result)
  }
)

# Transition ===================================================================
#' @export
#' @rdname transition
#' @aliases transition,numeric,numeric-method
setMethod(
  f = "transition",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, level = 0.95) {
    boundaries(x, y, level = level)
  }
)

#' @export
#' @rdname transition
#' @aliases transition,PhasesMCMC,missing-method
setMethod(
  f = "transition",
  signature = c(x = "PhasesMCMC", y = "missing"),
  definition = function(x, level = 0.95) {
    ## Validation
    if (!is_ordered(x)) {
      stop("Phases must be arranged in chronological order.", call. = FALSE)
    }
    if (length(start) != length(end)) {
      stop(sprintf("%s and %s must have the same length.",
                   sQuote("start"), sQuote("end")), call. = FALSE)
    }

    ## Number of phases
    start <- utils::head(x@end, -1)
    end <- utils::tail(x@start, -1)
    L <- length(start)

    # Names
    names_start <- colnames(x)[start]
    names_end <- colnames(x)[end]

    # Matrix of results
    result <- matrix(nrow = L, ncol = 2)
    rownames(result) <- paste(names_start, names_end, sep = "-")
    colnames(result) <- c("start", "end")

    for (i in seq_len(L)) {
      a <- start[[i]]
      b <- end[[i]]
      result[i, ] <- boundaries(x[, a], x[, b], level = level)
    }

    as.data.frame(result)
  }
)

# Hiatus =======================================================================
#' @export
#' @rdname hiatus
#' @aliases hiatus,numeric,numeric-method
setMethod(
  f = "hiatus",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, level = 0.95) {
    ## Validation
    if (length(x) != length(y)) {
      stop(sprintf("%s and %s must have the same length.",
                   sQuote("x"), sQuote("y")), call. = FALSE)
    }
    # if (!all(x <= y)) {
    #   stop(sprintf("%s must be strictly older than %s",
    #                sQuote("x"), sQuote("y")), call. = FALSE)
    # }

    no_hiatus <- c(lower = NA, upper = NA)

    epsilon <- seq(0, 1 - level, .001)
    p <- gap(epsilon, x, y, level)
    ## Compute the length of all intervals
    inter <- p[2, ] - p[1, ]
    dd <- inter[inter > 0]

    if (length(dd) < 1) return(no_hiatus)

    ## Find the longest interval
    i <- which(inter == max(dd))
    endpoints <- round(p[, i], 0)

    if (p[2, i] == p[1, i]) return(no_hiatus)

    c(endpoints[[1]], endpoints[[2]])
  }
)

#' @export
#' @rdname hiatus
#' @aliases hiatus,PhasesMCMC,missing-method
setMethod(
  f = "hiatus",
  signature = c(x = "PhasesMCMC", y = "missing"),
  definition = function(x, level = 0.95) {
    ## Validation
    if (!is_ordered(x)) {
      stop("Phases must be arranged in chronological order.", call. = FALSE)
    }
    if (length(start) != length(end)) {
      stop(sprintf("%s and %s must have the same length.",
                   sQuote("start"), sQuote("end")), call. = FALSE)
    }

    ## Number of phases
    start <- utils::head(x@end, -1)
    end <- utils::tail(x@start, -1)
    L <- length(start)

    # Names
    names_start <- colnames(x)[start]
    names_end <- colnames(x)[end]

    # Matrix of results
    result <- matrix(nrow = L, ncol = 2)
    rownames(result) <- paste(names_start, names_end, sep = "-")
    colnames(result) <- c("start", "end")

    for (i in seq_len(L)) {
      a <- start[[i]]
      b <- end[[i]]
      result[i, ] <- hiatus(x[, a], x[, b], level = level)
    }

    as.data.frame(result)
  }
)
