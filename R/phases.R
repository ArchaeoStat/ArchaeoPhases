# PHASES
#' @include AllClasses.R AllGenerics.R
NULL

# Build phases =================================================================
#' @export
#' @rdname phase
#' @aliases phase,EventsMCMC,missing-method
setMethod(
  f = "phase",
  signature = c(x = "EventsMCMC", groups = "missing"),
  definition = function(x) {
    groups <- list(seq_len(ncol(x)))
    methods::callGeneric(x = x, groups = groups)
  }
)

#' @export
#' @rdname phase
#' @aliases phase,EventsMCMC,list-method
setMethod(
  f = "phase",
  signature = c(x = "EventsMCMC", groups = "list"),
  definition = function(x, groups, ordered = FALSE) {
    m <- nrow(x)
    n <- length(groups) # Number of phases
    k <- seq_len(n)
    grp <- if (is.null(names(groups))) paste0("P", k) else names(groups)

    ## Calendar scale
    fun_min <- min
    fun_max <- max
    if (is_BP(x) || is_b2k(x)) {
      fun_min <- max
      fun_max <- min
    }

    ## Build array
    min_max <- array(data = NA_real_, dim = c(m, n, 2))
    dimnames(min_max) <- list(NULL, grp, c("begin", "end"))
    for (i in k) {
      index <- groups[[i]]
      tmp <- x[, index, drop = FALSE]
      min_max[, i, 1] <- apply(X = tmp, MARGIN = 1, FUN = fun_min)
      min_max[, i, 2] <- apply(X = tmp, MARGIN = 1, FUN = fun_max)
    }

    .PhasesMCMC(
      min_max,
      phases = grp,
      ordered = ordered,
      calendar = get_calendar(x),
      hash = get_hash(x)
    )
  }
)

# Phase order ==================================================================
#' @export
#' @rdname phase
#' @aliases set_order,PhasesMCMC,character-method
setMethod(
  f = "set_order<-",
  signature = c(x = "PhasesMCMC", value = "character"),
  definition = function(x, value) {
    pha <- x@phases
    if (is.null(value)) {
      value <- factor(pha, levels = pha, ordered = FALSE)
    } else {
      index <- match(value, pha)

      ## Validation
      if (anyNA(index)) {
        stop("Some phases do not seem to be defined.", call. = FALSE)
      }

      tmp <- x[, index, ]
      colnames(tmp) <- value
    }
    x@.Data <- tmp
    x@phases <- value
    x@ordered <- TRUE
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname phase
#' @aliases get_order,PhasesMCMC-method
setMethod(
  f = "get_order",
  signature = c(x = "PhasesMCMC"),
  definition = function(x) x@phases
)

#' @export
#' @rdname phase
#' @aliases as_ordered,PhasesMCMC-method
setMethod(
  f = "as_ordered",
  signature = c(x = "PhasesMCMC"),
  definition = function(x) {
    x@ordered <- TRUE
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname phase
#' @aliases is_ordered,PhasesMCMC-method
setMethod(
  f = "is_ordered",
  signature = c(x = "PhasesMCMC"),
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
    arkhe::assert_length(y, length(x))

    # no_bound <- c(lower = NA, upper = NA)
    # if (older(x, y) < 0.5) {
    #   warning("Events do not seem to be in chronological order; ",
    #           "NAs introduced.", call. = FALSE)
    #   return(no_bound)
    # }

    epsilon <- seq(from = 0, to = 1 - level, by = 0.001)
    p <- periode(epsilon, x, y, level = level)

    ## Compute the length of all intervals
    inter <- p[2, ] - p[1, ]

    ## Find the shortest interval
    short <- which.min(inter)
    endpoints <- p[, short]

    ## Return the endpoints of the shortest interval
    c(lower = endpoints[[1]], upper = endpoints[[2]])
  }
)

#' @export
#' @rdname boundaries
#' @aliases boundaries,PhasesMCMC,missing-method
setMethod(
  f = "boundaries",
  signature = c(x = "PhasesMCMC", y = "missing"),
  definition = function(x, level = 0.95) {
    ## Matrix of results
    n <- ncol(x)
    result <- matrix(nrow = n, ncol = 2)

    k <- seq_len(n)
    for (i in k) {
      a <- x[, i, 1]
      b <- x[, i, 2]
      result[i, ] <- boundaries(a, b, level = level)
    }

    ## Reverse boundaries if BP scale
    if (is_BP(x) || is_b2k(x)) {
      result <- result[, c(2, 1), drop = FALSE]
    }

    ## Names
    dimnames(result) <- list(names(x), c("lower", "upper"))

    as.data.frame(result)
  }
)

# Duration =====================================================================
#' @export
#' @rdname duration
#' @aliases duration,numeric,numeric-method
setMethod(
  f = "duration",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    ## Validation
    arkhe::assert_length(y, length(x))

    abs(y - x)
  }
)

#' @export
#' @rdname duration
#' @aliases duration,PhasesMCMC,missing-method
setMethod(
  f = "duration",
  signature = c(x = "PhasesMCMC", y = "missing"),
  definition = function(x) {
    ## Get phases
    pha <- names(x)

    # Matrix of results
    result <- matrix(nrow = nrow(x), ncol = ncol(x))
    dimnames(result) <- list(rownames(x), pha)

    k <- seq_along(pha)
    for (i in k) {
      a <- x[, i, 1]
      b <- x[, i, 2]
      result[, i] <- duration(a, b)
    }

    .DurationsMCMC(
      result,
      events = pha,
      calendar = "elapsed",
      hash = get_hash(x)
    )
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
    ## Get phases
    n <- ncol(x)
    z <- names(x)

    ## Matrix of results
    lower <- upper <- phase <- matrix(nrow = n, ncol = n, dimnames = list(z, z))

    for (i in 1:n) {
      for (j in 1:n) {
        if (i != j) {
          h <- boundaries(x[, i, 2], x[, j, 1], level = level)
          lower[i, j] <- h["lower"]
          upper[i, j] <- h["upper"]
        }
        phase[i, j] <- paste(z[i], z[j], sep = "-")
      }
    }

    ## Remove false results
    drop <- lower > upper

    ## Reverse boundaries if BP scale
    BP <- is_BP(x) || is_b2k(x)
    if (BP) {
      drop <- !drop
      lower <- t(lower)
      upper <- t(upper)
    }

    upper[drop] <- NA
    lower[drop] <- NA

    .TimeRange(
      lower = if (BP) upper else lower,
      upper = if (BP) lower else upper,
      names = phase,
      calendar = get_calendar(x),
      hash = get_hash(x)
    )
  }
)

# Hiatus =======================================================================
#' @export
#' @describeIn hiatus Hiatus between successive phases.
#' @aliases hiatus,PhasesMCMC,missing-method
setMethod(
  f = "hiatus",
  signature = c(x = "PhasesMCMC", y = "missing"),
  definition = function(x, level = 0.95) {
    ## Get phases
    n <- ncol(x)
    z <- names(x)

    ## Matrix of results
    lower <- upper <- phase <- matrix(nrow = n, ncol = n, dimnames = list(z, z))

    for (i in 1:n) {
      for (j in 1:n) {
        if (i != j) {
          h <- hiatus(x[, i, 2], x[, j, 1], level = level)
          lower[i, j] <- h["lower"]
          upper[i, j] <- h["upper"]
        }
        phase[i, j] <- paste(z[i], z[j], sep = "-")
      }
    }

    ## Re-reverse boundaries if BP scale
    BP <- is_BP(x) || is_b2k(x)
    if (BP) {
      lower <- t(lower)
      upper <- t(upper)
    }

    .TimeRange(
      lower = if (BP) upper else lower,
      upper = if (BP) lower else upper,
      names = phase,
      calendar = get_calendar(x),
      hash = get_hash(x)
    )
  }
)
