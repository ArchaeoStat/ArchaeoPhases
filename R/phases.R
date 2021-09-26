# PHASES
#' @include AllClasses.R AllGenerics.R
NULL

# Min-Max ======================================================================
#' @export
#' @rdname phase
#' @aliases as_phases,MCMC-method
setMethod(
  f = "as_phases",
  signature = c(from = "MCMC"),
  definition = function(from, start = seq(from = 1, to = ncol(from), by = 2),
                        stop = start + 1, names = NULL, ordered = FALSE) {
    ## Validation
    # TODO: check that length(start) == lenght(stop)

    pha <- if (is.null(names)) paste0("phase_", seq_along(start)) else names
    arr <- array(data = NA_real_, dim = c(nrow(from), length(start), 2),
                 dimnames = list(NULL, pha, c("begin", "end")))

    arr[, , 1] <- from[, start]
    arr[, , 2] <- from[, stop]

    .PhasesMCMC(
      arr,
      phases = pha,
      ordered = ordered,
      calendar = get_calendar(from),
      hash = get_hash(from)
    )
  }
)

#' @export
#' @rdname phase
#' @aliases as_phases,matrix-method
setMethod(
  f = "as_phases",
  signature = c(from = "matrix"),
  definition = function(from, start = seq(from = 1, to = ncol(from), by = 2),
                        stop = start + 1, names = NULL, ordered = FALSE,
                        BP = FALSE, iteration = NULL) {
    ## Validation
    # TODO: check that length(start) == lenght(stop)

    ## Remove the iteration column
    if (!is.null(iteration))
      from <- from[, -iteration]

    ## Convert from BP to CE
    if (BP)
      from <- BP_to_CE(from)

    pha <- if (is.null(names)) paste0("phase_", seq_along(start)) else names
    arr <- array(data = NA_real_, dim = c(nrow(from), length(start), 2),
                 dimnames = list(NULL, pha, c("begin", "end")))

    arr[, , 1] <- from[, start]
    arr[, , 2] <- from[, stop]

    .PhasesMCMC(
      arr,
      phases = pha,
      ordered = ordered,
      calendar = "CE"
    )
  }
)

#' @export
#' @rdname phase
#' @aliases as_phases,data.frame-method
setMethod(
  f = "as_phases",
  signature = c(from = "data.frame"),
  definition = function(from, start = seq(from = 1, to = ncol(from), by = 2),
                        stop = start + 1, names = NULL, ordered = FALSE,
                        BP = FALSE, iteration = NULL) {
    from <- data.matrix(from)
    methods::callGeneric(from, start = start, stop = stop, names = names,
                         ordered = ordered, BP = BP, iteration = iteration)
  }
)

# Build phases =================================================================
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
    n <- length(groups) # Number of phases
    k <- seq_len(n)
    grp <- if (is.null(names(groups))) paste0("phase_", k) else names(groups)

    ## Calendar scale
    fun_min <- min
    fun_max <- max
    if (is_BP(x)) {
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
    endpoints <- round(p[, short], getOption("ArchaeoPhases.precision"))

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
    ## Check calendar
    BP <- is_BP(x)

    ## Get phases
    pha <- get_order(x)

    ## Reverse boundaries if BP scale
    start <- ifelse(BP, 2, 1)
    end <- ifelse(BP, 1, 2)

    ## Matrix of results
    result <- matrix(nrow = ncol(x), ncol = 2)

    k <- seq_along(pha)
    for (i in seq_along(pha)) {
      a <- x[, i, start]
      b <- x[, i, end]
      result[i, ] <- boundaries(a, b, level = level)
    }

    ## Re-reverse boundaries if BP scale
    if (BP) {
      result <- result[, c(2, 1), drop = FALSE]
    }

    ## Names
    dimnames(result) <- list(pha, c("start", "end"))

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
    y - x
  }
)

#' @export
#' @rdname duration
#' @aliases duration,PhasesMCMC,missing-method
setMethod(
  f = "duration",
  signature = c(x = "PhasesMCMC", y = "missing"),
  definition = function(x) {
    ## Check calendar
    BP <- is_BP(x)

    ## Get phases
    pha <- get_order(x)

    ## Reverse boundaries if BP scale
    start <- ifelse(BP, 2, 1)
    end <- ifelse(BP, 1, 2)

    # Matrix of results
    result <- matrix(nrow = nrow(x), ncol = ncol(x))
    dimnames(result) <- list(rownames(x), pha)

    k <- seq_along(pha)
    for (i in k) {
      a <- x[, i, start]
      b <- x[, i, end]
      result[, i] <- duration(a, b)
    }

    .MCMC(
      result,
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
    ## Validation
    if (!is_ordered(x)) {
      stop("Phases must be arranged in chronological order.", call. = FALSE)
    }

    ## Check calendar
    BP <- is_BP(x)

    ## Get phases
    pha <- get_order(x)
    m <- ncol(x) - 1

    ## Reverse boundaries if BP scale
    start <- ifelse(BP, 1, 2)
    end <- ifelse(BP, 2, 1)
    fun_head <- ifelse(BP, utils::tail, utils::head)
    fun_tail <- ifelse(BP, utils::head, utils::tail)

    k <- seq_len(m + 1)
    deb <- fun_head(k, -1)
    fin <- fun_tail(k, -1)

    ## Matrix of results
    result <- matrix(nrow = m, ncol = 2)

    for (i in seq_len(m)) {
      a <- x[, deb[[i]], start]
      b <- x[, fin[[i]], end]
      result[i, ] <- boundaries(a, b, level = level)
    }

    ## Re-reverse boundaries if BP scale
    if (BP) {
      result <- result[, c(2, 1), drop = FALSE]
    }

    ## Names
    names_start <- if (BP) fin else deb
    names_end <- if (BP) deb else fin
    rownames(result) <- paste(pha[names_start], pha[names_end], sep = "-")
    colnames(result) <- c("start", "end")

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
    endpoints <- round(p[, i], getOption("ArchaeoPhases.precision"))

    if (p[2, i] == p[1, i]) return(no_hiatus)

    inf <- endpoints[[1]]
    sup <- endpoints[[2]]
    c(start = inf, end = sup)
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

    ## Check calendar
    BP <- is_BP(x)

    ## Get phases
    pha <- get_order(x)
    m <- ncol(x) - 1

    ## Reverse boundaries if BP scale
    start <- ifelse(BP, 1, 2)
    end <- ifelse(BP, 2, 1)
    fun_head <- ifelse(BP, utils::tail, utils::head)
    fun_tail <- ifelse(BP, utils::head, utils::tail)

    k <- seq_len(m + 1)
    deb <- fun_head(k, -1)
    fin <- fun_tail(k, -1)

    ## Matrix of results
    result <- matrix(nrow = m, ncol = 2)

    for (i in seq_len(m)) {
      a <- x[, deb[[i]], start]
      b <- x[, fin[[i]], end]
      result[i, ] <- hiatus(a, b, level = level)
    }

    ## Re-reverse boundaries if BP scale
    if (BP) {
      result <- result[, c(2, 1), drop = FALSE]
    }

    ## Names
    names_start <- if (BP) fin else deb
    names_end <- if (BP) deb else fin
    rownames(result) <- paste(pha[names_start], pha[names_end], sep = "-")
    colnames(result) <- c("start", "end")

    as.data.frame(result)
  }
)
