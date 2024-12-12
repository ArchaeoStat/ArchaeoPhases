# PHASES
#' @include AllGenerics.R
NULL

# Build phases =================================================================
#' @export
#' @rdname phases
#' @aliases phases,EventsMCMC,missing-method
setMethod(
  f = "phases",
  signature = c(x = "EventsMCMC", groups = "missing"),
  definition = function(x) {
    groups <- list(seq_len(ncol(x)))
    methods::callGeneric(x = x, groups = groups)
  }
)

#' @export
#' @rdname phases
#' @aliases phases,EventsMCMC,list-method
setMethod(
  f = "phases",
  signature = c(x = "EventsMCMC", groups = "list"),
  definition = function(x, groups) {
    m <- NROW(x)
    n <- length(groups) # Number of phases
    k <- seq_len(n)

    ## Phase names
    pha <- if (is.null(names(groups))) paste0("P", k) else names(groups)

    ## Build array
    arr <- array(data = NA_real_, dim = c(m, n, 2))
    dimnames(arr) <- list(NULL, pha, c("start", "end"))
    for (i in k) {
      index <- groups[[i]]
      tmp <- x[, index, drop = FALSE]
      arr[, i, 1] <- apply(X = tmp, MARGIN = 1, FUN = min)
      arr[, i, 2] <- apply(X = tmp, MARGIN = 1, FUN = max)
    }

    .PhasesMCMC(
      arr,
      labels = pha,
      iteration = x@iteration,
      hash = get_hash(x)
    )
  }
)

# To PhasesMCMC ================================================================
#' @export
#' @rdname as_phases
#' @aliases as_phases,matrix-method
setMethod(
  f = "as_phases",
  signature = c(from = "matrix"),
  definition = function(from, calendar = getOption("ArchaeoPhases.calendar"),
                        start = seq(from = 1, to = ncol(from), by = 2),
                        stop = start + 1, names = NULL, iteration = NULL) {
    ## Validation
    arkhe::assert_length(stop, length(start))

    ## Remove the iteration column
    iter <- seq_len(NROW(from))
    if (!is.null(iteration)) {
      iter <- from[, iteration]
      from <- from[, -iteration, drop = FALSE]
    }

    ## Phase names
    pha <- if (is.null(names)) paste0("P", seq_along(start)) else names

    ## Build array
    arr <- array(data = NA_real_, dim = c(NROW(from), length(start), 2))
    arr[, , 1] <- from[, start, drop = TRUE]
    arr[, , 2] <- from[, stop, drop = TRUE]

    if (!is.null(calendar)) {
      if (methods::is(from, "EventsMCMC")) {
        msg <- "%s is already expressed in rata die: %s is ignored."
        warning(sprintf(msg, sQuote("from"), sQuote("calendar")), call. = FALSE)
      } else {
        ## Coerce to vector
        dn <- dimnames(arr)
        d <- dim(arr)
        dim(arr) <- NULL

        ## Convert to rata die
        arr <- aion::fixed(arr, calendar = calendar)
        dim(arr) <- d
        dimnames(arr) <- dn
      }
    }

    dimnames(arr) <- list(NULL, pha, c("start", "end"))

    .PhasesMCMC(
      arr,
      labels = pha,
      iteration = as.integer(iter)
    )
  }
)

#' @export
#' @rdname as_phases
#' @aliases as_phases,data.frame-method
setMethod(
  f = "as_phases",
  signature = c(from = "data.frame"),
  definition = function(from, calendar = getOption("ArchaeoPhases.calendar"),
                        start = seq(from = 1, to = ncol(from), by = 2),
                        stop = start + 1, names = NULL, iteration = NULL) {
    from <- data.matrix(from)
    methods::callGeneric(from, calendar = calendar, start = start, stop = stop,
                         names = names, iteration = iteration)
  }
)
