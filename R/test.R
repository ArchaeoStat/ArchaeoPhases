# TEST
#' @include AllClasses.R AllGenerics.R
NULL

# Apportion ====================================================================
#' @export
#' @rdname apportion
#' @aliases apportion,EventsMCMC,numeric,numeric-method
setMethod(
  f = "apportion",
  signature = c(object = "EventsMCMC", from = "numeric", to = "numeric"),
  definition = function(object, from, to, groups = NULL) {
    ## Validation
    n <- length(from)
    if (n != length(to)) {
      msg <- sprintf("%s must be of length %d; not %d.",
                     sQuote("to"), n, length(to))
      stop(msg, call. = FALSE)
    }
    if (is_BP(object) && any(to - from >= 0) ||
        is_CE(object) && any(to - from <= 0)) {
      msg <- ""
      stop(msg, call. = FALSE)
    }
    if (!is.null(groups)) {
      if (n != length(groups)) {
        msg <- sprintf("%s must be of length %d; not %d.",
                       sQuote("groups"), n, length(groups))
        stop(msg, call. = FALSE)
      }
    } else {
      groups <- paste(from, to, sep = "_")
    }

    ## Grouping
    ## (preserve original ordering)
    lvl <- unique(groups)
    grp <- factor(groups, levels = lvl, exclude = NULL)
    spl <- split(seq_along(from), f = grp)

    ## Calendar scale
    fun <- switch(
      get_calendar(object),
      BP = function(x, a, b) which(x < a & x >= b),
      CE = function(x, a, b) which(x >= a & x < b),
      stop("Unknown calendar scale.", call. = FALSE)
    )

    ## Matrix of results
    n_dates <- ncol(object)
    n_group <- length(spl)
    prob <- matrix(data = 0, nrow = n_dates, ncol = n_group)
    colnames(prob) <- lvl
    rownames(prob) <- colnames(object)

    for (i in seq_len(n_dates)) {
      dates <- object[, i]
      for (j in seq_len(n_group)) {
        s <- spl[[j]]
        tmp <- vapply(
          X = s,
          FUN = function(x, dates, from, to) {
            index <- fun(dates, from[x], to[x])
            length(index)
          },
          FUN.VALUE = numeric(1),
          dates = dates,
          from = from,
          to = to
        )
        prob[i, j] <- sum(tmp)
      }
    }

    prob / nrow(object)
  }
)

#' @export
#' @rdname apportion
#' @aliases apportion,EventsMCMC,list,missing-method
setMethod(
  f = "apportion",
  signature = c(object = "EventsMCMC", from = "list", to = "missing"),
  definition = function(object, from, groups = NULL) {
    ## Validation
    k <- match(c("from", "to"), names(from))
    if (anyNA(k)) {
      msg <- sprintf("%s is a list, but does not have components %s and %s.",
                     sQuote("from"), sQuote("from"), sQuote("to"))
      stop(msg, call. = FALSE)
    }
    g <- groups
    if (!is.null(g) && length(g) == 1) {
      g <- from[[g]]
      if (is.null(g)) {
        msg <- sprintf("%s is a list, but does not have component %s.",
                       sQuote("from"), sQuote(groups))
        stop(msg, call. = FALSE)
      }
    }

    methods::callGeneric(object = object, from = from$from, to = from$to,
                         groups = g)
  }
)

# Hiatus =======================================================================
#' @export
#' @describeIn hiatus Returns a length-three [`numeric`] vector (upper and upper
#'  boundaries, and hiatus duration, if any).
#' @aliases hiatus,numeric,numeric-method
setMethod(
  f = "hiatus",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, level = 0.95) {
    ## Validation
    arkhe::assert_length(y, length(x))

    no_hiatus <- c(lower = NA, upper = NA)

    gamma <- mean(x < y)
    if (gamma < level) return(no_hiatus)

    ind <- which(x < y)
    epsilon <- seq(0, 1 - level / gamma, .001)
    p <- gap(epsilon, x[ind], y[ind], level / gamma)

    ## Compute the length of all intervals
    d <- p[2, ] - p[1, ]
    dd <- d[d > 0]

    if (length(dd) < 1) return(no_hiatus)

    i <- which(d == max(dd))
    endpoints <- p[, i]

    if (p[2, i] == p[1, i]) return(no_hiatus)

    inf <- endpoints[[1]]
    sup <- endpoints[[2]]
    c(lower = inf, upper = sup)
  }
)

#' @export
#' @describeIn hiatus Hiatus between successive events.
#' @aliases hiatus,EventsMCMC-method
setMethod(
  f = "hiatus",
  signature = c(x = "EventsMCMC", y = "missing"),
  definition = function(x, y) {
    ## Get phases
    n <- ncol(x)
    z <- names(x)

    ## Matrix of results
    lower <- upper <- event <- matrix(nrow = n, ncol = n, dimnames = list(z, z))

    for (i in 1:n) {
      for (j in 1:n) {
        if (i != j) {
          h <- hiatus(x[, i], x[, j])
          lower[i, j] <- h["lower"]
          upper[i, j] <- h["upper"]
        }
        event[i, j] <- paste(z[i], z[j], sep = "-")
      }
    }

    ## Check calendar
    BP <- is_BP(x) || is_b2k(x)
    ## Reverse boundaries if BP or b2k scale
    if (BP) {
      lower <- t(lower)
      upper <- t(upper)
    }

    .TimeRange(
      lower = if (BP) upper else lower,
      upper = if (BP) lower else upper,
      names = event,
      calendar = get_calendar(x),
      hash = get_hash(x)
    )
  }
)

# Interpolate ==================================================================
#' @export
#' @rdname interpolate
#' @aliases interpolate,numeric,numeric-method
setMethod(
  f = "interpolate",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    arkhe::assert_length(y, length(x))

    n <- length(x)
    stats::runif(n, x, y)
  }
)

#' @export
#' @rdname interpolate
#' @aliases interpolate,EventsMCMC,missing-method
setMethod(
  f = "interpolate",
  signature = c(x = "EventsMCMC", y = "missing"),
  definition = function(x, e1 = 1, e2 = 2) {
    a <- x[, e1]
    b <- x[, e2]
    e <- interpolate(x = a, y = b)

    ## Build names
    eve <- names(x)
    eve <- c(eve[[e1]], paste(eve[[e1]], eve[[e2]], sep = "-"), eve[[e2]])

    ## Return an MCM object
    .EventsMCMC(
      cbind(a, e, b),
      events = eve,
      calendar = get_calendar(x),
      hash = get_hash(x)
    )
  }
)

# Older ========================================================================
#' @export
#' @describeIn older Returns a length-one [`numeric`] vector (the posterior
#'  probability of the assumption: "event `x` is older than event `y`").
#' @aliases older,numeric,numeric-method
setMethod(
  f = "older",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    arkhe::assert_length(y, length(x))

    ## Bayesian test: x < y
    mean(x < y)
  }
)

#' @export
#' @describeIn older Returns a [`numeric`] matrix of posterior probabilities.
#' @aliases older,EventsMCMC-method
setMethod(
  f = "older",
  signature = c(x = "EventsMCMC", y = "missing"),
  definition = function(x, y) {
    n <- ncol(x)
    z <- matrix(nrow = n, ncol = n)
    dimnames(z) <- list(names(x), names(x))
    for (i in 1:n) {
      for (j in 1:n) {
        z[i, j] <- older(x[, i], x[, j])
      }
    }
    z
  }
)
