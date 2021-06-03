# TEST
#' @include AllClasses.R AllGenerics.R
NULL

# Apportion ====================================================================
#' @export
#' @rdname apportion
#' @aliases apportion,MCMC,numeric,numeric-method
setMethod(
  f = "apportion",
  signature = c(object = "MCMC", from = "numeric", to = "numeric"),
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
#' @aliases apportion,MCMC,list,missing-method
setMethod(
  f = "apportion",
  signature = c(object = "MCMC", from = "list", to = "missing"),
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

# Older ========================================================================
#' @export
#' @rdname test_older
#' @aliases test_older,MCMC-method
setMethod(
  f = "test_older",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, ...) {
    ## Bayesian test: x < y
    mean(x < y)
  }
)
