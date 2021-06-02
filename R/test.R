# TEST
#' @include AllClasses.R AllGenerics.R
NULL

# Apportion ====================================================================
#' @export
#' @rdname apportion
#' @aliases apportion,CumulativeEvents,numeric,numeric-method
setMethod(
  f = "apportion",
  signature = c(object = "CumulativeEvents", from = "numeric", to = "numeric"),
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

    ## Get data
    year <- object@year
    est <- object@estimate

    ## Calendar scale
    fun <- switch(
      get_calendar(object),
      BP = function(x, a, b) which(x < a & x >= b),
      CE = function(x, a, b) which(x >= a & x < b),
      stop("Unknown calendar scale.", call. = FALSE)
    )

    prob <- vector(mode = "integer", length = n)
    for (i in seq_len(n)) {
      index <- fun(year, from[i], to[i])

      if (length(index) != 0) {
        k <- range(index)
        # FIXME: if (diff(k) == 0) do something?
        prob[i] <- abs(diff(est[k]))
      } else {
        prob[i] <- NA_integer_
      }
    }

    ## Remove missing values
    no_na <- !is.na(prob)
    prob <- prob[no_na]
    groups <- groups[no_na]

    tapply(X = prob, INDEX = groups, FUN = sum, na.rm = TRUE)
  }
)

#' @export
#' @rdname apportion
#' @aliases apportion,CumulativeEvents,list,missing-method
setMethod(
  f = "apportion",
  signature = c(object = "CumulativeEvents", from = "list", to = "missing"),
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
