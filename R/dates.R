# DATES
#' @include AllClasses.R AllGenerics.R
NULL

# Hiatus =======================================================================
#' @export
#' @rdname gap
#' @aliases date_hiatus,numeric,numeric-method
setMethod(
  f = "date_hiatus",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, level = 0.95) {
    ## Validation
    if (length(x) != length(y)) {
      stop(sprintf("%s and %s must have the same length.",
                   sQuote("x"), sQuote("y")), call. = FALSE)
    }

    no_hiatus <- list(
      hiatus = c(lower = NA, upper = NA),
      duration = NA,
      level = level
    )

    gamma <- mean(x < y)
    if (gamma < level) return(no_hiatus)

    ind <- which(x < y)
    epsilon <- seq(0, 1 - level, gamma)
    p <- gap(epsilon, x[ind], y[ind], level / gamma)
    ## Compute the length of all intervals
    d <- p[2, ] - p[1, ]
    dd <- d[d > 0]

    if (length(dd) < 1) return(no_hiatus)

    i <- which(d == max(dd))
    i2 <- round(p[, i], 0)

    if (p[2, i] == p[1, i]) return(no_hiatus)

    inf <- unname(i2[1])
    sup <- unname(i2[2])
    list(
      hiatus = c(lower = inf, upper = sup),
      duration = sup - inf,
      level = level
    )
  }
)
