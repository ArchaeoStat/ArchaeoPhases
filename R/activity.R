# ACTIVITY PLOT
#' @include AllGenerics.R
NULL

#' @export
#' @rdname activity
#' @aliases activity,EventsMCMC-method
setMethod(
  f = "activity",
  signature = "EventsMCMC",
  definition = function(object, from = min(object), to = max(object),
                        resolution = NULL) {
    ## Tempo
    tmp <- tempo(object, level = 0.95, count = FALSE, credible = FALSE,
                 gauss = FALSE, from = from, to = to, resolution = resolution)
    ## Activity
    methods::callGeneric(object = tmp)
  }
)

#' @export
#' @rdname activity
#' @aliases activity,CumulativeEvents-method
setMethod(
  f = "activity",
  signature = "CumulativeEvents",
  definition = function(object) {
    ## Validation
    if (object@counts) {
      stop("Tempo must be computed as probabilities.", call. = FALSE)
    }

    a <- object@estimate
    b <- object@years

    x <- b[-1]
    y <- diff(a) / diff(b)

    ## Calendar scale
    if (is_BP(object)) {
      y <- max(y) - y
    }

    .ActivityEvents(
      years = x,
      estimate = y,
      calendar = get_calendar(object),
      hash = get_hash(object)
    )
  }
)
