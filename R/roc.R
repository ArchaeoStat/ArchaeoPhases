# RATE OF CHANGE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname roc
#' @aliases roc,EventsMCMC-method
setMethod(
  f = "roc",
  signature = "EventsMCMC",
  definition = function(object, from = min(object), to = max(object),
                        resolution = NULL) {
    ## Activity
    tmp <- activity(object, from = from, to = to, resolution = resolution)
    ## ROC
    methods::callGeneric(object = tmp)
  }
)

#' @export
#' @rdname roc
#' @aliases roc,CumulativeEvents-method
setMethod(
  f = "roc",
  signature = "CumulativeEvents",
  definition = function(object) {
    ## Validation
    if (object@counts) {
      stop("Tempo must be computed as probabilities.", call. = FALSE)
    }

    ## Activity
    tmp <- activity(object)

    ## ROC
    methods::callGeneric(object = tmp)
  }
)

#' @export
#' @rdname roc
#' @aliases roc,ActivityEvents-method
setMethod(
  f = "roc",
  signature = "ActivityEvents",
  definition = function(object) {
    ## Get data
    a <- object@estimate
    b <- object@years

    x <- b[-1]
    y <- diff(a) / diff(b)

    ## Calendar scale
    if (is_BP(object)) {
      y <- y * -1
    }

    .RateOfChange(
      years = x,
      estimate = y,
      calendar = get_calendar(object),
      hash = get_hash(object)
    )
  }
)
