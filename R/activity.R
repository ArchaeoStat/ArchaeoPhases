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
                        grid = getOption("ArchaeoPhases.grid")) {
    ## Tempo
    tmp <- tempo(object, level = 0.95, count = FALSE, credible = FALSE,
                 gauss = FALSE, from = from, to = to, grid = grid)
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

    years <- aion::time(object, calendar = NULL)
    est <- apply(
      X = object,
      MARGIN = 2,
      FUN = function(a, b) {
        diff(a) / diff(b)
      },
      b = years
    )

    ts <- aion::series(est, time = aion::as_fixed(years[-1]))
    .ActivityEvents(
      ts,
      hash = get_hash(object)
    )
  }
)
