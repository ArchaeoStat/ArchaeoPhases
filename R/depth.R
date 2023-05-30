# AGE-DEPTH MODELING
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname bury
#' @aliases bury,EventsMCMC-method
setMethod(
  f = "bury",
  signature = c("EventsMCMC", "numeric"),
  definition = function(object, depth) {
    ## Validation
    arkhe::assert_length(depth, ncol(object))
    arkhe::assert_unique(depth)

    ## Reorder data
    index <- order(depth)
    object <- object[, index]

    curve <- apply(
      X = object,
      MARGIN = 1,
      FUN = function(x, depth) {
        dt <- data.frame(y = x, x = depth)
        stats::loess(y ~ x, data = dt, degree = 1)
      },
      depth = depth,
      simplify = FALSE
    )

    .AgeDepthModel(
      depth = depth,
      model = curve,
      hash = get_hash(object)
    )
  }
)

#' @export
#' @rdname bury
#' @aliases bury,AgeDepthModel-method
setMethod(
  f = "predict",
  signature = c("AgeDepthModel"),
  definition = function(object, newdata) {
    if (missing(newdata)) {
      newdata <- object@depth
    }

    age <- object@model
    a <- vapply(
      X = age,
      FUN = stats::predict,
      FUN.VALUE = numeric(length(newdata)),
      newdata = newdata
    )

    ## Event names
    event_names <- names(newdata)
    if (is.null(event_names)) event_names <- paste0("E", seq_along(newdata))

    .EventsMCMC(
      t(a),
      labels = event_names,
      depth = newdata,
      hash = get_hash(object)
    )
  }
)
