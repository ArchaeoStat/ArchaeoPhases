
#' Test whether an object is a result vector
#'
#' Checks vector length, names, and class
#'
#' @param obj An object to test
#'
#' @return Boolean, TRUE if obj is a result vector, FALSE otherwise.
#'
#' @export
#'
is.result.vector <- function(obj) {
    if ((length(obj) == 13) && (names(obj) == allen.basic.relation.set()) && (class(obj) ==
        "numeric"))
        TRUE else FALSE
}

#' Test if an object is a set vector
#'
#' Checks for mode 'character', length less than 13.
#' Note: this predicate is a (very) partial implementation.
#'
#' @param obj An object to test
#'
#' @return Boolean, TRUE if obj is a set vector, FALSE otherwise.
#'
#' @author Thomas S. Dye
#'
is.set.vector <- function(obj) {
    if (identical(obj, c()))
        TRUE else if ((length(obj) <= 13 && mode(obj) == "character"))
        TRUE else FALSE
}

#' Test if an object is a set string
#'
#' Checks for mode 'character', length of 1, and nchar <= 13
#'
#' @param obj An object to test
#'
#' @return Boolean, TRUE if obj is a set string, FALSE otherwise.
#'
#' @author Thomas S. Dye
#'
is.set.string <- function(obj) {
    if ((mode(obj) == "character") && (length(obj) == 1) && (nchar(obj) <= 13))
        TRUE else FALSE
}

#' Test if an object is an illustration vector
#'
#' An illustration vector is a vector whose elements are all
#' zeros and ones.  It can be used to illustrate an Allen set.
#'
#' @param obj An object to test
#'
#' @return Boolean, TRUE if obj is an illustration vector, FALSE otherwise.
#'
#' @author Thomas S. Dye
#'
is.illustration.vector <- function(obj) {
    if (!is.vector(obj))
        stop("Expected a vector")
    test <- obj %in% c(0, 1)
    sum(test) == length(test)
}
