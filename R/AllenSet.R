#' The basic Allen relation set
#'
#' A vector of one-letter codes for the thirteen basic Allen relations.
#' The codes were proposed by Thomas Alspaugh.
#'
#' @return A vector of thirteen one-letter codes
#'
#' @author Thomas S. Dye
#'
allen.basic.relation.set <- function() {
    ret <- c("p", "m", "o", "F", "s", "D", "e", "d", "S", "f", "O", "M", "P")
    ret
}

#' Allen basic relation set as strings
#'
#' String descriptors of the Allen basic relations.
#'
#' @return A vector of thirteen strings
#'
#' @author Thomas S. Dye
#'
allen_basic_relation_strings <- function() {
    ret <- c(p = "precedes", m = "meets", o = "overlaps", F = "finished by", s = "starts",
        D = "contains", e = "equals", d = "during", S = "started by", f = "finishes",
        O = "overlapped by", M = "met by", P = "preceded by")
    ret
}

#' Allen concurrent relation set
#'
#' A vector of nine one-letter codes for the Allen concurrent relations.
#' The codes were proposed by Thomas Alspaugh.
#'
#' @return A vector of nine one-letter codes.
#'
allen.concurrent.relation.set <- function() {
    ret <- c("o", "F", "D", "s", "e", "S", "d", "f", "O")
    ret
}

#' Allen basic relation set complement
#'
#' Returns the complement of the set passed as an argument.
#'
#' @param allen.set An Allen set or result vector
#'
#' @return An Allen set, a vector of one-letter codes.
#'
#' @author Thomas S. Dye
#'
allen.complement.set <- function(allen.set) {
    result.set <- ensure.allen.set.vector(allen.set)
    allen.full.set <- allen.basic.relation.set()
    ret <- setdiff(allen.full.set, result.set)
    ret
}

#' Intersection of Allen relation sets
#'
#' Calculates the intersection of two Allen relation sets.  Attempts to
#' handle sets represented by result vectors, as well as sets represented
#' by vectors of one-letter codes.
#'
#' @param allen.set.1 An Allen set
#' @param allen.set.2 An Allen set
#'
#' @return An Allen relation set
#'
#' @author Thomas S. Dye
#'
allen.relations.intersection <- function(allen.set.1, allen.set.2) {
    result.set.1 <- ensure.allen.set.vector(allen.set.1)
    result.set.2 <- ensure.allen.set.vector(allen.set.2)
    ret <- intersect(result.set.1, result.set.2)
    ret
}

#' The Allen relation set represented by the non-zero elements of a result vector
#'
#' Given a result vector, returns a vector of the one-letter codes corresponding to
#' the non-zero elements of the result vector.
#'
#' @param result.vector A result vector
#'
#' @return An Allen set
#'
#' @author Thomas S. Dye
#'
allen.set.vector.from.result <- function(result.vector) {
    if (!is.result.vector(result.vector))
        stop("Cannot convert an object that is not a result vector.")
    ret <- names(result.vector[result.vector != 0])
    ret
}

#' String representation of an Allen set from a result vector
#'
#' Given a result vector, return a string corresponding to the non-zero
#' elements of the result vector
#'
#' @param result.vector A result vector
#'
#' @return A string representing an Allen set
#'
#' @author Thomas S. Dye
#'
allen.string.from.result <- function(result.vector) {
    if (!is.result.vector(result.vector))
        stop("Cannot convert an object that is not a result vector.")
    ret <- names(result.vector[result.vector != 0])
    ret <- paste(ret, collapse = "")
    ret
}

#' Coerce a string to an Allen relation set.
#'
#' Coerces a string to a vector representing an Allen relation set.
#' Does not check if the string represents an Allen set correctly.
#'
#' @param allen.set.string A string whose elements are expected to be one-letter
#' codes for Allen relations following the convention proposed by Thomas
#' Alspaugh.
#'
#' @author Thomas S. Dye
#'
allen.set.vector <- function(allen.set.string) {
    if (!is.set.string)
        stop("Cannot convert an object that is not a string representation of an Allen set.")
    ret <- unlist(strsplit(allen.set.string, ""))
    ret
}

#' Union of two Allen relation sets.
#'
#' Returns the union of two Allen relation sets, taking care to handle
#' empty sets and the sets represented by result vectors.
#'
#' @param allen.set.1 The first Allen relation set or result vector
#' @param allen.set.2 The second Allen relation set or result vector
#'
#' @return An Allen relation set
#'
#' @author Thomas S. Dye
#'
allen.relations.union <- function(allen.set.1, allen.set.2) {
    result.set.1 <- ensure.allen.set.vector(allen.set.1)
    result.set.2 <- ensure.allen.set.vector(allen.set.2)
    ret <- union(result.set.1, result.set.2)
    ret
}

#' Allen relation set converse.
#'
#' Calculates the converse of an Allen relation set, taking care to
#' convert a result vector to a relation set.
#'
#' @param allen.set An Allen relation set or result vector
#'
#' @return An Allen relation set
#'
#' @author Thomas S. Dye
#'
allen.converse.set <- function(allen.set) {
    allen.converse <- function(relation) {
        result <- switch(relation, p = "P", m = "M", o = "O", F = "f", D = "d", s = "S",
            e = "e", S = "s", d = "D", f = "F", O = "o", M = "m", P = "p", `NULL` = NULL,
            stop("unrecognized Allen relation identifier"))
        result
    }
    result.set <- ensure.allen.set.vector(allen.set)
    if (is.null(result.set))
        converse.set <- NULL else converse.set <- sapply(result.set, allen.converse)
    converse.set
}

#' Allen relation set for intervals with distinct endpoints.
#'
#' Return the six value Allen relation set for intervals with distinct
#' endpoints.
#'
#' @return An Allen relation set
#'
#' @author Thomas S. Dye
#'
allen.six.value.set <- function() {
    ret <- c("p", "o", "D", "d", "O", "P")
    ret
}

#' Ensure an Allen set is represented as a vector of single character strings
#'
#' Expects a string, set vector, or result vector and will stop with an
#' error if something else is encountered.
#'
#' @param obj An Allen set represented as a string, a set vector, or a
#' result vector.
#'
#' @return An Allen set represented as a set vector.
#'
#' @author Thomas S. Dye
#'
ensure.allen.set.vector <- function(obj) {
    if (is.set.vector(obj))
        result.set <- obj else if (is.set.string(obj))
        result.set <- allen.string.to.set(obj) else if (is.result.vector(obj))
        result.set <- allen.set.vector.from.result(obj) else stop("Unrecognized Allen set.")
    result.set
}
