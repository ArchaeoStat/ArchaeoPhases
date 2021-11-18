#' Extract phase names from MCMC csv file column headers.
#'
#' Extract two phase names from four MCMC csv file column headers, checking that
#' each pair refers to a single phase.  Stops with an error if the names in a
#' pair don't refer to the same phase.
#'
#' @param names A vector of four MCMC csv file column headers.
#'
#' @return A list of two phase names.
#'
#' @author Thomas S. Dye
#'
allen.check.names <- function(names) {
    first.name <- strsplit(names[1], " ")
    first.name <- first.name[[1]][-length(first.name[[1]])]
    second.name <- strsplit(names[2], " ")
    second.name <- second.name[[1]][-length(second.name[[1]])]
    if (!identical(first.name, second.name))
        stop(sprintf("'%s' and '%s' do not match", first.name, second.name))
    third.name <- strsplit(names[3], " ")
    third.name <- third.name[[1]][-length(third.name[[1]])]
    fourth.name <- strsplit(names[4], " ")
    fourth.name <- fourth.name[[1]][-length(fourth.name[[1]])]
    if (!identical(third.name, fourth.name))
        stop(sprintf("'%s' and '%s' do not match", third.name, fourth.name))
    list(first = paste(first.name, collapse = " "), second = paste(third.name, collapse = " "))
}

#' Check if an argument is odd.
#'
#' Utility function that checks if its numeric argument is odd.
#' Stops with an error if the argument is non-numeric.
#'
#' @param x An integer.
#'
#' @return Boolean
is.odd <- function(x) {
    if (!is.numeric(x))
        stop("non-numeric argument") else x%%2 != 0
}

#' Allen relation of two definite intervals
#'
#' Calculates the Allen relation of two definite intervals and
#' reports the one-letter code for the interval proposed by Thomas Alspaugh.
#' Stops with an error if the end of an interval is earlier than its start.
#'
#' @param start.1 The start date of the first interval
#' @param end.1 The end date of the first interval
#' @param start.2 The start date of the second interval
#' @param end.2 The end date of the second interval
#'
#' @return A one-letter code indicating the Allen relation
#'
#' @author Thomas S. Dye
allen.relation <- function(start.1, end.1, start.2, end.2) {
    start.1 <- unlist(start.1)
    end.1 <- unlist(end.1)
    start.2 <- unlist(start.2)
    end.2 <- unlist(end.2)
    if (!is.numeric(c(start.1, end.1, start.2, end.2)))
        stop("arguments must be numeric")
    if ((end.1 < start.1) || (end.2 < start.2))
        stop("beta is older than alpha") else {
        result <- if (start.1 < start.2) {
            if (end.1 < start.2)
                "p" else if (end.1 == start.2)
                "m" else if (end.1 < end.2)
                "o" else if (end.1 == end.2)
                "F" else "D"
        } else if (start.1 == start.2) {
            if (end.1 > end.2)
                "S" else if (end.1 == end.2)
                "e" else "s"
        } else if (start.1 > start.2) {
            if (start.1 < end.2) {
                if (end.1 < end.2)
                  "d" else if (end.1 == end.2)
                  "f" else "O"
            } else if (start.1 == end.2)
                "M" else "P"
        }
        result
    }
}

#' Relationship string from code
#'
#' Return a string that describes the relationship indicated by a one-letter code.
#' Stops with an error if the code is not recognized.
#'
#' @param code A one-letter code
#'
#' @return A string
#'
#' @author Thomas S. Dye
#'
allen_code_to_string <- function(code) {
    switch(code, p = "precedes", m = "meets", o = "overlaps", F = "finished by",
        D = "contains", s = "starts", e = "equals", S = "started by", d = "during",
        f = "finishes", O = "overlapped by", M = "met by", P = "preceded by", stop("unrecognized code"))
}

#' Convert a string containing Allen relation codes to a relation set
#'
#' Characters in the string that are not Allen relation codes are
#' not identified and are added to the set.
#'
#' @param s A string with Allen relation codes.
#'
#' @return A vector of single letter Allen relation codes.
#'
#' @author Thomas S. Dye
#'
#' @export
allen.string.to.set <- function(s) {
    res <- strsplit(s, "")
    unlist(res)
}

#' Convert an Allen relation set to a named vector
#'
#' Set elements that are not Allen relation codes are silently ignored.
#'
#' @param s An Allen relation set, a vector of single letter codes.
#'
#' @return A named result vector.
#'
#' @author Thomas S. Dye
#'
#' @export
allen.set.to.vector <- function(s) {
    res <- allen.create.result.vector()
    for (x in 1:length(s)) {
        res <- allen.update.result(res, s[x])
    }
    res
}

#' Convert a string containing Allen relation codes to a result vector
#'
#' A result vector is named with Allen relation codes and contains counts of
#' observed relations.
#'
#' @param s A string with Allen relation codes
#'
#' @return A named result vector
#'
#' @author Thomas S. Dye
#'
#' @export
allen.string.to.vector <- function(s) {
    s.set <- allen.string.to.set(s)
    res <- allen.set.to.vector(s.set)
    res
}

#' Order Allen relations in a string representing an Allen set
#'
#' @param s A string representing an Allen set
#'
#' @return A string ordered conventionally
#'
#' @author Thomas S. Dye
allen.order.string <- function(s) {
    ret <- allen.string.to.vector(s)
    ret <- allen.string.from.result(ret)
    ret
}

#' Nokel lattice y coordinates
#'
#' A vector of arbitrary coordinates for lattice node placement
#'
#' @return A vector of integers
#'
#' @author Thomas S. Dye
#'
allen_lattice_y <- function() {
    y <- c(8, 7, 6, 5, 5, 4, 4, 4, 3, 3, 2, 1, 0)
}

#' Nokel lattice x coordinates
#'
#' A vector of arbitrary coordinates for lattice node placement
#'
#' @return A vector of integers
#'
#' @author Thomas S. Dye
#'
allen_lattice_x <- function() {
    x <- c(0, 0, 0, -1, 1, -2, 0, 2, -1, 1, 0, 0, 0)
    x
}
