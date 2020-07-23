.pkgenv <- new.env(parent=emptyenv())
.onLoad <- function(libname, pkgname) {
    has_data <- requireNamespace("ArchaeoPhases.dataset", quietly = TRUE)
    .pkgenv[["has_data"]] <- has_data
}
.onAttach <- function(libname, pkgname) {
    if (!.pkgenv$has_data) {
        msg <- paste("To produce graphics for the package vignettes, you must install the",
                     "ArchaeoPhases.dataset package. To install that ",
                     "package, run `install.packages('ArchaeoPhases.dataset')'.")
        msg <- paste(strwrap(msg), collapse="\n")
        packageStartupMessage(msg)
    }
}
hasData <- function(has_data = .pkgenv$has_data) {
    if (!has_data) {
        msg <- paste("To use this function, you must have the",
                     "`ArchaeoPhases.dataset` package installed.")
        msg <- paste(strwrap(msg), collapse="\n")
        stop(msg)
    }
}
