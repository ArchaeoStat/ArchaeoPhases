.onLoad <- function(libname, pkgname) {
  op <- options()
  op.ArchaeoPhases <- list(
    ArchaeoPhases.precision = 0
  )
  toset <- !(names(op.ArchaeoPhases) %in% names(op))
  if(any(toset)) options(op.ArchaeoPhases[toset])

  invisible()
}
