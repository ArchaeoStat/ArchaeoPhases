.onLoad <- function(libname, pkgname) {
  op <- options()
  op.ArchaeoPhases <- list(
    ArchaeoPhases.precision = 0,
    ArchaeoPhases.progress = TRUE
  )
  toset <- !(names(op.ArchaeoPhases) %in% names(op))
  if(any(toset)) options(op.ArchaeoPhases[toset])

  invisible()
}
