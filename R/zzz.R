.onLoad <- function(libname, pkgname) {
  op <- options()
  op.ArchaeoPhases <- list(
    ArchaeoPhases.grid = 512,
    ArchaeoPhases.precision = 0,
    ArchaeoPhases.progress = interactive()
  )
  toset <- !(names(op.ArchaeoPhases) %in% names(op))
  if(any(toset)) options(op.ArchaeoPhases[toset])

  invisible()
}
