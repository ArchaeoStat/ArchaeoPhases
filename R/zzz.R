.onLoad <- function(libname, pkgname) {
  op <- options()
  op.ArchaeoPhases <- list(
    ArchaeoPhases.calendar = chronos::CE(),
    ArchaeoPhases.grid = 512,
    ArchaeoPhases.precision = 0,
    ArchaeoPhases.progress = interactive()
  )
  toset <- !(names(op.ArchaeoPhases) %in% names(op))
  if(any(toset)) options(op.ArchaeoPhases[toset])

  invisible()
}
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste('ArchaeoPhases v2.0 brings a comprehensive package rewrite, ',
          'resulting in the renaming of nearly all functions.',
          'For further details, please consult the changelog.', sep = "\n")
  )
}
