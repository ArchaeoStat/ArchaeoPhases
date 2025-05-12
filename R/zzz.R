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
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste(tr_("Breaking changes!"),
          tr_("ArchaeoPhases v2.0 is the result of a complete code rewrite."),
          tr_("For further details, please consult the changelog."), sep = "\n")
  )
}
