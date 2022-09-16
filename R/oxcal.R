# OXCAL
#' @include AllClasses.R AllGenerics.R
NULL

#' Download OxCal
#'
#' @param path A [`character`] string specifying the directory to extract files
#'  to. It will be created if necessary (see [utils::unzip()]).
#' @note
#'  Adapted from [oxcAAR::quickSetupOxcal()].
#' @return Invisibly returns `path`.
#' @author N. Frerebeau
#' @family OxCal tools
#' @export
oxcal_download <- function(path = NULL) {
  ## Set path
  if (is.null(path)) path <- tempdir()
  tmp <- tempfile()
  on.exit(unlink(tmp))

  ## Download
  url <- "https://c14.arch.ox.ac.uk/OxCalDistribution.zip"
  utils::download.file(url, destfile = tmp, quiet = getOption("chronos.progress"))

  ## Extract
  utils::unzip(tmp, exdir = path)
  msg <- sprintf("OxCal successfully downloaded and extracted to %s.", path)
  message(msg)

  invisible(path)
}

#' Setup OxCal
#'
#' @param path A [`character`] string specifying the directory where to find the
#'  OxCal executable (or to extract OxCal files to).
#' @param ask A [`logical`] scalar: if OxCal is not installed, should the user
#'  be asked to download it?
#'  If `FALSE` and the OxCal executable cannot be found, will raise an error.
#'  Only used if \R is being used interactively.
#' @details
#'  Downloads the latest version of Oxcal (if needed) and sets the executable
#'  path correctly.
#' @note
#'  Adapted from [oxcAAR::quickSetupOxcal()].
#' @return Invisibly returns the path to the OxCal executable.
#' @author N. Frerebeau
#' @family OxCal tools
#' @export
oxcal_setup <- function(path = NULL, ask = TRUE) {
  ## Set path
  if (is.null(path)) path <- tempdir()

  ## Construct the executable path
  operator <- Sys.info()["sysname"]
  binary <- switch(
    operator,
    Linux = "OxCalLinux",
    Windows = "OxCalWin.exe",
    Darwin = "OxCalMac",
    stop(sprintf("Unknown operating system: %s.",
                 sQuote(operator)), call. = FALSE)
  )
  full_path <- file.path(path, "OxCal", "bin", binary)

  ## Validation
  if (!file.exists(full_path)) {
    download <- ""
    if (ask && interactive()) {
      cat(
        "OxCal doesn't seem to be installed.",
        "Do you want to download it?",
        "1. Yes",
        "2. No",
        sep = "\n"
      )
      download <- readline("Choice: ")
    }
    if (download != "1") {
      stop(sprintf("There is no such file: %s.", full_path), call. = FALSE)
    }
    oxcal_download(path)
  }

  ## Set option
  Sys.chmod(full_path, mode = "0777")
  options(chronos.oxcal = full_path)

  invisible(full_path)
}

oxcal_path <- function() {
  path <- getOption("chronos.oxcal")
  if (is.null(path) || path == "") {
    stop("Please set the path to OxCal executable.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("OxCal doesn't seem to be installed.", call. = FALSE)
  }
  path
}

#' Execute an Oxcal Script
#'
#' @param script A [`character`] string of instructions for OxCal.
#' @param file A [`character`] string naming a file (without extension) to
#'  write `script` to. Output files will be named after `file` and written to
#'  the same directory.
#' @param output A [`character`] string specifying the file extension to be
#'  returned (see below). It must be one of "`js`", "`log`", "`txt`" or "`csv`".
#' @param timeout An [`integer`] value specifying the timeout in seconds,
#'  ignored if 0. This is a limit for the elapsed time running OxCal.
#'  Fractions of seconds are ignored.
#' @return Invisibly returns the path to the `output` file.
#' @references
#'  \url{https://c14.arch.ox.ac.uk/oxcalhelp/hlp_analysis_file.html}
#' @author N. Frerebeau
#' @family OxCal tools
#' @export
oxcal_execute <- function(script, file = NULL,
                          output = c("js", "log", "txt", "csv"), timeout = 0) {
  ## Validation
  output <- match.arg(output, several.ok = FALSE)

  ## Get OxCal path
  path <- oxcal_path()

  ## Construct output path
  if (is.null(file)) {
    file <- tempfile()
  } else {
    direct <- dirname(file)
    dir.create(direct, showWarnings = FALSE, recursive = TRUE)
  }

  ## Remove existing .work file (if any)
  work <- sprintf("%s.work", file)
  if (file.exists(work)) file.remove(work)

  ## Write script
  oxcal <- sprintf("%s.oxcal", file)
  cat(script, file = oxcal)

  ## Run OxCal
  out <- suppressWarnings(system2(path, oxcal, wait = TRUE, timeout = timeout))
  if (out == 127L) {
    stop("Something goes wrong...", call. = FALSE)
  }
  if (out == 124L) {
    stop(sprintf("Command terminated after %gs.", timeout), call. = FALSE)
  }

  ## Output file
  output <- sprintf("%s.%s", file, output)
  if (!file.exists(output)) {
    warning(sprintf("%s does not exist.", output), call. = FALSE)
  }
  invisible(output)
}

#' 14C Calibration with OxCal
#'
#' @param names A [`character`] vector specifying the names of the dates (e.g.
#'  laboratory codes).
#' @param dates A [`numeric`] vector giving the BP dates to be calibrated.
#' @param errors A [`numeric`] vector giving the standard deviation of the dates
#'  to be calibrated.
#' @param curve A [`character`] string specifying the calibration curve to be
#'  used.
#' @author N. Frerebeau
#' @family OxCal tools
#' @export
oxcal_calibrate <- function(names, dates, errors, curve = "IntCal20") {
  ## Validation
  n <- length(names)
  if (length(dates) != n || length(errors) != n) {
    msg <- sprintf("%s, %s and %s must have the same lenght.",
                   sQuote("names"), sQuote("dates"), sQuote("errors"))
    stop(msg, call. = FALSE)
  }
  if (length(curve) != 1) {
    stop("Please select one calibration curve.", call. = FALSE)
  }

  ## OxCal options
  curve <- tolower(curve)
  opt <- sprintf("Options()\n{\nCurve=\"%s.14c\"\n};", curve)

  ## R_Dates
  r_dates <- sprintf("R_Date(\"%s\",%g,%g);", names, dates, errors)
  r_dates <- paste0(r_dates, collapse = "\n")

  ## Execute OxCal
  script <- paste0(c(opt, r_dates), collapse = "\n")
  out <- oxcal_execute(script, output = "js")
  out
}
