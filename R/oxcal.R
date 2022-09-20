# OXCAL
#' @include AllClasses.R AllGenerics.R
NULL

# Dowload ======================================================================
#' Download OxCal
#'
#' @param path A [`character`] string specifying the directory to extract files
#'  to. It will be created if necessary (see [utils::unzip()]).
#' @note
#'  Adapted from [oxcAAR::quickSetupOxcal()].
#' @return Invisibly returns `path`.
#' @example inst/examples/ex-oxcal.R
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

# Setup ========================================================================
#' Setup OxCal
#'
#' @param path A [`character`] string specifying the directory where to find the
#'  OxCal executable (or to extract OxCal files to).
#' @param os A [`character`] string specifying the operating system of the
#'  workstation. It must be one of "`Linux`", "`Windows`" or "`Darwin`".
#'  If `NULL` (the default), the operating system will be determined
#'  automatically (see [Sys.info()]).
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
#' @example inst/examples/ex-oxcal.R
#' @author C. Schmid, N. Frerebeau
#' @family OxCal tools
#' @export
oxcal_setup <- function(path = NULL, os = NULL, ask = TRUE) {
  ## Set path
  if (is.null(path)) path <- tempdir()

  ## Construct the executable path
  operator <- if (is.null(os)) Sys.info()["sysname"] else os
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

#' Get OxCal Executable Path
#'
#' @return Returns the path to OxCal executable.
#' @example inst/examples/ex-oxcal.R
#' @author N. Frerebeau
#' @family OxCal tools
#' @keywords internal
#' @noRd
oxcal_path <- function() {
  path <- getOption("chronos.oxcal")
  if (is.null(path) || path == "") {
    stop("Please set the path to OxCal executable.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Please fix the path to OxCal executable.", call. = FALSE)
  }
  path
}

# Execute ======================================================================
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
#'  Fractions of seconds are ignored (see [system2()]).
#' @note
#'  Adapted from [oxcAAR::executeOxcalScript()].
#' @return Invisibly returns the path to the `output` file.
#' @example inst/examples/ex-oxcal.R
#' @references
#'  \url{https://c14.arch.ox.ac.uk/oxcalhelp/hlp_analysis_file.html}
#' @author M. Hinz, N. Frerebeau
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

# Parse ========================================================================
#' Read and Parse OxCal Output
#'
#' @param file A [`character`] string naming a JavaScript file which the data
#'  are to be read from (typically returned by [oxcal_execute()]).
#' @return A [`list`] with the following elements:
#'  \describe{
#'   \item{`ocd`}{A `list` holding the ranges, probability distributions, etc.
#'   for each parameter.}
#'   \item{`model`}{A `list` containing information about the model.}
#'   \item{`calib`}{A `list` containing information about the calibration
#'   curve.}
#'  }
#' @example inst/examples/ex-oxcal.R
#' @references
#'  \url{https://c14.arch.ox.ac.uk/oxcalhelp/hlp_analysis_file.html}
#' @author N. Frerebeau
#' @family OxCal tools
#' @export
oxcal_parse <- function(file) {
  ox <- V8::v8()
  ox$eval("ocd={};")
  ox$eval("calib={};")
  ox$eval("model={};")
  ox$source(file)

  results <- list(
    ocd = ox$get("ocd"),
    model = ox$get("model"),
    calib = ox$get("calib")
  )

  structure(results, class = "OxCalOutput")
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
#' @return A [`data.frame`] with the following columns:
#'  \describe{
#'   \item{`name`}{}
#'   \item{`type`}{}
#'   \item{`date`}{}
#'   \item{`error`}{}
#'   \item{`range`}{}
#'   \item{`likelihood`}{}
#'  }
#' @example inst/examples/ex-oxcal.R
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

  ## Parse OxCal output
  res <- oxcal_parse(out)

  ## Get dates
  df <- data.frame(
    name = oxcal_get_name(res),
    type = oxcal_get_type(res),
    date = oxcal_get_bp_date(res),
    error = oxcal_get_bp_error(res),
    range = I(oxcal_get_range(res)),
    likelihood = I(oxcal_get_likelihood(res))
  )
  structure(df, class = c("OxCalCalibratedDates", "data.frame"))
}

# Print ========================================================================
#' @export
format.OxCalOutput <- function(x, ...) {
  com <- oxcal_get_comment(x)
  sep <- paste0(rep("-", length.out = getOption("width")), collapse = "")

  paste(com, sep, sep = "\n")
}

#' @export
print.OxCalOutput <- function(x, ...) cat(format(x, ...), sep = "\n")

# Getters ======================================================================
oxcal_get_name <- function(x) {
  UseMethod("oxcal_get_name")
}
oxcal_get_name.OxCalOutput <- function(x) {
  vapply(X = x$ocd[-1], FUN = `[[`, FUN.VALUE = character(1), i = "name")
}
oxcal_get_type <- function(x) {
  UseMethod("oxcal_get_type")
}
oxcal_get_type.OxCalOutput <- function(x) {
  vapply(X = x$ocd[-1], FUN = `[[`, FUN.VALUE = character(1), i = "type")
}
oxcal_get_bp_date <- function(x) {
  UseMethod("oxcal_get_bp_date")
}
oxcal_get_bp_date.OxCalOutput <- function(x) {
  vapply(X = x$ocd[-1], FUN = `[[`, FUN.VALUE = numeric(1), i = "date")
}
oxcal_get_bp_error <- function(x) {
  UseMethod("oxcal_get_bp_error")
}
oxcal_get_bp_error.OxCalOutput <- function(x) {
  vapply(X = x$ocd[-1], FUN = `[[`, FUN.VALUE = numeric(1), i = "error")
}
oxcal_get_likelihood <- function(x) {
  UseMethod("oxcal_get_likelihood")
}
oxcal_get_likelihood.OxCalOutput <- function(x) {
  lapply(
    X = x$ocd[-1],
    FUN = function(x) {
      years <- seq.int(
        from = x$likelihood$start,
        by = x$likelihood$resolution,
        length.out = length(x$likelihood$prob)
      )
      list(x = years, y = x$likelihood$prob)
    }
  )
}
oxcal_get_range <- function(x) {
  UseMethod("oxcal_get_range")
}
oxcal_get_range.OxCalOutput <- function(x) {
  lapply(X = x$ocd[-1], FUN = function(x) x$likelihood$range)
}
oxcal_get_comment <- function(x) {
  UseMethod("oxcal_get_comment")
}
oxcal_get_comment.OxCalOutput <- function(x) {
  vapply(
    X = x$ocd,
    FUN = function(x) {
      com <- x$likelihood$comment
      paste0(com, collapse = "\n")
    },
    FUN.VALUE = character(1)
  )
}
