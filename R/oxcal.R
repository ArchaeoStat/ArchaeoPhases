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
  ## TODO: extract only OxCal/bin/
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

  results <- structure(results, class = "OxCalOutput")
  attr(results, "oxcal") <- results$ocd[[1]]$ref
  attr(results, "curve") <- results$calib[[1]]$ref
  results
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
#' @return A [`list`] with the following elements:
#'  \describe{
#'   \item{`ocd`}{A `list` holding the ranges, probability distributions, etc.
#'   for each parameter.}
#'   \item{`model`}{A `list` containing information about the model.}
#'   \item{`calib`}{A `list` containing information about the calibration
#'   curve.}
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
  res
}

# Plot =========================================================================
#' @export
#' @method autoplot OxCalOutput
autoplot.OxCalOutput <- function(object, ..., curve = FALSE, posterior = TRUE,
                                 decreasing = TRUE) {
  ## Get data
  oxcal <- as.data.frame(object)

  gg_curve <- NULL
  facet_dates <- NULL

  if (curve) {
    ## Intervals
    ranges <- oxcal_get_range(object, prob = "likelihood")
    ranges <- lapply(X = ranges, FUN = function(x) {
      if (is.list(x) && length(x) > 2) x[[2]] else matrix(0, nrow = 0, ncol = 3)
    })
    p <- vapply(X = ranges, FUN = nrow, FUN.VALUE = integer(1))

    ## Build a long table for ggplot2
    ranges <- as.data.frame(do.call(rbind, ranges))
    ranges$BP <- rep(oxcal$date, p)
    ranges$Date <- rep(oxcal$name, p)

    ## Dates
    aes_dates <- ggplot2::aes(x = .data$V1, xend = .data$V2,
                              y = .data$BP, yend = .data$BP,
                              group = .data$Date)
    gg_dates <- ggplot2::geom_segment(mapping = aes_dates, data = ranges,
                                      size = 2, colour = "black")

    ## Calibraion curve
    calib <- oxcal_get_curve(object)
    k <- calib$years > min(ranges$V1) & calib$years < max(ranges$V2)
    calib <- calib[k, ]

    calib <- data.frame(
      x = c(calib$years, rev(calib$years)),
      y = c(calib$bp - calib$sigma, rev(calib$bp + calib$sigma))
    )

    aes_curve <- ggplot2::aes(x = .data$x, y = .data$y)
    gg_curve <- ggplot2::geom_polygon(mapping = aes_curve, data = calib,
                                      fill = "lightgrey", colour = "darkgrey")

  } else {
    ## Likelihood
    prob <- lapply(X = oxcal$likelihood, FUN = as.data.frame)
    n <- vapply(X = prob, FUN = nrow, FUN.VALUE = integer(1))

    ## Posterior
    post <- lapply(X = oxcal$posterior, FUN = as.data.frame)
    m <- vapply(X = post, FUN = nrow, FUN.VALUE = integer(1))

    ## Build a long table for ggplot2
    prob <- do.call(rbind, prob)
    prob$ymin <- rep(oxcal$date, n)
    prob$Date <- rep(oxcal$name, n)
    prob$Distribution <- "Likelihood"

    ## Posterior
    if (posterior && any(m > 0)) {
      post <- do.call(rbind, post)
      post$ymin <- rep(oxcal$date, m)
      post$Date <- rep(oxcal$name, m)
      post$Distribution <- "Posterior"

      prob <- rbind(prob, post)
    }

    ## Order
    ref <- if (decreasing) oxcal$name else rev(oxcal$name)
    prob$Date <- factor(prob$Date, levels = ref)
    prob$ymin <- 0

    ## Dates
    aes_dates <- ggplot2::aes(x = .data$x, ymin = .data$ymin, ymax = .data$y,
                              fill = .data$Distribution)
    gg_dates <- ggplot2::geom_ribbon(mapping = aes_dates, data = prob,
                                     colour = "black", alpha = 0.5)

    ## Facets
    if (nrow(oxcal) > 1) {
      facet_dates <- ggplot2::facet_grid(rows = ggplot2::vars(.data$Date))
    }
  }

  ggplot2::ggplot() +
    gg_curve +
    gg_dates +
    facet_dates
}

#' @export
#' @method plot OxCalOutput
plot.OxCalOutput <- function(x, posterior = TRUE, curve = TRUE,
                             decreasing = TRUE, ...) {
  gg <- autoplot(object = x, ..., posterior = posterior, curve = curve,
                 decreasing = decreasing) +
    ggplot2::labs(caption = attr(x, "curve")) +
    ggplot2::scale_fill_manual(values = c("#004488", "#BB5566", "#DDAA33")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  print(gg)
  invisible(x)
}

# Print ========================================================================
# @return A [`data.frame`] with the following columns:
#  \describe{
#   \item{`name`}{}
#   \item{`type`}{}
#   \item{`date`}{}
#   \item{`error`}{}
#   \item{`likelihood`}{}
#   \item{`posterior`}{}
#  }
#' @export
#' @method as.data.frame OxCalOutput
as.data.frame.OxCalOutput <- function(x, ...) {
  data.frame(
    name = oxcal_get_name(x),
    type = oxcal_get_type(x),
    date = oxcal_get_bp_date(x),
    error = oxcal_get_bp_error(x),
    likelihood = I(oxcal_get_density(x, prob = "likelihood")),
    posterior = I(oxcal_get_density(x, prob = "posterior"))
  )
}

#' @export
format.OxCalOutput <- function(x, ...) {
  com <- oxcal_get_comment(x)
  sep <- paste0(rep("-", length.out = getOption("width")), collapse = "")

  paste(com, sep, sep = "\n")
}

#' @export
print.OxCalOutput <- function(x, ...) cat(format(x, ...), sep = "\n")

`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) != 0) x else y
}
compact <- function(f, x) {
  Filter(Negate(f), x)
}

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
  vapply(
    X = x$ocd[-1],
    FUN = function(x) x[["date"]] %||% NA_real_,
    FUN.VALUE = numeric(1)
  )
}
oxcal_get_bp_error <- function(x) {
  UseMethod("oxcal_get_bp_error")
}
oxcal_get_bp_error.OxCalOutput <- function(x) {
  vapply(
    X = x$ocd[-1],
    FUN = function(x) x[["error"]] %||% NA_real_,
    FUN.VALUE = numeric(1)
  )
}
oxcal_get_curve <- function(x) {
  UseMethod("oxcal_get_curve")
}
oxcal_get_curve.OxCalOutput <- function(x) {
  years <- seq.int(
    from = x$calib[[1]]$start,
    by = x$calib[[1]]$resolution,
    length.out = length(x$calib[[1]]$bp)
  )
  if (length(years) == 0) return(data.frame())
  data.frame(
    years = years,
    bp = x$calib[[1]]$bp,
    sigma = x$calib[[1]]$sigma
  )
}
oxcal_get_density <- function(x, ...) {
  UseMethod("oxcal_get_density")
}
oxcal_get_density.OxCalOutput <- function(x, prob = c("likelihood", "posterior")) {
  prob <- match.arg(prob, several.ok = FALSE)
  lapply(
    X = x$ocd[-1],
    FUN = function(x) {
      years <- seq.int(
        from = x[[prob]]$start,
        by = x[[prob]]$resolution,
        length.out = length(x[[prob]]$prob)
      )
      if (length(years) == 0) return(list())
      list(x = years, y = x[[prob]]$prob)
    }
  )
}
oxcal_get_range <- function(x, ...) {
  UseMethod("oxcal_get_range")
}
oxcal_get_range.OxCalOutput <- function(x, prob = c("likelihood", "posterior")) {
  prob <- match.arg(prob, several.ok = FALSE)
  lapply(X = x$ocd[-1], FUN = function(x) compact(is.null, x[[prob]]$range))
}
oxcal_get_comment <- function(x, ...) {
  UseMethod("oxcal_get_comment")
}
oxcal_get_comment.OxCalOutput <- function(x, prob = c("likelihood", "posterior")) {
  prob <- match.arg(prob, several.ok = FALSE)
  vapply(
    X = x$ocd,
    FUN = function(x) {
      com <- x[[prob]]$comment
      paste0(com, collapse = "\n")
    },
    FUN.VALUE = character(1)
  )
}


oxcal_has_likelihood <- function(x) {
  UseMethod("oxcal_has_likelihood")
}
oxcal_has_likelihood.OxCalOutput <- function(x) {
  like <- oxcal_get_density(x, prob = "likelihood")
  any(lengths(like) > 0)
}
oxcal_has_posterior <- function(x) {
  UseMethod("oxcal_has_posterior")
}
oxcal_has_posterior.OxCalOutput <- function(x) {
  post <- oxcal_get_density(x, prob = "posterior")
  any(lengths(post) > 0)
}
