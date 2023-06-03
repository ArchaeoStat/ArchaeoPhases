# READ
#' @include AllGenerics.R
NULL

# Check ========================================================================
#' @export
#' @rdname check
#' @aliases is_original,MCMC-method
setMethod(
  f = "is_original",
  signature = "MCMC",
  definition = function(object, file, download = FALSE) {
    identical(make_hash(file, download), get_hash(object))
  }
)

#' @export
#' @rdname check
#' @aliases is_original,PhasesMCMC-method
setMethod(
  f = "is_original",
  signature = "PhasesMCMC",
  definition = function(object, file, download = FALSE) {
    identical(make_hash(file, download), get_hash(object))
  }
)

#' @export
#' @rdname check
#' @aliases is_original,CumulativeEvents-method
setMethod(
  f = "is_original",
  signature = "CumulativeEvents",
  definition = function(object, file, download = FALSE) {
    identical(make_hash(file, download), get_hash(object))
  }
)

#' @export
#' @rdname check
#' @aliases is_original,ActivityEvents-method
setMethod(
  f = "is_original",
  signature = "ActivityEvents",
  definition = function(object, file, download = FALSE) {
    identical(make_hash(file, download), get_hash(object))
  }
)

#' @export
#' @rdname check
#' @aliases is_original,OccurrenceEvents-method
setMethod(
  f = "is_original",
  signature = "OccurrenceEvents",
  definition = function(object, file, download = FALSE) {
    identical(make_hash(file, download), get_hash(object))
  }
)

# Read =========================================================================
## OxCal -----------------------------------------------------------------------
#' @export
#' @rdname read_oxcal
#' @aliases read_oxcal,character-method
setMethod(
  f = "read_oxcal",
  signature = "character",
  definition = function(file, calendar = CE()) {
    ## OxCal hard codes csv file conventions, per C. Bronk Ramsey
    ## These match the R defaults
    data <- utils::read.table(file = file, header = TRUE, sep = ",",
                              quote = "\"", dec = ".", comment.char = "#",
                              check.names = FALSE)

    ## Calculate hash
    file_hash <- make_hash(file)

    ## Remove the iteration column
    iter <- data[, 1]
    data <- data[, -1, drop = FALSE]

    ## OxCal uses trailing commas in MCMC output,
    ## so trim the last column, which is empty
    if (!is.numeric(data[, ncol(data), drop = TRUE]))
      data <- data[, -ncol(data)]

    ## Fix names
    ## check.names = FALSE allows to get the original names
    ## then column names must be properly set with make.names()
    date_names <- colnames(data)
    colnames(data) <- make.names(date_names)
    if (any(duplicated(date_names)))
      warning("Duplicated names!", call. = FALSE)

    ## Coerce to vector
    d <- dim(data)
    data <- unlist(data)

    ## Convert to rata die
    data <- aion::fixed(data, calendar = calendar)
    dim(data) <- d

    ## Return an MCM object
    .EventsMCMC(
      data,
      labels = date_names,
      iteration = as.integer(iter),
      hash = file_hash
    )
  }
)

## BCal ------------------------------------------------------------------------
#' @export
#' @rdname read_bcal
#' @aliases read_bcal,character-method
setMethod(
  f = "read_bcal",
  signature = "character",
  definition = function(file, bin_width = 1, calendar = BP()) {
    ## BCal uses English locale csv conventions
    data <- utils::read.table(file = file, header = TRUE, sep = ",",
                              quote = "\"", dec = ".", comment.char = "#",
                              check.names = FALSE)

    ## Calculate hash
    file_hash <- make_hash(file)

    ## Remove the iteration column
    iter <- data[, 1]
    data <- data[, -1, drop = FALSE]

    ## Remove an empty last column, if present
    if (!is.numeric(data[, ncol(data), drop = TRUE]))
      data <- data[, -ncol(data)]

    ## Fix names
    ## check.names = FALSE allows to get the original names
    ## then column names must be properly set with make.names()
    date_names <- colnames(data)
    colnames(data) <- make.names(date_names)
    if (any(duplicated(date_names)))
      warning("Duplicated names!", call. = FALSE)

    ## BCal used to add an empty row at the end, check if empty and remove
    if (anyNA(data[nrow(data), ]))
      data <- data[-nrow(data), ]

    ## Take bin width into account, if necessary
    if (bin_width != 1)
      data <- data * bin_width

    ## Coerce to vector
    d <- dim(data)
    data <- unlist(data)

    ## Convert to rata die
    data <- aion::fixed(data, calendar = calendar)
    dim(data) <- d

    ## Return an MCM object
    .EventsMCMC(
      data,
      labels = date_names,
      iteration = as.integer(iter),
      hash = file_hash
    )
  }
)

## ChronoModel -----------------------------------------------------------------
#' @export
#' @rdname read_chronomodel
#' @aliases read_chronomodel_events,character-method
setMethod(
  f = "read_chronomodel_events",
  signature = "character",
  definition = function(file, calendar = CE(), sep = ",", dec = ".") {
    ## ChronoModel allows the user to choose any separator
    ## and either a period or comma for decimals
    data <- utils::read.table(file = file, header = TRUE, sep = sep,
                              quote = "\"", dec = dec, comment.char = "#",
                              colClasses = "numeric", check.names = FALSE)

    ## Calculate hash
    file_hash <- make_hash(file)

    ## Remove the iteration column
    iter <- data[, 1]
    data <- data[, -1, drop = FALSE]

    ## Fix names
    ## check.names = FALSE allows to get the original names
    ## then column names must be properly set with make.names()
    date_names <- colnames(data)
    colnames(data) <- make.names(date_names)
    if (any(duplicated(date_names)))
      warning("Duplicated names!", call. = FALSE)

    ## Coerce to vector
    d <- dim(data)
    data <- unlist(data)

    ## Convert to rata die
    data <- aion::fixed(data, calendar = calendar)
    dim(data) <- d

    ## Return an MCM object
    .EventsMCMC(
      data,
      labels = date_names,
      iteration = as.integer(iter),
      hash = file_hash
    )
  }
)

#' @export
#' @rdname read_chronomodel
#' @aliases read_chronomodel_phases,character-method
setMethod(
  f = "read_chronomodel_phases",
  signature = "character",
  definition = function(file, calendar = CE(), sep = ",", dec = ".") {
    ## ChronoModel allows the user to choose any separator
    ## and either a period or comma for decimals
    data <- utils::read.table(file = file, header = TRUE, sep = sep,
                              quote = "\"", dec = dec, comment.char = "#",
                              colClasses = "numeric", check.names = FALSE)
    ## Calculate hash
    file_hash <- make_hash(file)

    ## Fix names
    ## check.names = FALSE allows to get the original names
    ## then column names must be properly set with make.names()
    date_names <- colnames(data)[-1]
    colnames(data) <- make.names(date_names)
    if (any(duplicated(date_names)))
      warning("Duplicated names!", call. = FALSE)

    ## Return an MCMC object
    ## Get phase names
    pattern <- "(alpha|beta|Begin|End)"
    pha <- unique(trimws(sub(pattern, "", date_names)))
    start <- seq(from = 1L, to = ncol(data) - 1, by = 2L)

    pha <- as_phases(
      from = data,
      calendar = calendar,
      start = start,
      stop = start + 1,
      names = pha,
      iteration = 1
    )
    pha@hash <- file_hash
    pha
  }
)
