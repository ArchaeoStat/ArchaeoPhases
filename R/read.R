# READ
#' @include AllGenerics.R
NULL

#' @export
#' @rdname check
#' @aliases is_original,MCMC-method
setMethod(
  f = "is_original",
  signature = "MCMC",
  definition = function(object, file, ...) {
    ## Calculate hash, if connection, save temp file
    make_hash(file) == object@hash
  }
)

#' @export
#' @rdname read
#' @aliases read_oxcal,character-method
setMethod(
  f = "read_oxcal",
  signature = "character",
  definition = function(file, BP = FALSE) {
    ## OxCal hard codes csv file conventions, per C. Bronk Ramsey
    ## These match the R defaults
    data <- utils::read.table(file = file, header = TRUE, sep = ",",
                              quote = "\"", dec = ".", comment.char = "#")

    ## Calculate hash
    file_hash <- make_hash(file)

    ## Remove the iteration column
    data <- data[, -1]

    ## OxCal uses trailing commas in MCMC output,
    ## so trim the last column, which is empty
    if (!is.numeric(data[, ncol(data), drop = TRUE]))
      data <- data[, -ncol(data)]

    ## Convert from BP to BC/AD
    if (BP)
      data <- BP_to_BCAD(data)

    ## Return an MCM object
    .MCMC(as.matrix(data), calendar = "BCAD", hash = file_hash)
  }
)

#' @export
#' @rdname read
#' @aliases read_bcal,character-method
setMethod(
  f = "read_bcal",
  signature = "character",
  definition = function(file, bin_width = 1) {
    ## BCal uses English locale csv conventions
    data <- utils::read.table(file = file, header = TRUE, sep = ",",
                              quote = "\"", dec = ".", comment.char = "#")

    ## Calculate hash
    file_hash <- make_hash(file)

    ## Remove the iteration column
    data <- data[, -1]

    ## Remove an empty last column, if present
    if (!is.numeric(data[, ncol(data), drop = TRUE]))
      data <- data[, -ncol(data)]

    ## BCal used to add an empty row at the end, check if empty and remove
    if (anyNA(data[nrow(data), ]))
      data <- data[-nrow(data), ]

    ## Take bin width into account, if necessary
    if (bin_width != 1)
      data <- bin_width * data

    ## Convert from BP to BC/AD
    data <- BP_to_BCAD(data)

    ## Return BCalMCM object
    .MCMC(as.matrix(data), calendar = "BCAD", hash = file_hash)
  }
)

#' @export
#' @rdname read
#' @aliases read_chronomodel,character-method
setMethod(
  f = "read_chronomodel",
  signature = "character",
  definition = function(file, BP = FALSE, sep = ",", dec = ".") {
    ## ChronoModel allows the user to choose any separator
    ## and either a period or comma for decimals
    data <- utils::read.table(file = file, header = TRUE, sep = sep,
                              quote = "\"", dec = dec, comment.char = "#")

    ## Calculate hash
    file_hash <- make_hash(file)

    ## Remove the iteration column
    data <- data[, -1]

    ## Convert from BP to BC/AD
    if (BP)
      data <- BP_to_BCAD(data)

    ## Return ChronoModelMCMC object
    .MCMC(as.matrix(data), calendar = "BCAD", hash = file_hash)
  }
)
