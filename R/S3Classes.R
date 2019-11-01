#' Constructor for \code{archaeophases_mcmc} object
#'
#' Object to be returned by functions that read MCMC data from csv files.
#'
#' @param x A data frame with the data from the csv file.
#' @param mcmc_file_path A path to the csv file.
#' @param mcmc_file_hash A SHA256 hash of the csv file.
#' @param mcmc_source The Bayesian calibration application responsible
#' for the csv file, one of "bcal", "oxcal", "chronomodel".
#'
#' @return An \code{archaeophases_mcmc} object that inherits from \code{data.frame}.
#'
#' @details
#' The SHA256 hash should be secure against intentional and unintentional
#' alterations of the MCMC csv file.
#'
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}
#'
#' @seealso \code{\link{read_chronomodel}}
#' @seealso \code{\link{read_bcal}}
#' @seealso \code{\link{read_oxcal}}
#'
new_archaeophases_mcmc <- function(x = list(),
                                   call = language(),
                                   hash = character()) {

    stopifnot(is.list(x))
    structure(x,
              class = c("archaeophases_mcmc", "data.frame"),
              mcmc = call,
              hash = hash)
}

#' Constructor for \code{archaeophases_plot} object
#'
#' Objects returned by ArchaeoPhases plot functions.
#'
#' @param x A data frame with the plot data.
#'
#' @return An \code{archaeophases_plot} object that inherits from
#' \code{archaeophases_mcmc}.
#'
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}
#'
#' @seealso \code{\link{read_chronomodel}}
#' @seealso \code{\link{read_bcal}}
#' @seealso \code{\link{read_oxcal}}
new_archaeophases_plot <- function(x = list(),
                                   mcmc = list(),
                                   call = language()) {

    stopifnot(is.list(x))
    mcmc_attrs <- c("class", "mcmc", "hash")
    if (is.element("archaeophases_plot", class(mcmc))) {
        attr_list <- as.list(attributes(mcmc)$mcmc)
    }
    else {
        attr_list <- as.list(attributes(mcmc))
        }
    structure(x,
              class = c("archaeophases_plot", "archaeophases_mcmc", "data.frame"),
              mcmc = attr_list[match(mcmc_attrs, names(attr_list))],
              call = call)
}

#' Check for an original mcmc file
#'
#' Checks whether or not a file is identical to the one used to create
#' an \code{archaeophases_mcmc} object.
#'
#' @param x An \code{archaeophases_mcmc} object.
#'
#' @param file Either a path to a CSV file, a connection,
#' or the value \code{clipboard()} to read from the system clipboard.
#' The CSV \code{file} can be compressed or plain.
#'
#' @return A boolean, \code{TRUE} if the files match, \code{FALSE}
#' otherwise.
#'
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}
#'
#' @examples
#' \dontrun{
#' rem <- read_chronomodel("http://tsdye.online/AP/cm/Chain_all_Events.csv")
#' original_file(rem, "http://tsdye.online/AP/cm/Chain_all_Events.csv")
#' }
#'
#' @export
original_file <- function(x, ...) {
    UseMethod("original_file")
}

#' Check for an original mcmc file
#'
#' Checks whether or not a file is identical to the one used to create
#' an \code{archaeophases_mcmc} object.
#'
#' @param x An \code{archaeophases_mcmc} object.
#' @param file Either a path to a CSV file, a connection,
#' or the value \code{clipboard()} to read from the system clipboard.
#' The CSV \code{file} can be compressed or plain.
#'
#' @details
#' If called with a single argument, checks the file indicated by
#' the \code{file_path} attribute.
#'
#' @return A boolean, \code{TRUE} if the files match, \code{FALSE}
#' otherwise.
#'
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}
#'
#' @importFrom digest digest
#'
#' @export
original_file.archaeophases_mcmc <- function(x, file = NULL) {
    ## Calculate hash, if connection, save temp file
    if(is.null(file))
        file <- as.list(attr(x, "mcmc"))$file
    if(!file_test("-f", file)) {
        temp_file <- tempfile(pattern = "", fileext = "csv")
        write(file, temp_file)
        file_hash <- digest(file = temp_file, algo = "sha256")
        unlink(temp_file)
    }
    else {
        file_hash <- digest(file = file, algo = "sha256")
    }
    file_hash == attr(x, "hash")
}

original_file.archaeophases_plot <- function(x, file = NULL) {
    ## Calculate hash, if connection, save temp file
    ## if (is.element("archaeophases_plot", class(x))) {
    ##     m <- as.list(attr(x, "mcmc")$mcmc)
    ## }
    ## else {
        m <- as.list(attr(x, "mcmc"))
    ## }
    if(is.null(file))
        file <- m$mcmc$file
    if(!file_test("-f", file)) {
        temp_file <- tempfile(pattern = "", fileext = "csv")
        write(file, temp_file)
        file_hash <- digest(file = temp_file, algo = "sha256")
        unlink(temp_file)
    }
    else {
        file_hash <- digest(file = file, algo = "sha256")
    }
    file_hash == m$hash
}

reproduce <- function(x, ...) {
    UseMethod("reproduce")
}

#' Reproduce an MCMC data frame
#'
#' Reproduces a data frame from metadata held in an \code{archaeophases_mcmc}
#' object.
#'
#' @param x An \code{archaeophases_mcmc} object.
#'
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}
#'
#' @examples
#' \dontrun{
#' x <- read_bcal("http://tsdye.online/AP/bc-1.csv")
#' y <- reproduce(x)
#' # TRUE
#' identical(x, y)
#' }
#'
#' @seealso \code{\link{original_file}}
#'
#' @export
reproduce.archaeophases_mcmc <- function(x, file = NULL) {
    if (!original_file(x, file))
        stop("Not the original file.")
    eval(attr(x, "mcmc"))
}

#' Reproduce an ArchaeoPhases plot
#'
#' Reproduces a plot from metadata held in an \code{archaeophases_plot}
#' object.
#'
#' @param x An \code{archaeophases_plot} object.
#'
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}
#'
#' @examples
#' \dontrun{
#' x <- read_bcal("http://tsdye.online/AP/bc-1.csv")
#' y <- multi_dates_plot(x)
#' z <- reproduce(y)
#' # TRUE
#' identical(y, z)
#'
#' #ERROR, Not the original file.
#' z <- reproduce(y, file = "foo.csv")
#' }
#'
#' @seealso \code{\link{original_file}}
#'
#' @export
reproduce.archaeophases_plot <- function(x, file = NULL) {
    if (!original_file(x, file))
        stop("Not the original file.")
    eval(attr(x, "call"))
}

#' Recreate a graphical plot
#'
#' Recreates a graphic from data and metadata held in a
#' \code{archaeophases_plot} object.
#'
#' @details
#' Uses data stored in the \code{archaeophases_plot} object, along with
#' metadata from the call of the plotting function, to recreate the original
#' graphic on the display.
#'
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}
#'
#' @seealso \code{\link{tempo_plot}}
#' @seealso \code{\link{occurrence_plot}}
#' @seealso \code{\link{marginal_plot}}
#' @seealso \code{\link{multi_marginal_plot}}
#' @seealso \code{\link{tempo_activity_plot}}
#' @seealso \code{\link{multi_dates_plot}}
#'
#' @examples
#'
#' \dontrun{
#' # Read from connection
#'   ox <- read_oxcal("http://tsdye.online/AP/ox.csv")
#'   tp_1 <- tempo_plot(ox, position = 1:ncol(ox))
#' # Recreate the tempo_plot with the original arguments
#'   plot(tp_1)
#' }
#' @export
plot.archaeophases_plot <- function(x) {
    foo <- as.list(attr(x, "call"))
    foo$data <- as.name(deparse(substitute(x)))
    foo$position <- NULL
    eval(as.call(foo))
}