#' Estimate ranges from two or more calibrations
#'
#' Calculates the ranges of summary statistics from the output of two or more
#' runs of the MCMC algorithm. Results are given in
#' calendar years for statistics that estimate them.
#'
#' This function is useful for estimating the sensitivity of calibration results
#' to different model parameters.
#'
#' @param mcmc A vector of path names to the MCMC files.
#' @param position Numeric vector containing the positions of the columns
#' corresponding to the MCMC chains of interest, or a vector of column
#' names.
#' @param app Name of the application that created the MCMC files,
#' one of \code{bcal}, \code{oxcal}, \code{chronomodel.}
#' @param estimates Numeric vector containing the positions of the columns
#' corresponding to the statistics of interest returned by the
#' \code{multi_marginal_statistics()} function, or a vector of column
#' names.
#' @param quiet One of \code{no} (default) to allow messages and warnings,
#' \code{partial} to suppress messages and allow warnings,
#' or \code{yes} to suppress messages and warnings.
#' @param bin_width If \code{app} is set to \code{bcal}, the bin width
#' specified for the \href{https://bcal.shef.ac.uk/}{BCal} calibration.
#' Defaults to the \href{https://bcal.shef.ac.uk/}{BCal} default of 1.
#' @param decimal If \code{app} is set to \code{chronomodel},
#' either \code{.} (default) or \code{,}, the two choices offered by
#' \href{https://chronomodel.com/}{ChronoModel}.
#' @param separator If \code{app} is set to \code{chronomodel}, the character
#' used to separate fields in the CSV file.  Defaults to \code{,}.
#' @return A list with the following components:
#' \describe{
#' \item{range_table}{A matrix of estimate ranges.}
#' \item{mean}{The mean of the ranges in \code{range_table}.}
#' \item{sd}{The standard deviation of the ranges in \code{range_table}.}
#' \item{min}{The minimum of the ranges in \code{range_table}.}
#' \item{median}{The median of the ranges in \code{range_table}.}
#' \item{max}{The maximum value of the ranges in \code{range_table}.}
#' }
#'
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}
#'
#' @examples
#'\dontrun{
#' ## Generate 0's
#' res <- estimate_range(mcmc = c("http://tsdye.online/AP/ox.csv",
#' "http://tsdye.online/AP/ox.csv"), position = c(1, 2),
#' app = "oxcal", quiet = "yes")
#' sum(res$range_table)
#' }
#'
#' @export
estimate_range <- function(mcmc, position, app = "bcal",
                           estimates = c("mean", "q1", "median",
                                         "q3", "ci.inf", "ci.sup"),
                           quiet = "partial",
                           bin_width = 1,
                           dec = ".", sep = ",") {
  if (length(mcmc) < 2)
    stop("Two or more MCMC files required.")
  if (is.null(position))
    stop("One or more column positions are required.")
  if (!is.element(app, c("bcal", "oxcal", "chronomodel")))
    stop("Invalid calibration application.")
  mcmc_list <- lapply(X = mcmc,
                      FUN = function(x, a = app)
                          switch(a,
                                 bcal = read_bcal(file = x,
                                                  bin_width = bin_width,
                                                  quiet = quiet),
                                 oxcal = read_oxcal(file = x,
                                                    quiet = quiet),
                                 chronomodel = read_chronomodel(file = x,
                                                                decimal = dec,
                                                                separator = sep,
                                                                quiet = quiet)))
  data_list <- lapply(X = mcmc_list,
                      FUN = function(x) x[, position])
  stats_array <- lapply(X = data_list,
                        FUN = function(x)
                        {multi_marginal_statistics(x)$statistics[, estimates]})
  min_matrix <- matrix(.Machine$integer.max, nrow(stats_array[[1]]), ncol(stats_array[[1]]))
  max_matrix <- -min_matrix
  for (x in 1:length(stats_array))
    {
      min_matrix <- pmin(min_matrix, stats_array[[x]])
      max_matrix <- pmax(max_matrix, stats_array[[x]])
    }
  res <- max_matrix - min_matrix
  rownames(res) <- rownames(stats_array[[1]])
  colnames(res) <- colnames(stats_array[[1]])
  list(range_table = res,
       mean = mean(res),
       sd = sd(res),
       min = min(res),
       median = median(res),
       max = max(res))
}
