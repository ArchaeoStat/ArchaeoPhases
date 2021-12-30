#' Joint concurrence of two or more observed intervals
#'
#' Estimate the age of an undated context based on the
#' known depositional history of associated artifacts.
#'
#' @author Thomas S. Dye
#'
#' @param mcmc Dataframe or archaeophases_mcmc object with the MCMC output
#' from a Bayesian calibration.
#' @param chains a list of vectors of names or indexes of columns in \code{mcmc}.
#' @param ... Arguments to \code{multi_marginal_statistics}.
#'
#' @return foo bar
#'
#' @export
allen_joint_concurrency <- function(mcmc, chains, ...) {

  if(!is.data.frame(mcmc))
    stop("The 'mcmc' parameter must be a dataframe.")

  if(!is.list(chains))
    stop("The 'chains' parameter must be a list.")

  get_joint_start <- function(step, chains) {
    start <- -.Machine$integer.max
    for(x in seq(along = chains)) {
      start <- max(start, min(step[chains[[x]]]))
    }
    start
  }

  get_joint_end <- function(step, chains) {
    end <- .Machine$integer.max
    for(x in seq(along = chains)) {
      end <- min(end, max(step[chains[[x]]]))
    }
    end
  }

  res.start <- apply(X = mcmc,
                     MARGIN = 1,
                     FUN = get_joint_start,
                     chains = chains)

  res.end <- apply(X = mcmc,
                   MARGIN = 1,
                   FUN = get_joint_end,
                   chains = chains)

  res <- cbind.data.frame(start = res.start, end = res.end)

  stats <- multi_marginal_statistics(res, ...)

  list(statistics = stats[-3],
       result.matrix = res)
}
