#' Observed frequency of an Allen set
#'
#' Create a matrix of observed frequencies of a given
#' Allen set among two or more groups of chains from
#' the MCMC output of a Bayesian calibration.  The
#' groups are permuted to form the matrix.
#'
#' @author Thomas S. Dye
#'
#' @param mcmc Dataframe or archaeophases_mcmc object with the MCMC output
#' from a Bayesian calibration.
#' @param chains a list of vectors of names or indexes of columns in \code{mcmc}.
#' @param allen_set A string representation of an Allen set.
#'
#' @return A matrix of observed frequencies.
#'
#' @examples
#'
#' \dontrun{
#' # Dates associated with bead BE3 Amber
#' be3.amber <- c("UB-4836 (WG27)", "UB-5208 (ApD107)", "UB-4965 (ApD117)",
#' "UB-4735 (Ber022)", "UB-4739 (Ber134/1)", "UB-4728 (MH064)",
#' "UB-4729 (MH068)", "UB-4732 (MH094)", "UB-4733 (MH095)", "UB-4734 (MH105c)",
#' "UB-4984 (Lec018)", "UB-4709 (EH014)", "UB-4707 (EH079)", "UB-4708 (EH083)",
#' "UB-6033 (WHes113)", "UB-4706 (WHes118)", "UB-4705 (WHes123)",
#' "UB-6040 (CasD053)", "UB-6037 (CasD134)", "UB-6472 (BuD222)",
#' "UB-6473 (BuD250)", "UB-6476 (BuD339)", "UB-4963 (SPTip208)",
#' "UB-4890 (MelSG075)", "UB-4887 (MelSG082)", "UB-4888 (MelSG089)",
#' "MaDE1 & E2", "UB-4552 (MaDE3)", "UB-4975 (AstCli12)", "UB-4835 (ApD134)",
#' "SUERC-39108 ERLK G322", "SUERC-39109 ERL G362", "SUERC-39112 ERL G405",
#' "SUERC-51560 ERL G038", "SUERC-39091 (ERL G003)", "SUERC-39092 (ERL G005)",
#' "SUERC-39113 (ERL G417)", "SUERC-51549 (ERL G195)", "SUERC-51552 (ERL G107)",
#' "SUERC-51550 (ERL G254)")
#'
#' # Dates associated with bead BE1 Dghnt
#' be1.dghnt <- c("UB-4503 (Lec148)", "UB-4506 (Lec172/2)",
#' "UB-6038 (CasD183)", "UB-4512 (EH091)", "UB-4501 (Lec014)",
#' "UB-4507 (Lec187)", "UB-4502 (Lec138)", "UB-4042 (But1674)",
#' "SUERC-39100 (ERL G266)")
#'
#' # Construct a list of vectors
#' chains <- list("BE3-Amber" = be3.amber, "BE1-Dghnt" = be1.dghnt)
#'
#' # Read the calibration MCMC output
#' oxc <- read_oxcal("https://tsdye.online/AP/beads-mcmc.csv", quiet = 'yes')
#' 
#' # Observe 2x2 frequency matrix of the relation of trunk to branch
#' # allen_observe(mcmc = oxc, chains = chains, allen_set = "oFD")
#' }
#'
#' @importFrom gtools permutations
#' 
#' @export
allen_observe_frequency <- function(mcmc,
                                    chains,
                                    allen_set) {

  if(!is.data.frame(mcmc))
    stop("The 'mcmc' parameter must be a dataframe.")

  if(!is.list(chains))
    stop("The 'chains' parameter must be a list.")

  if(is.null(names(chains)))
    stop("The 'chains' parameter must be a list with named elements.")

  if(!allen_is_set_string(allen_set))
    stop("The 'allen_set' parameter must be an Allen set.")

  allen_vec <- allen_string_to_set(allen_set)

  get_relation <- function(mcmc, first, second) {
    allen_relation(min(mcmc[first]),
                   max(mcmc[first]),
                   min(mcmc[second]),
                   max(mcmc[second]))
  }

  compile_result <- function(perms, chains, mcmc, allen_vec) {
    result <- allen_create_result_vector()

    result.vec <- apply(X = mcmc,
                        MARGIN = 1,
                        FUN = get_relation,
                        first = chains[[perms[1]]],
                        second = chains[[perms[2]]])

    for(res in result.vec)
      result <- allen_update_result(relation = res,
                                    result_vector = result)

    result <- allen_proportion_result(result)

    sum(result[allen_vec])
  }

  ## a matrix
  perms <- gtools::permutations(length(chains), 2, names(chains))

  foo <- apply(X = perms,
               MARGIN = 1,
               FUN = compile_result,
               chains = chains,
               mcmc = mcmc,
               allen_vec = allen_vec)

  res <- matrix(nrow = length(chains),
                ncol = length(chains),
                dimnames = list(names(chains), names(chains)))

  for(x in 1:nrow(perms))
    res[perms[x, 1], perms[x, 2]] <- foo[x]

  list(rounded.percentage = round(res * 100),
       observed = res)
}
