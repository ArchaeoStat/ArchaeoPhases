#####################################################
#          Hiatus between two dates                 #
#####################################################
#'  Test for the existence of a hiatus between two parameters
#'
#' Finds if a gap exists between two dates and returns the longest interval that satisfies: \eqn{P(a_chain < IntervalInf < IntervalSup < b_chain | M) = level}
#'
#' @param a_chain : Numeric vector containing the output of the MCMC
#' algorithm for the first parameter.
#' @param b_chain : Numeric vector containing the output of the MCMC
#' algorithm for the second parameter.
#' @param level Probability corresponding to the confidence level of the interval.
#'
#' @return A named vector with the level and the endpoints of the gap in calendar years (AD/BC)
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#'
#' @author  Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}
#'
#' @examples
#'   data(Events); attach(Events)
#'   DatesHiatus(Event.1, Event.12)
#'   DatesHiatus(Event.1, Event.12, level = 0.5)
#'
#' @importFrom stats quantile
#'
#' @export
DatesHiatus <- function(a_chain, b_chain, level=0.95){

  if(length(a_chain) != length(b_chain)) {stop('Error : the parameters do not have the same length')} # test for the length of both chains

  gamma = mean((a_chain<b_chain))
  if (gamma < level) {print("No hiatus at this level")
    return(c(level=level, HiatusIntervalInf='NA',HiatusIntervalSup='NA')) } else #
    {

      interval <- function(epsilon, P1End, P2Beginning, level)
      {
        q1 = quantile(P1End ,probs = 1-epsilon) ;
        indz = (P1End < q1)
        q2 = quantile(P2Beginning[indz],probs= (1-level-epsilon)/(1-epsilon))
        c(q1,q2)
      }
      hia = Vectorize(interval,"epsilon")

      indz = which(a_chain<b_chain)
      epsilon = seq(0,1-level/gamma,.001)
      p = hia(epsilon, a_chain[indz], b_chain[indz], level/gamma)
      rownames(p)<- c("HiatusIntervalInf", "HiatusIntervalSup")

      D<- p[2,]-p[1,]
      DD = D[D>0]

      if (length(DD) > 0){
        I = which(D==max(DD))
        interval2 =  p[,I]
        if (p[2,I] != p[1,I]) {
          c(level=level, interval2[1], interval2[2])
        } else {
          c(level=level, HiatusIntervalInf='NA',HiatusIntervalSup='NA')
        }#end if (p[2,I] != p[1,I])

      } else {
        c(level=level, HiatusIntervalInf='NA',HiatusIntervalSup='NA')
      }#end if (length(DD) > 0)

    } # end if( sum(ifelse(PhaseBeginning < PhaseEnd, 1, 0) == length(PhaseBeginning) ) ) {  # test for Beginning < End
}

#'  Test for the existence of a hiatus between two MCMC chains.
#'
#' Determines whether there is a hiatus between two MCMC chains and returns
#' the longest interval that satisfies:
#' \eqn{P(a_chain < IntervalInf < IntervalSup < b_chain | M) = level}
#'
#' @param a_chain : Numeric vector containing the output of the MCMC
#' algorithm for the first parameter.
#' @param b_chain : Numeric vector containing the output of the MCMC
#' algorithm for the second parameter.
#' @param level Probability corresponding to the confidence level of the
#' interval.
#'
#' @return A list with the following components:
#' \describe{
#' \item{hiatus}{A named vector where \code{inf} is the lower endpoint of the
#' hiatus as a calendar year (AD/BC) or \code{NA} if there is no hiatus at
#' \code{level}, and \code{sup} is the upper endpoint of the gap as a calendar
#' year (AD/BC), or \code{NA} if there is no hiatus at \code{level}.}
#' \item{duration}{The duration of the hiatus at \code{level}.}
#' \item{level}{Probability corresponding to the confidence level of the
#' interval.}
#' \item{call}{The function call.}
#' }
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr},
#' @author Marie-Anne Vibet, \email{Marie-Anne.Vibet@@univ-nantes.fr}, and
#' @author Thomas S. Dye, \email{tsd@tsdye.online}
#'
#' @examples
#'   data(Events); attach(Events)
#'   dates_hiatus(Event.1, Event.12)
#'   dates_hiatus(Event.1, Event.12, level = 0.5)
#'
#' @importFrom stats quantile
#'
#' @export
dates_hiatus <- function(a_chain, b_chain, level = 0.95) {

    ## test for the length of both chains
    if (length(a_chain) != length(b_chain)) {
        stop('Error : the parameters do not have the same length')
    }

    no_hiatus <- list(hiatus = c(inf = 'NA', sup = 'NA'),
                      duration = 'NA',
                      level = level,
                      call = match.call())

    gamma <- mean(a_chain < b_chain)

    if (gamma < level) return(no_hiatus)

    interval <- function(epsilon, p1, p2, level) {
        prob <- 1 - epsilon
        q1 <- quantile(p1, probs = prob)
        ind <- (p1 < q1)
        q2 <- quantile(p2[ind], probs = (prob - level) / prob)
        c(q1, q2)
    }
    hia = Vectorize(interval, "epsilon")

    ind = which(a_chain < b_chain)
    epsilon = seq(0, 1 - level/gamma, .001)
    p = hia(epsilon, a_chain[ind], b_chain[ind], level / gamma)
    d <- p[2, ] - p[1, ]
    dd <- d[d > 0]

    if (length(dd) < 1) return(no_hiatus)

    i <- which(d == max(dd))
    i2 = p[, i]

    if (p[2, i] == p[1, i]) return(no_hiatus)

    inf <- unname(i2[1])
    sup <- unname(i2[2])
    list(hiatus = c(inf = inf, sup = sup),
         duration = sup - inf,
         level = level,
         call = match.call())
}
