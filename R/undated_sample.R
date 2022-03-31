#' Predictive distribution of date
#'
#' @detailsSimulate the sample from the predictive distribution of an undated sample
#' in stratigraphic constraint between two dates. The input is an MCMC sample simulated
#'  from the joint posterior distribution of these dates.
#'
#' @param data1  Numeric vector containing the output of the MCMC algorithm
#' for the begining of interval
#' @param data2  Numeric vector containing the output of the MCMC algorithm
#' for the end of interval
#' @param level Probability corresponding to the desired level of confidence.

#'  @return A list with the following components:
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr} and
#' \describe{
#' \item{TR}{ time range to characterize the period defied by data1 and data2}
#' \item{ci}{ credible interval for the preditive date}
#' \item{mcmc}{ simulated sample from undated sample age }
#' \item{call}{Function call.}
#'}
#'
#' @importFrom ggplot2 ggplot aes geom_density layer_scales geom_segment xlab ylab
#' @examples
#'   data(Phases);
#'   attach(Phases)
#'   sample = undated_sample(Phase.1.alpha,Phase.1.beta)
#'   # credible interval for the new date.
#'   sample$credible
#'   #time range interval
#'   sample$timerange
#'  # graphics = densities / IC / time range ggplot
#'    sample$gr

#' @export
undated_sample <- function(data1,data2, level =.95) {

  if (!is.vector(data1) || !is.numeric(data1) || !is.vector(data2) || !is.numeric(data2) )
    stop("Data format not recognized.")

  if ( length(data1) != length(data2) )  stop("both vetors must have the same length")
  if ( sum(data1 >data2) >0 )  stop("condition data1 < data2 is not satisfied.")

  N = length(data1)
  data.new = stats::runif(N, data1,data2)
  ci = credible_interval(data.new ,  level)$ci
  timerange =  PhaseTimeRange(data1, data2, level)[2:3]
  chain = c(data1,data2,data.new)
  name = c(rep("start",N),rep("end",N),rep("pred",N))

  df = data.frame("chain" = chain , "date" = name)
  p <- ggplot(df, aes(x=chain,col = date) ) +
     geom_density()

  M = layer_scales(p)$y$range$range[2]
  p<- p +  geom_segment(aes(x = timerange[1], y = M*1.01 , xend = timerange[2], yend =M*1.01) ,size=1.5, colour="purple")
  p<- p +   geom_segment(aes(x = ci[1], y = 0 , xend = ci[2], yend = 0),colour="#00BA38" ,size=1.5)
  p<- p+xlab("date") +
    ylab("")
  p
   list(credible = ci , timerange=timerange, mcmc = data.new ,gr = p,
       call = match.call()  )
}
