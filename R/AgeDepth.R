#####################################################
#         Age Depth curve                           #
#####################################################
#'  age depth curve
#' Compute the age-depth curve from the output of mcmc algorithm of ages  and the known depth of each dated samples.
#'
#' @param data Data frame containing the output of the MCMC algorithm.
#' @param depth Vector of the depths of the dated samples
#' @param new.depth Vector of the undated depths whose the age will be predict. By defauft \code{new.depth=NULL}
#' @param max.iter a non-negative integer giving the limit number of MCMC  itereations  By defauft \code{max.iter=nrow(data)}
#' @param sampling should sampling be random. By defauft \code{sampling = FALSE}

#' @details Estimate the Age-Depth relationship from the MCMC output of a Bayesian chronological model and the depth of each dated sample.
#' We assume it exists a function \eqn{f} relating the age and  the depth \eqn{ age = f( depth) }. We estimate the function using local regression (also called local polynomial regression):   \eqn{f = loess(age ~ depth)}.
#' This  estimated function \eqn{f} depends on the unknown dates.  However, from the posterior distribution of the age/date sequence, we can evaluate the posterior distribution of the age function for each desired depth.


#' @references  D.K. Jha , P. Sanyal and A. Philippe 2020.  Multi-Proxy Evidence of Late Quaternary Climate and Vegetational History of North-Central India: Implication for the Paleolithic to Neolithic Phases. Quaternary Science Reviews 229: 106-121.
#' @references  S. Ghosh, P. Sanyal, R. Bhushan, S. P Sati, A. Philippe, and N. Juyal. 2020. Early Holocene Indian summer monsoon and its impact on vegetation in the Central Himalaya: Insight from δD and δ13C values of leaf wax lipid. The Holecene 30:7, 1063-1074.
#'
#' @author Anne Philippe, \email{Anne.Philippe@@univ-nantes.fr}
#'
#' @examples
#'
#'    data = matrix(rnorm(6000,(1:6)^2), ncol=6 , byrow = TRUE )
#'    depth= 1:6
#'    AgeDepth(data,depth)
#'    AgeDepth(data,depth, 1.5:6)

#'
#' @return
#' A list  containing two matrix.  \code{data.stat} summarises the MCMC output from the L1 Bayes estimate and  credible interval
#' at significance level 68% and 95%. \code{age.depth} provide the L1 Bayes estimate, credible interval
#' at significance level 68% and 95% on the age  at depth \code{depth} and \code{new.depth}
#'
#'
#'
#' @export

AgeDepth <- function(data, depth, new.depth=NULL , max.iter=nrow(data), sampling =FALSE){

  if (length(unique(depth)) != length(depth))  stop('Error : all depth values must be different ')
  if (ncol(data)!= length(depth)) stop('Error : the dimension of age is not compatible with the length of depth ')
  I= order(depth)
  depth=depth[I]
  data = as.matrix( data[,I])

  #summary MCMC Output
  CI = apply(data , 2, median )
  CI0 = t(apply(data,2,CredibleInterval ,level = .95, roundingOfValue=5))[,-1]
  CI00 = t(apply(data,2,CredibleInterval ,level = .68, roundingOfValue=5))[,-1]
  I = 1:length(depth)
  data.stat <- data.frame(depth = depth[I] , estimate = CI ,  credible95 = CI0 ,credible68 = CI00 )
  names( data.stat) <- c("depth","estimate" ," lower95","upper95","lower68","upper68")

 # check the position of undated depths
depth.in = ifelse(new.depth > min(depth),1,0) * ifelse(new.depth < max(depth),1,0)
if (sum(depth.in) < length(new.depth))
  {
    print('the age cannot be predicted outside the range [min(depth) , max(depth) ]. There are omitted ')
    new.depth <- new.depth[depth.in == 1]
   }

# number of MCMC iterations
    L = min(nrow(data),max.iter)
    indz.L = 1:L
    if  ((L < nrow(data)) & (sampling == TRUE) ) indz.L = sample(size = L , x=1:nrow(data))
    curve = NULL


        if(length(new.depth) == 0)
      {
      for ( i in indz.L )
    {
      dt<- data.frame(y=data[i,] , x =depth  )
      lo <- stats::loess(y  ~ x,data = dt ,  degree = 1)
     curve <-  rbind( curve , stats::predict(lo,depth) )
      }
    }
    else
    {
      a =   matrix( 0 , ncol =length(new.depth) , nrow = L)

      for ( i in indz.L )
      {   dt<- data.frame(y=data[i,] , x =depth )
         lo <- stats::loess(y  ~ x,data = dt ,  degree = 1)
                                 curve = rbind( curve , stats::predict(lo,depth) )
                                 a[i, ] = stats::predict(lo, new.depth)
      }
      }

    if(length(new.depth) > 0)
    {  plot(M  <- apply(data, 2, median) , depth ,type ="p",xlim=c(min(a,curve) , max(a,curve)),ylim=c(min(depth,new.depth),max(depth,new.depth)) , xlab="Age", ylab = "depth/alt " ,col="white", bty="n")}else
      { plot(M  <- apply(data, 2, median) , depth ,type ="p",xlim=c(min(curve) , max(curve)), xlab="Age", ylab = "depth/alt " ,col="white", bty="n")}
    lines(apply(curve,2,median), depth, col ="violet ")
    CIcurve =  apply(curve,2,CredibleInterval,level=.95,roundingOfValue=5)[2:3,]
    lines(CIcurve[1,] , depth, col = " turquoise")
    lines(CIcurve[2,] , depth, col = " turquoise")
    CIcurve0 =  apply(curve,2,CredibleInterval, level = .68,roundingOfValue=5)[2:3,]
    lines(CIcurve0[1,] , depth, col = "slateblue" )
    lines(CIcurve0[2,] , depth, col = "slateblue" )
    title("depth- age curve")
    segments(CI0[,1],depth,CI0[,2],depth  , col =grDevices::terrain.colors(ncol(data) ), lwd=1)

    result = data.frame(depth = depth, estimate = apply(curve,2,median) ,   credible95 = t(CIcurve), credible68= t(CIcurve0))
    names(result) <- c("depth","estimate" ," lower95","upper95","lower68","upper68")

    if(length(new.depth) > 0)
    {

    CI.prev = as.matrix(apply(a,2,CredibleInterval,level=.95,roundingOfValue=5)[2:3,] )
    CI.prev0 = as.matrix(apply(a,2,CredibleInterval, level = .68,roundingOfValue=5 )[2:3,] )
    segments(CI.prev[1,],new.depth,CI.prev[2,],new.depth , col = "Turquoise", lwd=1)
    segments(CI.prev0[1,],new.depth,CI.prev0[2,],new.depth , col = "slateblue", lwd=1)
    points(apply(a,2,median), new.depth, cex=.3,col ="violet ")
    result.prev = data.frame(depth = new.depth, estimate = apply(a,2,median) ,   credible95 = t(CI.prev), credible68= t(CI.prev0))
    names(result.prev) <- c("depth","estimate" ," lower95","upper95","lower68","upper68")

    result = rbind(result ,result.prev)
    result = data.frame(  samp = c(rep("dated",length(depth)),rep("undated",length(new.depth))),result)

    }
    return(list(age.depth = result, data.stat = data.stat))
    }

