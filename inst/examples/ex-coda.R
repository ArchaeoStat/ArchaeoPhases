## Load coda
library(coda)

## Coerce to MCMC
events <- as_mcmc(Events, iteration = 1)

## Coerce to coda
mc <- as_coda(events[, 1:2], chains = 3)
plot(mc)

## Autocorrelation
autocorr.plot(mc)

## Gelman-Rubin diagnostic
## The multivariate criterion can not be evaluated when a phase
## contains only one date. This induces colinearity problems.
gelman.diag(mc)
gelman.plot(mc)
