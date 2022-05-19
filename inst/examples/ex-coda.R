## Load coda
library(coda)

## Coerce to MCMC
eve <- as_events(events, iteration = 1)

## Coerce to coda
mc <- as_coda(eve[, 1:2], chains = 3)
plot(mc)

## Autocorrelation
autocorr.plot(mc)

## Gelman-Rubin diagnostic
## The multivariate criterion can not be evaluated when a phase
## contains only one date. This induces colinearity problems.
gelman.diag(mc)
gelman.plot(mc)
