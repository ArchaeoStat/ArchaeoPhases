## Coerce to MCMC
events <- as_mcmc(Events, iteration = 1)

## Coerce to coda
library(coda)
mcmc <- as_coda(events, chains = 3)
plot(mcmc)
gelman.diag(mcmc)

# The multivariate criterion can not be evaluated when a phase
# contains only one date. This induces colinearity problems.
gelman.diag(mcmc, multivariate = FALSE)
