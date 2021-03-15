\donttest{
## Coerce to MCMC
events <- as_mcmc(Events, iteration = 1)
events <- events[1:10000, ]

## Activity plot
act <- activity(events)
plot(act)
}
