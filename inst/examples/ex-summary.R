## Coerce to MCMC
events <- as_mcmc(Events, iteration = 1)
summary(events)

## Plot events
plot(events, interval = "ci")
plot(events, interval = "hpdi")

## Compute phases
phases <- phase(events, groups = list(A = c(1, 3), B = c(2, 4)))
summary(phases)

## Plot phases
plot(phases)
