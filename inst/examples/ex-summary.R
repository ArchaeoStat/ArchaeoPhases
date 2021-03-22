## Coerce to MCMC
events <- as_mcmc(Events, iteration = 1)
summary(events)

## Plot events
plot(events, interval = "ci", calendar = "BCAD")
plot(events, interval = "hpdi", calendar = "BC")

## Compute phases
phases <- phase(events, groups = list(B = c(2, 4), A = c(1, 3)), ordered = TRUE)
summary(phases)

## Plot phases
plot(phases, succession = FALSE, calendar = "BCAD")
plot(phases, succession = TRUE, calendar = "BCAD")
