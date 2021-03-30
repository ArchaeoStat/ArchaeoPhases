## Coerce to MCMC
eve <- as_mcmc(events, iteration = 1)
summary(eve)

## Plot events
plot(eve, interval = "ci", calendar = "BCAD")
plot(eve, interval = "hpdi", calendar = "BP")

## Compute phases
pha <- as_phases(eve, groups = list(B = c(2, 4), A = c(1, 3)), ordered = TRUE)
summary(pha)

## Plot phases
plot(pha, succession = FALSE, calendar = "BCAD")
plot(pha, succession = TRUE, calendar = "BCAD")
