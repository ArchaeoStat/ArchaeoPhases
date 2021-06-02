## Coerce to MCMC
eve <- as_mcmc(events, iteration = 1)
summary(eve)

## Plot events
plot(eve, interval = "ci", level = 0.68)
plot(eve, interval = "hpdi", level = 0.68)

## Compute phases
pha <- as_phases(eve, groups = list(B = c(2, 4), A = c(1, 3)), ordered = TRUE)
summary(pha)

## Plot phases
plot(pha, succession = FALSE)
plot(pha, succession = TRUE)
