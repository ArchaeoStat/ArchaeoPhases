## Coerce to phases
(pha <- as_phases(mcmc_phases, start = c(1, 3), calendar = CE(), iteration = 1))
summary(pha, calendar = CE())

## Plot phases
plot(pha)
plot(pha, succession = "hiatus")
plot(pha, succession = "transition")

## Compute phases from events
(eve <- as_events(mcmc_events, calendar = CE(), iteration = 1))

## Compute min-max range for all chains
pha1 <- phases(eve)
summary(pha1, calendar = CE())

## Compute min-max range by group
pha2 <- phases(eve, groups = list(phase_1 = c(1, 3), phase_2 = c(2, 4)))
summary(pha2, calendar = CE())
