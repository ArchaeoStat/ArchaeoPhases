## Coerce to phases
pha <- as_phases(mcmc_phases, start = c(1, 3), calendar = CE(), iteration = 1)

## Compute phase duration
dur <- duration(pha)
summary(dur)
