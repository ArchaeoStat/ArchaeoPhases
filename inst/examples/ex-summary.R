## Coerce to MCMC
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)

## Summary
summary(eve, calendar = CE())
summary(eve, calendar = BP())

## Plot events
plot(eve, calendar = CE(), interval = "credible", level = 0.68)
plot(eve, calendar = BP(), interval = "hdr", level = 0.68)
plot(eve[, 1], interval = "hdr")

## Compute phases
pha <- phases(eve, groups = list(B = c(2, 4), A = c(1, 3)))

## Summary
summary(pha, calendar = CE())
summary(pha, calendar = BP())

## Plot phases
plot(pha, calendar = BP())
plot(pha, succession = "hiatus")
plot(pha, succession = "transition")
