## Coerce to MCMC
eve <- as_events(events, calendar = "CE", iteration = 1)

## BP
eve_BP <- CE_to_BP(eve)
summary(eve_BP)

## CE
eve_CE <- BP_to_CE(eve_BP)
summary(eve_CE)

## Plot events
plot(eve_CE, interval = "credible", level = 0.68)
plot(eve_BP, interval = "hpdi", level = 0.68)

## Compute phases
pha <- phase(eve, groups = list(B = c(2, 4), A = c(1, 3)))
summary(pha)

## Plot phases
plot(pha)
plot(pha, range = "hiatus")
plot(pha, range = "transition")
