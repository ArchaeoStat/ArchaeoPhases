## Coerce to MCMC
eve <- as_events(events, iteration = 1)

## Plot events
## BP
eve_BP <- CE_to_BP(eve)
plot(eve_BP)

## CE
eve_CE <- BP_to_CE(eve_BP)
plot(eve_CE)

## Elapsed origin
eve_elapse <- elapse(eve, origin = 4)
plot(eve_elapse)
