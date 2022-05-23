## Coerce to MCMC
eve <- as_events(events, calendar = "CE", iteration = 1)
eve <- eve[1:10000, ]

## Test for anteriority
older(eve)

## Test for hiatus
hia <- hiatus(eve)
as.data.frame(hia)
