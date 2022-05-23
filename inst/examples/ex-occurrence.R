## Coerce to MCMC
eve <- as_events(events, calendar = "CE", iteration = 1)
eve <- eve[1:10000, ]

## Occurrence plot
occ_ci <- occurrence(eve, interval = "ci")
plot(occ_ci)

occ_hpdi <- occurrence(eve, interval = "hpdi")
plot(occ_hpdi)
