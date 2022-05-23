## Coerce to MCMC
eve <- as_events(events, calendar = "CE", iteration = 1)
eve <- eve[1:10000, ]

## CE
credible(eve, level = 0.95) # Credible interval
hpdi(eve, level = 0.95) # HPD interval

## BP
eve_BP <- CE_to_BP(eve)
credible(eve_BP, level = 0.95) # Credible interval
hpdi(eve_BP, level = 0.95) # HPD interval
