## Events
(eve <- as_events(events, calendar = "CE", iteration = 1))

eve[1:1000, ] # Select the first 1000 iterations
eve[, 1:2]    # Select the first 2 events

cbind2(eve[, 1:2], eve[, 3:4]) # Combine two MCMC objects

## Phases
(pha <- as_phases(phases, start = c(1, 3), calendar = "CE", iteration = 1))

pha[1:1000, , ]          # Select the first 1000 iterations
pha[, 1, , drop = FALSE] # Select the first phase
