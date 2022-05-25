## Coerce to MCMC
eve <- matrix(rnorm(6000, (1:6)^2), ncol = 6, byrow = TRUE)
eve <- as_events(eve, calendar = "CE")

## Compute an age-depth curve
age <- bury(eve, depth = 1:6)
plot(age)

## Predict new values
new <- predict(age, newdata = 1.5:5.5)

plot(eve)
plot(new)
