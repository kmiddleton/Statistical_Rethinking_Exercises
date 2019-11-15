(x <- rnorm(10))
(x_bar <- mean(x))
(x_sd <- sd(x))

(fm <- lm(x ~ 1))

logLik(fm)

sum(dnorm(x, mean = x_bar, sd = x_sd, log = TRUE))

