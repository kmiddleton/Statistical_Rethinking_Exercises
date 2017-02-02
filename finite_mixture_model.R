library(dplyr)
library(ggplot2)
library(ggthemes)
library(rstan)
options(mc.cores = 2)

# Number of data points
N <- 400

# Let's make three states
mu <- c(3, 6, 9)
sigma <- c(2, 4, 3)

# with probability
Theta <- c(.5, .2, .3)

# Draw which model each belongs to
z <- sample(1:3, size = N, prob = Theta, replace = T)

# Some white noise
epsilon <- rnorm(N)

# Simulate the data using the fact that y ~ normal(mu, sigma) can be 
# expressed as y = mu + sigma*epsilon for epsilon ~ normal(0, 1)
y <- mu[z] + sigma[z]*epsilon

data_frame(y, z = as.factor(z)) %>% 
  ggplot(aes(x = y, fill = z)) +
  geom_density(alpha = 0.3) +
  theme_economist() +
  ggtitle("Three data generating processes")

compiled_model <- stan_model("finite_mixture_linear_regression.stan")

estimated_model <- sampling(
  compiled_model,
  data = list(N = N, y = y, n_groups = 3),
  iter = 6000)

traceplot(estimated_model)
summary(estimated_model)
