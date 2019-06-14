library(tidyverse)
library(cowplot)
library(rethinking)

## Code 3.17
(p_grid <- seq(0, 1, length.out = 1000))
prior <- rep(1, 1000)
likelihood <- dbinom(3, size = 3, prob = p_grid)

posterior <- likelihood * prior
hist(posterior)

posterior <- posterior / sum(posterior)
hist(posterior)
plot(p_grid, posterior)

samples <- sample(p_grid, size = 1e4, replace = TRUE,
                  prob = posterior)
hist(samples)
dens(samples)

PI(samples, prob = 0.5)

sum(posterior * abs(0.5 - p_grid))

loss <- sapply(p_grid, function(d) sum(posterior * abs(d - p_grid)))

p_grid[which.min(loss)]
median(samples)

###############################
## Quantiles as counting

M <- tibble(x = rnorm(1e6))

M %>% 
  ggplot(aes(x)) +
  geom_histogram(bins = 50)

(qq <- quantile(M$x, c(0.025, 0.975)))

sum(M$x <= qq[1]) / length(M$x)
sum(M$x >= qq[2]) / length(M$x)

M <- M %>% 
  mutate(
    tail_prob = case_when(
      x < qq[1] ~ "yes",
      x > qq[2] ~ "yes",
      TRUE ~ "no"
    ))

head(M)

M %>% 
  ggplot(aes(x, fill = tail_prob)) +
  geom_histogram(bins = 500)

###############################
## Posterior predictive checks
## "Right" model
set.seed(28364)

x <- runif(50)
y <- 2 * x + rnorm(20, 0, 0.2)

M <- data.frame(x, y)

ggplot(M, aes(x, y)) + geom_point()

fm <- rethinking::map(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + b * x,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 2),
    sigma ~ dexp(1)
  ),
  data = M
)

precis(fm)

pred <- data.frame(x = seq(0, 1, length.out = 200))
mu <- sim(fm, data = pred, n = 1e4)
mu_mean <- apply(mu, 2, mean)
mu_HPDI <- apply(mu, 2, HPDI, prob = 0.89)

preds <- tibble(
  x = pred$x,
  pred = mu_mean,
  lower = mu_HPDI[1, ],
  upper = mu_HPDI[2, ]
)

ggplot() + 
  geom_ribbon(data = preds, aes(x = x, ymin = lower, ymax = upper),
              fill = "gray", alpha = 0.5) +
  geom_line(data = preds, aes(x = x, y = mu_mean)) +
  geom_point(data = M, aes(x, y))

## "Wrong" model

fm <- rethinking::map(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a,
    a ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ),
  data = M
)

precis(fm)

pred <- data.frame(x = seq(0, 1, length.out = 200))
mu <- sim(fm, data = pred, n = 1e4)
mu_mean <- apply(mu, 2, mean)
mu_HPDI <- apply(mu, 2, HPDI, prob = 0.89)

preds <- tibble(
  x = pred$x,
  pred = mu_mean,
  lower = mu_HPDI[1, ],
  upper = mu_HPDI[2, ]
)

ggplot() + 
  geom_ribbon(data = preds, aes(x = x, ymin = lower, ymax = upper),
              fill = "gray", alpha = 0.5) +
  geom_line(data = preds, aes(x = x, y = mu_mean)) +
  geom_point(data = M, aes(x, y))
