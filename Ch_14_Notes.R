library(rethinking)
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())

set.seed(43876365)

n <- 20
x <- runif(n)
y <- 2 * (x + rnorm(n, 0, 0.5))

p1 <- tibble(x, y) %>% 
  ggplot(aes(x, y)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(alpha = 0.5) +
  geom_point(x = x[5], y = y[5], color = "red")
p1

fm0 <- map2stan(
  alist(
    y ~ normal(mu, sigma),
    mu <- a + b * x,
    a ~ normal(0, 5),
    b ~ normal(0, 4),
    sigma ~ exponential(1)
  ),
  data = list(y = y, x = x),
  chains = 4, cores = 1,
  iter = 1e4,
  WAIC = FALSE
)

precis(fm0)
lm(y ~ x)
post <- extract.samples(fm0)
str(post)

p1 + 
  geom_abline(slope = median(post$b), intercept = median(post$a), color = "blue")
###########################################################################

x2 <- x
x2[5] <- NA

y2 <- c(y, seq(0, 2, length.out = 20))
x3 <- c(x2, rep(NA, 20))

fm1 <- map2stan(
  alist(
    y ~ normal(mu, sigma),
    mu <- a + b * x,
    x ~ normal(nu, sigma_x),
    a ~ normal(0, 5),
    b ~ normal(0, 4),
    nu ~ normal(0, 2),
    c(sigma, sigma_x) ~ exponential(1)
  ),
  data = list(y = y2, x = x3),
  chains = 4, cores = 1,
  control = list(adapt_delta = 0.99),
  iter = 1e4,
  WAIC = FALSE
)

post <- extract.samples(fm1)
str(post)

pi <- PI(post$x_impute)

p1 +
  geom_point(y = y[5], x = median(post$x_impute), color = "blue") +
  geom_errorbarh(aes(y = y[5], xmin = pi[1], xmax = pi[2]), height = 0.2,
                 , color = "blue") +
  geom_abline(slope = median(post$b), intercept = median(post$a),
              color = "blue")

fm_1_na <- fm1
precis(fm_1_na)
precis(fm1)

###########################################################################
set.seed(89543568)

n <- 500
x <- runif(n)
y <- 2 * (x + rnorm(n, 0, 0.5))

p1 <- tibble(x, y) %>% 
  ggplot(aes(x, y)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point() +
  geom_point(x = x[5], y = y[5], color = "red")
p1

x2 <- x
x2[5] <- NA

fm1 <- map2stan(
  alist(
    y ~ normal(mu, sigma),
    mu <- a + b * x,
    x ~ normal(nu, sigma_b),
    c(a, nu) ~ normal(0, 5),
    b ~ normal(0, 4),
    c(sigma, sigma_b) ~ exponential(1)
  ),
  data = list(y = y, x = x2),
  chains = 4, cores = 1,
  control = list(adapt_delta = 0.99),
  iter = 1e4,
  WAIC = FALSE
)

post <- extract.samples(fm1)

pi <- PI(post$x_impute)

p1 +
  geom_point(y = y[5], x = median(post$x_impute), color = "blue") +
  geom_errorbarh(aes(y = y[5], xmin = pi[1], xmax = pi[2]), height = 0.2,
                 , color = "blue") +
  geom_abline(slope = median(post$b), intercept = median(post$a),
              color = "blue")
precis(fm1)

###########################################################################
set.seed(89543568)

n <- 500
x <- runif(n)
y <- 2 * (x + rnorm(n, 0, 0.01))

p1 <- tibble(x, y) %>% 
  ggplot(aes(x, y)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point() +
  geom_point(x = x[5], y = y[5], color = "red")
p1

x2 <- x
x2[5] <- NA

fm1 <- map2stan(
  alist(
    y ~ normal(mu, sigma),
    mu <- a + b * x,
    x ~ normal(nu, sigma_b),
    c(a, nu) ~ normal(0, 5),
    b ~ normal(0, 4),
    c(sigma, sigma_b) ~ exponential(1)
  ),
  data = list(y = y, x = x2),
  chains = 4, cores = 1,
  control = list(adapt_delta = 0.99),
  iter = 1e4,
  WAIC = FALSE
)

post <- extract.samples(fm1)
str(post)

pi <- PI(post$x_impute)

p1 +
  geom_point(y = y[5], x = median(post$x_impute), color = "blue") +
  geom_errorbarh(aes(y = y[5], xmin = pi[1], xmax = pi[2]), height = 0.2,
                 , color = "blue") +
  geom_abline(slope = median(post$b), intercept = median(post$a),
              color = "blue")
precis(fm1)
