library(tidyverse)
library(rethinking)
library(cowplot)
theme_set(theme_cowplot())

set.seed(824763)

M <- tibble(
  x = runif(100),
  y = 0.1 * x + rnorm(100, 0, 0.5),
  x_c = x - mean(x),
  y_c = y - mean(y)) %>% 
  as.data.frame()

p1 <- ggplot(M, aes(x, y)) +
  geom_point()

p2 <- ggplot(M, aes(x_c, y_c)) +
  geom_point()

plot_grid(p1, p2, nrow = 2)

fm2 <- rethinking::map(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + b * x,
    a ~ dnorm(0, 5),
    b ~ dnorm(0, 5),
    sigma ~ dexp(1)
  ),
  data = M)

fm2c <- rethinking::map(
  alist(
    y_c ~ dnorm(mu, sigma),
    mu <- a + b * x_c,
    a ~ dnorm(0, 5),
    b ~ dnorm(0, 5),
    sigma ~ dexp(1)
  ),
  data = M)
precis(fm2)
precis(fm2c)



fm1 <- rethinking::map(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a,
    a ~ dnorm(0, 5),
    sigma ~ dexp(1)
  ),
  data = M)
precis(fm1)
precis(fm2)
rethinking::compare(fm1, fm2)

###############################################
set.seed(28937)
x1 <- runif(100)
x2 <- runif(100)
y <- 2 * x1 + 2 * x2 + 20 * x1 * x2 + rnorm(100, 0, 1)
M <- data.frame(x1, x2, y, x1x2 = x1 * x2)
GGally::ggscatmat(M)

fm1 <- rethinking::map(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- b1 * x1 + b2 * x2 + bint * x1 * x2,
    c(b1, b2, bint) ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ),
  data = M
)
precis(fm1)
lm(y ~ x1 * x2, M)

M %>% 
  ggplot(aes(x1, x2, size = y)) +
  geom_point()

###############################################

set.seed(753479)
M <- tibble(
  x1 = runif(100),
  x2 = rep(c(0,1), each = 50),
  y = 2 * x1 + 0.25 * x2 + rnorm(100, 0, 0.5)) %>% 
  as.data.frame()

M %>% ggplot(aes(y = y, x = x1, color = factor(x2))) +
  geom_point()

fm <- rethinking::map(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + b1 * x1 + b2 * x2,
    a ~ dnorm(0, 5),
    b1 ~ dnorm(0, 1),
    b2 ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = M
)
precis(fm)

post <- extract.samples(fm)

g1 <- post$a + post$b1 * 10
g2 <- post$a +  + post$b1 * 10 + post$b2

G <- tibble(g1, g2, d = g2 - g1)

hist(G$d)
HPDI(G$d)
