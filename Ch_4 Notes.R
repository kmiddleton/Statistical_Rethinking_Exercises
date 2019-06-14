library(plotly)
library(tidyverse)
library(cowplot)
library(rethinking)

undulation_rate <- c(0.9, 1.2, 1.2, 1.3, 1.4, 1.4, 1.6, 2.0)

n <- 100 # How fine is the grid
mu <- seq(0.01, 6, length = n) # Vector of mu
sigma <- seq(0.2, 2, length = n) # Vector of sigma

grid_approx <- crossing(mu, sigma) %>% 
  mutate(log_lik = NA)

for (ii in 1:nrow(grid_approx)) {
  grid_approx[ii, "log_lik"] <- 
    sum(dnorm(undulation_rate,
              mean = grid_approx$mu[ii],
              sd = grid_approx$sigma[ii],
              log = TRUE))
}

head(grid_approx)

(max_ll <- grid_approx[which.max(grid_approx$log_lik), ])

# grid_approx <- grid_approx %>% filter(log_lik > -500)
plot_ly() %>%
  add_markers(data = grid_approx, x = ~ mu, y = ~ sigma, z = ~ log_lik, color = ~ log_lik,
              colors = c('#BF382A', '#0C4B8E'), size = 1) %>% 
  add_markers(data = max_ll, x = ~ mu, y = ~ sigma, z = ~ log_lik,
              colors = "orange") %>% 
  hide_colorbar() %>% hide_legend()

log_lik_mat <- matrix(grid_approx$log_lik, ncol = n, nrow = n)
plot_ly(z = ~ log_lik_mat) %>% add_surface()

fm <- rethinking::map(
  alist(
    undulation_rate ~ dnorm(mu, sigma),
    mu ~ dnorm(1, 1),
    sigma ~ dcauchy(0, 1)
  ),
  data = data.frame(undulation_rate),
  start = list(mu = 1, sigma = 1)
)

precis(fm)

x <- seq(-2, 2, length.out = 1000)
y <- 2 * x ^ 2 + 3

M <- tibble(x, y)
M_pr <- tibble(x = x[2:1000],
               y = diff(y))
p1 <- ggplot() +
  geom_line(data = M, aes(x, y))
p2 <- ggplot() +
    geom_line(data = M_pr, aes(x, y), color = "red")
plot_grid(p1, p2, ncol = 2)

###

library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

m1 <- rethinking::map( flist <- alist(
  height ~ dnorm( mu , sigma ) ,
  mu ~ dnorm( 178 , 20 ) ,
  sigma ~ dunif( 0 , 10 )
), data=d2 )

m2 <- rethinking::map( flist <- alist(
  height ~ dnorm( mu , sigma ) ,
  mu ~ dnorm( 178 , 1 ) ,
  sigma ~ dunif( 0 , 10 )
), data=d2 )

rethinking::compare(m1, m2)

## Model from scratch

d2 <- d2 %>% filter(male == 0)

ggplot(d2, aes(age, height)) + geom_point()

ggplot(d2, aes(height)) + geom_histogram()

m1 <- rethinking::map2stan(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * age,
    a ~ dnorm(150, 20),
    b ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 3)
  ),
  data = d2,
  start = list(a = 150, b = 0, sigma = 1)
)
precis(m1)

m2 <- rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a,
    a ~ dnorm(150, 20),
    sigma ~ dcauchy(0, 3)
  ),
  data = d2,
  start = list(a = 150, sigma = 4)
)
precis(m2)

rethinking::compare(m1, m2)

vcov(m1)

pairs(m1)
