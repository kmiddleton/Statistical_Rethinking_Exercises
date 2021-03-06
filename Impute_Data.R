library(rethinking)
library(tidyverse)
library(cowplot)

###### Simulated data

set.seed(83746)
n <- 400
x <- rnorm(n)
y <- 2 * x + rnorm(n)
plot(x, y)

prop_missing <- 0.5

fm1 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + b * x,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 1)
  ),
  data = list(x = x, y = y)
)

x1 <- x
missing <- sample(1:n)[1:trunc(prop_missing * n)]
x1[missing] <- NA

fm2 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + b * x1,
    x1 ~ dnorm(nu, sigma_nu),
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 10),
    nu ~ dnorm(0, 10),
    sigma_nu ~ dcauchy(0, 1),
    sigma ~ dcauchy(0, 1)
  ),
  data = list(x1 = x1, y = y)
)

precis(fm1)
precis(fm2)
rethinking::compare(fm1, fm2)

post <- extract.samples(fm2)
M <- tibble(y = y[missing],
            x_obs = x[missing],
            x_imp = apply(post$x1_impute, 2, median))
ggplot() +
  geom_point(data = M, aes(x_obs, y), color = "black") +
  geom_point(data = M, aes(x_imp, y), color = "red")

##

y1 <- y
y1[1] <- NA

fm3 <- map2stan(
  alist(
    yest ~ dnorm(mu, sigma),
    mu <- a + b * x,
    yest ~ 
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 1)
  ),
  data = list(x1 = x1, y = y)
)


###### Real data #####################################################

mammals <- read_delim("~/Dropbox/Classes/Topics_Biostats/data/mammals.txt",
                      delim = "\t")
names(mammals) <- sub("^[0-9]*-[0-9]*_", "", names(mammals))
names(mammals) <- sub("MSW05_", "", names(mammals))
mammals <- dplyr::select(mammals, Order, Binomial, AdultBodyMass_g, 
                         AdultForearmLen_mm)
names(mammals) <- gsub("([A-Z])", "_\\L\\1", names(mammals),
                       perl = TRUE)
names(mammals) <- gsub("^_", "", names(mammals),
                       perl = TRUE)
mammals[mammals == -999] <- NA
names(mammals)[names(mammals) == "binomial"] <- "species"
names(mammals) <- c("Order", "Species", "Mass", "Forearm")

# mammals <- mammals %>% filter(Mass <= 5000)

mammals_s <- mammals %>% mutate(log_Mass = log10(Mass),
                              log_Forearm = log10(Forearm)) %>% 
  drop_na(log_Mass)

##
fm <- lm(log_Forearm ~ log_Mass, data = mammals_s)
summary(fm)

m_noNA <- mammals_s %>% 
  drop_na() %>% 
  as.data.frame()

data_list <- list(
  log_Mass = mammals_s$log_Mass,
  log_Forearm = mammals_s$log_Forearm
)

##
fm0 <- map2stan(
  alist(
    log_Forearm ~ dnorm(mu, sigma),
    mu <- a + bMass * log_Mass,
    a ~ dnorm(0, 10),
    bMass ~ dnorm(0, 5),
    sigma ~ dcauchy(0, 1)
  ) ,
  data = m_noNA,
  iter = 1e4,
  chains = 2
)

plot(fm0)

traceplot(fm0@stanfit) + 
  xlim(c(5000, 5005))

post <- extract.samples(fm0) %>% as.data.frame()

precis(fm0)

post <- extract.samples(fm0)

pp_data <- sim(fm0)

data_list <- list(
  log_Mass = mammals_s$log_Mass,
  log_Forearm = mammals_s$log_Forearm
)

fm1 <- map2stan(
  alist(
    log_Forearm ~ dnorm(mu, sigma),
    mu <- a + bMass * log_Mass,
    log_Mass ~ dnorm(nu, sigma_N),
    a ~ dnorm(0, 10),
    bMass ~ dnorm(0, 5),
    nu ~ dcauchy(0.5, 4),
    sigma_N ~ dcauchy(0, 1),
    sigma ~ dcauchy(0, 1)
  ) ,
  data = data_list,
  iter = 1e4,
  chains = 2
)

load("fm1.Rda")

precis(fm1, digits = 4)
traceplot(fm1@stanfit, pars = c("a", "bMass", "nu"))

#######

M <- mammals_s %>% 
  select(log_Mass, log_Forearm) %>% 
  drop_na() %>% 
  as.data.frame()

data_list <- list(
  log_Mass = M$log_Mass,
  log_Forearm = M$log_Forearm
)

fm2 <- rethinking::resample(fm1, data = data_list)

