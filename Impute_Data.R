library("rethinking")
library("tidyverse")
library("cowplot")

mammals <- read_delim("~/Dropbox/Classes/Topics_Biostats/data/mammals.txt", delim = "\t")
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

data_list <- list(
  log_Mass = mammals_s$log_Mass,
  log_Forearm = mammals_s$log_Forearm
)

##
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

