library(tidyverse)
library(rethinking)

data(chimpanzees)
d <- chimpanzees

## R code 10.2
m10.1 <- map(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a ,
    a ~ dnorm(0,10)
  ) ,
  data=d )
precis(m10.1)

logistic(coef(m10.1))

mean(d$pulled_left)

##

m10.2 <- map(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a + bp * prosoc_left ,
    a ~ dnorm(0,10) ,
    bp ~ dnorm(0,10)
  ) ,
  data=d )
precis(m10.2)
logistic(c(0.05, 0.05 + 0.56))

d %>% 
  group_by(prosoc_left) %>% 
  summarize(mean_pulled_left = mean(pulled_left))

##

logistic(-8)
logistic(-8) * 10000
logistic(-8 + 0.61) * 10000

##

d %>% 
  group_by(actor) %>% 
  summarize(mean_actor = mean(pulled_left))

