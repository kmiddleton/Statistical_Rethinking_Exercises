## R code 2.6
library(rethinking)
globe.qa <- rethinking::map(
  alist(
    w ~ dbinom(5000,p) ,  # binomial likelihood
    p ~ dbeta(17,10)     # uniform prior
  ) ,
  data=list(w=2000) )

# display summary of quadratic approximation
precis( globe.qa, digits = 7 )

## R code 2.7
# analytical calculation
w <- 6
n <- 9
curve( dbeta( x , w+1 , n-w+1 ) , from=0 , to=1 )
# quadratic approximation
curve( dnorm( x , 0.67 , 0.16 ) , lty=2 , add=TRUE )

##################################################

library(tidyverse)
library(cowplot)

(s <- seq(0, 1, length.out = 200))
(max_beta <- s[which.max(dbeta(s, w + 1, n - w + 1))])
(max_norm <- s[which.max(dnorm(s, 0.67, 0.16))])

tibble(x = seq(0, 1, length.out = 200),
       beta = dbeta(x, 17, 10)) %>% 
  ggplot(aes(x, beta)) +
  geom_line()

tibble(x = seq(0, 1, length.out = 200),
       beta = dbeta(x, 1 + 6, 1 + 3),
       norm = dnorm(x, 0.67, 0.18)) %>%
  gather(key = "d function", value = "density", -x) %>% 
  ggplot(aes(x, y = density, color = `d function`)) +
  geom_line() +
  geom_vline(xintercept = max_beta, color = "red") +
  geom_vline(xintercept = max_norm, color = "blue", linetype = "dashed")



