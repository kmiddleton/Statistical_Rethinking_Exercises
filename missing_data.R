library(rethinking)
library(tidyverse)
library(cowplot)


set.seed(4328746)
rainfall <- c(runif(20, 100, 160),
              runif(20, 160, 210)) 
presence <- c(rbinom(20, 1, 0.15),
              rbinom(20, 1, 0.95))
LogReg <- data.frame(Rainfall = rainfall,
                     Presence = as.factor(presence))
head(LogReg)

fm <- glm(Presence ~ Rainfall,
          data = LogReg,
          family = "binomial")

library(epiDisplay)
logistic.display(fm)
ggplot(LogReg, aes(x = Rainfall,
                   y = as.numeric(Presence) - 1)) + 
  geom_hline(yintercept = 0.5, linetype = "dotted", size = 0.5) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE, size = 2) + 
  geom_point(size = 3) +
  ylab("Probability of Presence") +
  xlab("Rainfall (cm/y)") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))

m_complete <- map2stan(
  alist(
    Presence ~ dbinom(p, lambda),
    logit(p) <- ap + b * Rainfall,
    log(lambda) <- al,
    ap ~ dnorm(0, 1),
    al ~ dnorm(0, 10)
  ) ,
  data = LogReg )

precis(m11.4_stan)

post <- extract.samples(m11.4_stan)

logistic(median(post$ap))  # probability drink
exp(median(post$al))       # rate finish manuscripts, when not drinking
