##==================================
##
##      Aula 6 - Big Data
##
##==================================

##======================================
## Tema da aula de hoje: Regressões
##======================================

library(tidyverse)
library(janitor)
library(car)


## abrir base ========================================

duncan <- data.frame(Duncan)


## olhar o tipo dos dados ============================

glimpse(duncan)


## sumarizar os dados ================================

summary(duncan)


## correlação entre prestígio e salário ==============

duncan %>%
  select(prestige, income) %>% 
  cor(.)


## regressão linear simples =========================

modelo1 <- lm(prestige ~ income, data = duncan)

summary(modelo1)


## simular magnitude do efeito

library(sjPlot)

plot_model(modelo1, type = "eff", 
           terms = "income")















