##===================================================
##
##       AULA 7 - BIG DATA
##
##===================================================

library(tidyverse)
library(foreign)
library(janitor)

## importar base do livro Marketing with R ==========

sat.df <- read.csv("http://goo.gl/HKnl74")

## nomes das variáveis ==============================

names(sat.df)

## weekend: se visitou no final de semana
## num.child: número de crianças que trouxe
## distance: distância percorrida até chegar ao parque
## overall: nível de satisfação geral
## rides: satisfação com os passeios
## games: satisfação com os jogos
## wait: satisfação com o tempo de espera
## clean: satisfação com a limpeza

## saber os tipos ==================================

glimpse(sat.df)


## sumarizar as variáveis ==========================

summary(sat.df)

## correlações ====================================

## install.packages("corrplot")
library(corrplot)

sat.df <- sat.df %>% 
  mutate(weekend = as.integer(as.factor(weekend)))  

res <- cor(sat.df)
round(res, 2)


corrplot(res,
         order = "hclust", 
         tl.col = "black", tl.srt = 45)



## regressões ====================================


mod1 <- lm(overall ~ rides, data = sat.df)
summary(mod1)

mod2 <- lm(overall ~ rides + clean, data = sat.df)
summary(mod2)

mod3 <- lm(overall ~ rides + clean + wait + games, data = sat.df)
summary(mod3)


## diagnósticos ===============================

library(sjPlot)

plot_model(mod1, type = "diag")

plot_model(mod2, type = "diag")

plot_model(mod3, type = "diag")

               