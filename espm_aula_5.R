##========================================
##
##          Aula 4: Predições
##
##========================================

## abrir pacotes

library(tidyverse)
library(haven)

## abrir base 
## antes crtl + shift + h para direcionar pasta

base_banco <- read_csv2("bank-full.csv")

## saber nomes das variáveis

names(base_banco)

## tipo das variáveis

glimpse(base_banco)

## intervalos de confiança

library(gmodels)

## calcular média ======================

mean(base_banco$age)

## calcular desvio-padrao =============

sd(base_banco$age)

## calcular intervalos de confiança ===

ci(base_banco$age, confidence = 0.95)

ci(base_banco$age, confidence = 0.99)

ci(base_banco$age, confidence = 0.90)

## testes de significância ===========

names(base_banco)

## criar variável jovens x outros
base_banco <- base_banco %>% 
  mutate(age2 = ifelse(age <= 35, "jovens", "outros"))

library(janitor)

## tabular relação entre idade e emprestimo
base_banco %>% 
  tabyl(age2, loan) 

## tabular relação entre idade e emprestimo
base_banco %>% 
  tabyl(age2, loan) %>% 
  adorn_crosstab()

## quem pede mais empréstimos, jovens ou outros?
chisq.test(base_banco$age2, base_banco$loan)

options(scipen = 999)

## tabular relação entre casamento e empréstimo
base_banco %>% 
  tabyl(marital, loan) %>% 
  adorn_crosstab()

## quem pede mais empréstimos, solteiros ou casados?
chisq.test(base_banco$marital, base_banco$loan)

## sera que salario de jovens e outros é o mesmo?
t.test(base_banco$balance ~ base_banco$age2)











