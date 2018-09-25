##=============================================
##
##             Aula 2 ESPM
##
##=============================================

install.packages(c("tidyverse", "haven", "janitor",
                   "formattable"))


library(haven) # pacote para importar dados
library(tidyverse) # pacote para mexer nos dados
library(janitor) # pacote para sumarizar dados
library(formattable) # mudar valores para porcentagens


## abrir banco de dados =================================

## apontar a pasta (ctrl + shift + H)


dados_banco <- read_csv2("bank-full.csv")

## nomes das variáveis =================================

names(dados_banco)

## tipos das variáveis =================================

glimpse(dados_banco)

## sumário de todas as variáveis =======================

summary(dados_banco)


## Tirar proporção de variável categórica ================

dados_banco %>% 
  tabyl(job) 

## Arrumar segundo o n ==================================

dados_banco %>% 
  tabyl(job) %>% 
  arrange(-n)

## Comparar 2 variáveis categóricas =====================

dados_banco %>% 
  filter(job == "blue-collar") %>% 
  tabyl(job, education) %>% 
  adorn_crosstab()

## Comparar 2 variáveis categóricas =====================

dados_banco %>% 
  filter(job %in% c("blue-collar", "management")) %>% 
  tabyl(job, education) %>% 
  adorn_crosstab()


## ver sumário estatístico =============================

dados_banco %>% 
  select(age) %>% 
  summary(.)

dados_banco %>% 
  select(balance) %>% 
  summary(.)


## correlação entre duas variáveis ===================

dados_banco %>% 
  select(balance, age) %>% 
  cor(.)

