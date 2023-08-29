# pacote `survey()` e outros pacotes considerando peso amostral

library(PNADcIBGE)
library(survey)
library(matrixStats)
library(tidyverse)
library(pollster)
# install.packages('expss')
library(expss)
# install.packages('questionr')
library(questionr)



# Utilizando survey
variaveis_selecionadas <- c("UF","V1027","V1028",
                            "V2001","V2005","V2007","V2009","V2010",
                            "V3007","VD3004","VD4001","VD4002","VD4020",
                            "VD4035")

dadosPNADc <- get_pnadc(year=2017, quarter=4, vars=variaveis_selecionadas)

totalsexo <- svytotal(x=~V2007, design=dadosPNADc, na.rm=TRUE)
totalsexo

#               total    SE
# V2007Homem  101168151 8e-04
# V2007Mulher 105660589 8e-04

# media de var. continua
mediarenda <- svymean(x=~VD4020, design=dadosPNADc, na.rm=TRUE)
# 2182.6



# utilizando outros pacotes

# table de var. categorias
dadosPNADc <- get_pnadc(year = 2017, quarter = 4, 
                        vars = variaveis_selecionadas,
                        design = F)


expss::fre(dadosPNADc$V2007, weight = dadosPNADc$V1028)
# ou
wtd.table(x = dadosPNADc$V2007, weights = dadosPNADc$V1028)


# media de var continua
dadosPNADc %>% 
  reframe(mediaRenda = weightedMean(x = VD4020, w = V1028, na.rm = T))
# 2183


### CONSIDERAR SEMPRE A VARIÁVEL V1028, O PESO AMOSTRAL COM A ESTRATIFICAÇÃO PÓS COLETA

