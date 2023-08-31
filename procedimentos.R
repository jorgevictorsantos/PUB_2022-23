# packages utilizados --------
library(pacman)
pacman::p_load(tidyverse, PNADcIBGE, pollster, matrixStats, descr)

# O que vamos fazer?

# 1. Abertura das bases e seleção das variáveis

# Abertura das PNAD

vars <- c(
  'Ano',
  #
  'Trimestre',
  #
  'UF',
  #
  'Capital',
  #
  'UPA',
  #
  'Estrato',
  #
  'V1008',
  #Número do domicílio
  'V1014',
  #Painel
  'V1022',
  #Situação do domicílio
  'V1023',
  #Tipo de área
  'V1032',
  #
  'V2005',
  ##Condição no domicílio
  'V2007',
  ##Sexo
  'V20082',
  ##Ano de nascimento
  'V2009',
  ##Idade do morador na data de referência
  'V2010',
  ##Cor ou raça
  
  ### Educação
  
  'V3009A',
  ##Qual foi o curso mais elevado que ... frequentou anteriormente? -  4º tri/2015 - atual
  
  'V3014',
  ##concluiu este curso que frequentou anteriormente
  
  'VD3004',
  ##Nível de instrução mais elevado alcançado (pessoas de 5 anos ou mais de idade) padronizado para o Ensino fundamental -  SISTEMA DE 9 ANOS
  
  'VD3005',
  ##Anos de estudo (pessoas de 5 anos ou mais de idade) padronizado para o Ensino fundamental - SISTEMA DE 9 ANOS
  
  # Ocupação
  'V4009',
  ##Quantos trabalhos ... tinha na semana de ... a ... (semana de referência ?
  'V4010',
  ##Código da ocupação (cargo ou função)
  
  'V4012',
  ##Nesse trabalho, ... era: (posição na ocupação)
  
  'V4013',
  ##Código da principal atividade desse negócio/empresa
  
  'V4014',
  ##Esse trabalho era na área:
  
  'V4016',
  ##Na semana de ... a ... (semana de referência), quantos empregados trabalhavam nesse negócio/empresa que ... tinha ?
  
  'V4017',
  ##Na semana de ... a ... (semana de  referência), ... tinha pelo menos um sócio que trabalhava nesse negócio/empresa ?
  
  'V4018',
  ##Na semana de ... a ... (semana de referência), contando com ... , quantas pessoas trabalhavam nesse negócio/empresa ?
  
  'V4028',
  ##Nesse trabalho, ... era servidor público estatutário (federal, estadual ou municipal)?
  
  'V4029',
  ##Nesse trabalho, ... tinha carteira de trabalho assinada ?
  
  'V403311',
  ##Número da faixa do rendimento/retirada em dinheiro
  
  'V403312',
  ##Qual era o rendimento bruto/retirada mensal que ... recebia/fazia normalmente nesse trabalho ? (valor em dinheiro)
  
  'VD4001',
  ##Condição em relação à força de trabalho na semana de referência para pessoas de 14 anos ou mais de idade
  
  'VD4002',
  ##Condição de ocupação na semana de referência para pessoas de 14 anos ou mais de idade
  
  'VD4003',
  ##Força de trabalho potencial para pessoas de 14 anos ou mais de idade
  
  'VD4007',
  ##Posição na ocupação no trabalho principal da semana de referência para pessoas de 14 anos ou mais de idade
  
  'VD4008',
  ##Posição na ocupação no trabalho principal da semana de referência para pessoas de 14 anos ou mais de idade (com subcategorias de empregados)
  
  'VD4009',
  ##Posição na ocupação e categoria do emprego do trabalho principal da semana de referência para pessoas de 14 anos ou mais de idade
  
  'VD4016',
  ##Rendimento mensal habitual do trabalho principal para pessoas de 14 anos ou mais de idade (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias no trabalho principal)
  
  'VD4017'
  ##Rendimento mensal efetivo do trabalho principal para pessoas de 14 anos ou mais de idade (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias no trabalho principal)
)

pnad16 <- PNADcIBGE::get_pnadc(year = 2016, 
                               interview = 1,
                               design = F)

# recortes das variáveis e recorte das pessoas na força de trabalho, sejam 
# ocupadas ou desocupadas
pnad16 <- pnad16 %>% 
  select(vars) %>% 
  filter(VD4001 == "Pessoas na força de trabalho")

glimpse(pnad16)

# 2. Recorte da fração de classe dos intermediários culturais pela variável ocupação
# OCUPAÇÃO PRINCIPAL

# base das ocupações
ocupacoesCOD <- readxl::read_excel('Estrutura_Ocupacao_COD_limpa.xls',col_names = T)

ocupacoesCOD <- ocupacoesCOD %>% 
  dplyr::filter(!is.na(codigo))

pnad16 <- left_join(x = pnad16,
                        y = ocupacoesCOD,
                        by = c('V4010' = 'codigo'))

pnad16 <- pnad16 %>%
  mutate(interCultural = case_when(V4010 == '1222' ~ 1,
                                   V4010 == '2161' ~ 1,
                                   V4010 == '2162' ~ 1,
                                   V4010 == '2163' ~ 1,
                                   V4010 == '2166' ~ 1,
                                   V4010 == '2265' ~ 1,
                                   V4010 == '2431' ~ 1,
                                   V4010 == '2432' ~ 1,
                                   V4010 == '2621' ~ 1,
                                   V4010 == '2642' ~ 1,
                                   V4010 == '2643' ~ 1,
                                   V4010 == '2656' ~ 1,
                                   V4010 == '3230' ~ 1,
                                   V4010 == '3423' ~ 1,
                                   V4010 == '3431' ~ 1,
                                   V4010 == '3432' ~ 1,
                                   V4010 == '3433' ~ 1,
                                   V4010 == '3434' ~ 1,
                                   V4010 == '3435' ~ 1,
                                   V4010 == '5113' ~ 1,
                                   .default = 0),
         interCultural2 = case_when(interCultural == 0 ~ 'Não',
                                    interCultural == 1 ~ 'Sim')) %>% 
  rename(ocupacao = titulo)
  
pollster::crosstab(pnad16,
                   interCultural,
                   interCultural2,
                   V1032)

descr::freq(pnad16$interCultural, w = pnad16$V1032)

# 1.352.662 de pessoas nas ocupações de intermediários culturais

# 3. Contingência com sexo, cor/raça e idade -- no geral, nas ocupações e na média da classe
# sexo e cor/raça --> tabelas de contingência
# mediana e média de idade

pnad16 %>% 
  filter(interCultural == 1) %>% 
  pollster::crosstab(x = V4010,
                     y = V2007,
                     w = V1032,
                     unwt_n = T) %>%
  mutate(codigo = as.character(V4010)) %>% 
  left_join(x = .,
            y = ocupacoesCOD,
            by = 'codigo')

pnad16 %>% 
  filter(interCultural == 1) %>% 
  pollster::crosstab(x = V4010,
                     y = V200,
                     w = V1032,
                     unwt_n = T)


# 4. Participação do grupo na População Economicamente Ativa no Brasil
# n geral
# participação nas ocupações
# participação na classe

# 5. Participação do grupo na População Economicamente Ativa nos Estados
# n geral
# participação nas ocupações
# participação na categoria


# 6. Situação empregatícia - Posição na ocupação; se empregado, era carteira 
# assinada e quantos empregados na empresa? se conta própria ou empregado, 
# quantos empregados e sócios?
# no geral
# nas ocupações selecionadas
# na categoria

# 7. Principais áreas de atividade por ocupação e na média da classe
# 
# 

# 8. renda
# renda geral
# renda nas ocupações
# renda na categoria

# 9. educação
# geral com ensino médio completo, superior completo e pós-graduação completo
# nas ocupações
# na classe