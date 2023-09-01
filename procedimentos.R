# packages utilizados --------
pacman::p_load(tidyverse, PNADcIBGE, pollster, matrixStats,
               descr, expss,cowplot,readxl,writexl,priceR)

# rm(list = ls())
# gc()

# variáveis utilizadas da PNAD ----
vars <- c(
  'Ano','Trimestre','UF','Capital','UPA','Estrato',
  'V1008',
  #Número do domicílio
  'V1014',
  #Painel
  'V1022',
  #Situação do domicílio
  'V1023',
  #Tipo de área
  'V1032',
  # peso
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
  'V3008',
  # estudou anteriormente?
  'V3009A',
  ##Qual foi o curso mais elevado que ... frequentou anteriormente? -  4º tri/2015 - atual
  'V3014',
  ##concluiu este curso que frequentou anteriormente
  'VD3004',
  ##Nível de instrução mais elevado alcançado (pessoas de 5 anos ou mais de idade) padronizado para o Ensino fundamental -  SISTEMA DE 9 ANOS
  
  # Ocupação
  'V4010',
  ##Código da ocupação (cargo ou função)
  
  'VD4001',
  ##Condição em relação à força de trabalho na semana de referência para pessoas de 14 anos ou mais de idade
  
  'VD4002',
  ##Condição de ocupação na semana de referência para pessoas de 14 anos ou mais de idade
  
  'VD4007',
  ##Posição na ocupação no trabalho principal da semana de referência para pessoas de 14 anos ou mais de idade
  
  'VD4008',
  ##Posição na ocupação no trabalho principal da semana de referência para pessoas de 14 anos ou mais de idade (com subcategorias de empregados)
  
  'VD4009',
  ##Posição na ocupação e categoria do emprego do trabalho principal da semana de referência para pessoas de 14 anos ou mais de idade
  
  'VD4010',
  # atividades
  
  'VD4016',
  ##Rendimento mensal habitual do trabalho principal para pessoas de 14 anos ou mais de idade (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias no trabalho principal)
  
  'CO2'
  # deflator para rendimento habitual para o último ano
)

anos <- c(2016,2017,2018,2019,2022)

# loop completo  -----

for (i in anos) {
  cat('Estamos trabalhando agora na PNAD:', i, '\n')
  
  # 1. Abertura das bases e seleção das variáveis ----
  
  # Abertura das PNAD
  
  cat('Processo de abertura e transformações da base Pnad',i, '\n')
  
  df <- get_pnadc(year = i, 
                  interview = 1,
                  design = F,
                  defyear = 2022)
  
  # recortes das variáveis e recorte das pessoas na força de trabalho, sejam 
  # ocupadas ou desocupadas
  
  df <- df %>% 
    select(all_of(vars)) %>% 
    filter(VD4001 == "Pessoas na força de trabalho")
  
  #estrutura cod
  ocupacoesCOD <- read_excel('Estrutura_Ocupacao_COD_limpa.xls',col_names = T)
  
  ocupacoesCOD <- ocupacoesCOD %>% 
    dplyr::filter(!is.na(codigo))
  
  # 2. Recorte da fração de classe dos intermediários culturais pela variável ocupação ----
  # SOMENTE A OCUPAÇÃO PRINCIPAL
  
  # base das ocupações
  
  df <- left_join(x = df,
                  y = ocupacoesCOD,
                  by = c('V4010' = 'codigo'))
  
  df <- df %>%
    mutate(interCultural = case_when(V4010 == '2163' ~ 1,
                                     V4010 == '2166' ~ 1,V4010 == '2265' ~ 1,
                                     V4010 == '2431' ~ 1,V4010 == '2432' ~ 1,
                                     V4010 == '2621' ~ 1,V4010 == '2642' ~ 1,
                                     V4010 == '2656' ~ 1,V4010 == '3423' ~ 1,
                                     V4010 == '3431' ~ 1,V4010 == '3432' ~ 1,
                                     V4010 == '3433' ~ 1,V4010 == '3434' ~ 1,
                                     V4010 == '3435' ~ 1,V4010 == '5113' ~ 1,
                                     .default = 0),
           interCultural2 = case_when(interCultural == 0 ~ 'Não',
                                      interCultural == 1 ~ 'Sim'),
           raca2 = case_when(V2010 == 'Branca' ~ 'Brancos',
                             V2010 == 'Preta' ~ 'Negros',
                             V2010 == 'Parda' ~ 'Negros',
                             V2010 == 'Indígena' ~ NA,
                             V2010 == 'Amarela' ~ 'Amarelos'),
           rendimento_mensal_hab_2022_IBGE = VD4016 * CO2,
           ensMedio = if_else((VD3004 == 'Médio completo ou equivalente' 
                               | VD3004 == 'Superior incompleto ou equivalente' 
                               | VD3004 == 'Superior completo'), 
                              'EM Completo', 'EM Incompleto'),
           ensSuperior = if_else((VD3004 == 'Superior completo'),
                                 'ES Completo', 'ES Incompleto'),
           posgraduacao = if_else(((V3009A == 'Especialização de nível superior'
                                   | V3009A == 'Mestrado'
                                   | V3009A == 'Doutorado') 
                                   & V3014 == 'Sim' & V3008 == 'Sim'),
                                  'Tem Pós-graduação completa', 'Não tem'),
           posgrad_lato = if_else((V3009A == 'Especialização de nível superior' 
                                   & V3014 == 'Sim'& V3008 == 'Sim'),
                                  'Tem pós-graduacao lato completa', 'Nao tem'),
           posgrad_stricto = if_else(((V3009A == 'Mestrado'
                                       | V3009A == 'Doutorado') 
                                      & V3014 == 'Sim' & V3008 == 'Sim'),
                                     'Tem Pós-graduação stricto completa', 'Não tem')
           )
  
  cat('Feitas as transformações e recategorizações da Pnad:', i, '\n')
  
  df_ocupados <- df %>% 
    filter(VD4002 == 'Pessoas ocupadas') %>% 
    mutate(n_ocupados = sum(V1032))
  
  grupo <- c('não é inter cult', 'inter cult', 'total')
  n_amostra <- descr::freq(df$interCultural2)
  n_amostraPeso <- descr::freq(df$interCultural2, w = df$V1032)
  
  amostra <- data.frame(grupo, n_amostra, n_amostraPeso) 
  
  amostra <- amostra %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  assign(paste0('amostra_Pnad',i), amostra)
  
  write_xlsx(amostra, paste0('amostraPnad',i,'.xlsx'))
  
  cat('Descritiva da amostra da pnad', i,'está pronta', '\n')
  
  # 3. Contingência sexo, cor/raça e idade -----
  # sexo e cor/raça --> tabelas de contingência
  # mediana e média de idade
  
  ### sexo ----
  # sexo no geral da PEA
  
  sexoGeral <- pollster::crosstab(df = df,
                     x = VD4001,
                     y = V2007,
                     w = V1032,
                     unwt_n = T) %>%
    mutate(ano = i) %>% 
    mutate(across(where(is.numeric), round, digits = 2))
  
  assign(paste0('sexoGeral_Pnad',i), sexoGeral)
  
  write_xlsx(sexoGeral, paste0('descr_sexo_geral_Pnad',i,'.xlsx'))
  
  # sexo da classe
  
  sexoClasse <- df %>%  
    pollster::crosstab(x = interCultural2,
                       y = V2007,
                       w = V1032,
                       unwt_n = T) %>%
    filter(interCultural2 == 'Sim') %>% 
    mutate(across(where(is.numeric), round, digits = 2))
  
  assign(paste0('sexoClasse_Pnad',i), sexoClasse)
  
  write_xlsx(sexoClasse, paste0('descr_sexo_classe_Pnad',i,'.xlsx'))
  
  
  # sexo das ocupações da classe
  
  sexoOcupClasse <- df %>% 
    filter(interCultural == 1) %>% 
    pollster::crosstab(x = V4010,
                       y = V2007,
                       w = V1032,
                       unwt_n = T) %>%
    mutate(codigo = as.character(V4010)) %>% 
    left_join(x = .,
              y = ocupacoesCOD,
              by = 'codigo') %>% 
    mutate(across(where(is.numeric), round, digits = 2))
  
  assign(paste0('sexoOcupClasse_Pnad',i), sexoOcupClasse)
  
  write_xlsx(sexoOcupClasse, paste0('descr_sexo_ocup_classe_Pnad',i,'.xlsx'))
  
  cat('Descritivas de sexo da pnad', i,'estão prontas', '\n')
  
  ### raca ----
  # raca geral

  racaGeral <- pollster::crosstab(df = df,
                     x = VD4001,
                     y = raca2,
                     w = V1032,
                     unwt_n = T) %>% 
    mutate(ano = i) %>% 
    mutate(across(where(is.numeric), round, digits = 2))
  
  assign(paste0('racaGeral_Pnad',i), racaGeral)
  
  write_xlsx(racaGeral, paste0('descr_raca_geral_Pnad',i,'.xlsx'))
  
  # raca na classe
  
  racaClasse <- df %>%  
    pollster::crosstab(x = interCultural2,
                       y = raca2,
                       w = V1032,
                       unwt_n = T) %>%
    filter(interCultural2 == 'Sim') %>% 
    mutate(across(where(is.numeric), round, digits = 2))
  
  assign(paste0('racaClasse_Pnad',i), racaClasse)
  
  write_xlsx(racaClasse, paste0('descr_raca_classe_Pnad',i,'.xlsx'))
  
  
  # raca nas ocupações
  
  racaOcupClasse <- df %>% 
    filter(interCultural == 1) %>% 
    pollster::crosstab(x = V4010,
                       y = raca2,
                       w = V1032,
                       unwt_n = T) %>%
    mutate(codigo = as.character(V4010)) %>% 
    left_join(x = .,
              y = ocupacoesCOD,
              by = 'codigo') %>% 
    mutate(across(where(is.numeric), round, digits = 2))
  
  assign(paste0('racaOcupClasse_Pnad',i), racaOcupClasse)
  
  write_xlsx(racaOcupClasse, paste0('descr_raca_ocup_classe_Pnad',i,'.xlsx'))
  
  cat('Descritivas de raça da pnad', i,'estão prontas', '\n')
  
  
  ### idade ----
  
  # geral
  idadeGeral <- df %>% 
    reframe(mediaIdadeGeral = weightedMean(x = V2009, w = V1032),
            medianaIdadeGeral = weightedMedian(x = V2009, w = V1032)) %>% 
    mutate(across(where(is.numeric), round, digits = 2)) %>% 
    mutate(ano = i)
  
  
  # na classe dos inter cultural
  idadeInterCult <- df %>% 
    filter(interCultural == 1) %>% 
    reframe(mediaIdadeInterCult = weightedMean(x = V2009, w = V1032),
            medianaIdadeInterCult = weightedMedian(x = V2009, w = V1032)) %>%
    mutate(across(where(is.numeric), round, digits = 2),
           ano = i) 
  
  
  assign(paste0('idade_Pnad',i),
         data.frame(idadeGeral, idadeInterCult))
  
  write_xlsx(data.frame(idadeGeral, idadeInterCult), paste0('IdadeGeral',i,'.xlsx'))
  
  cat('Descritivas de idade da pnad', i,'estão prontas', '\n')
  
  
  # 4. Participação do grupo na População Ocupada no Brasil -----
  
  # Ocupados no Geral
  ocupadosGeral <- pollster::crosstab(df,
                     x = VD4001,
                     y = VD4002,
                     weight = V1032,
                     unwt_n = T) %>% 
    mutate(ano = i) %>% 
    mutate(across(where(is.numeric), round, digits = 2))
  
  assign(paste0('ocupadosGeral_Pnad',i),ocupadosGeral)
  
  write_xlsx(ocupadosGeral, paste0('ocupadosGeralPnad',i,'.xlsx'))
  
  # Ocupados na classe
  ocupadosClasse <- df %>% 
    pollster::crosstab(x = interCultural,
                       y = VD4002,
                       weight = V1032,
                       unwt_n = T) %>% 
    filter(interCultural == 1) %>% 
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  assign(paste0('ocupadosClasse_Pnad',i),ocupadosClasse)
  
  write_xlsx(ocupadosClasse, paste0('ocupadosClassePnad',i,'.xlsx'))
  
  
  # Porcentagem da classe no total de ocupados
  pct_classe_PEA <- df_ocupados %>% 
    group_by(interCultural2, n_ocupados) %>%
    reframe(n_ocupados2 = sum(V1032)) %>% 
    mutate(pct_ocupados = n_ocupados2/n_ocupados) %>% 
    filter(interCultural2 == 'Sim') %>% 
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  assign(paste0('ClassePEA_Pnad',i),pct_classe_PEA)
  
  write_xlsx(pct_classe_PEA, paste0('pct_classe_PEA_Pnad',i,'.xlsx'))
  
  
  cat('Tabelas de contingência da PEA da pnad', i,'estão prontas', '\n')
  
  
  # 5. Situação empregatícia - Posição na ocupação ----
  
  ### posição na ocupação ----
  
  # geral
  
  posiOcupGeral <- pollster::crosstab(df,
                                      x = VD4001,
                                      y = VD4009,
                                      w = V1032) %>%
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  assign(paste0('posiOcupGeral_Pnad',i),posiOcupGeral)
  
  write_xlsx(posiOcupGeral, paste0('posiOcupGeral_PNAD',i,'.xlsx'))
  
  # na categoria
  
  posiOcupClasse <- df %>%
    pollster::crosstab(x = interCultural2,
                       y = VD4009,
                       w = V1032,
                       unwt_n = T) %>% 
    filter(interCultural2 == 'Sim') %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  assign(paste0('posiOcupClasse_Pnad',i),posiOcupClasse)
  
  write_xlsx(posiOcupClasse, paste0('posiOcupClassePNAD',i,'.xlsx'))
  
  # nas ocupações selecionadas
  
  posiOcupClasseOcupacoes <- df %>% 
    filter(interCultural == 1) %>% 
    pollster::crosstab(x = V4010,
                       y = VD4009,
                       w = V1032,
                       unwt_n = T) %>% 
    mutate(codigo = as.character(V4010),
           ano = i) %>% 
    left_join(x = .,
              y = ocupacoesCOD,
              by = 'codigo') %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  assign(paste0('posiOcupClasseOcupacoes_Pnad',i),posiOcupClasse)
  
  write_xlsx(posiOcupClasseOcupacoes, paste0('posiOcupClasseOcupacoesPNAD',i,'.xlsx'))
  
  
  cat('Tabelas de contingência da posição na ocupação da pnad', i,'estão prontas', '\n')
  
  
  
  # 6. Distribuição nas áreas de atividade da classe -----
  # grupo maior da CNAE
  
  # geral
  distCNAEgeral <- df %>%
    pollster::crosstab(x = VD4001,
                       y = VD4010,
                       w = V1032,
                       unwt_n = T) %>% 
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  assign(paste0('distCNAEgeral',i),distCNAEgeral)
  
  write_xlsx(distCNAEgeral, paste0('distCNAEgeral_PNAD',i,'.xlsx'))

  
  # categoria
  
  distCNAEclasse <- df %>%
    pollster::crosstab(x = interCultural,
                       y = VD4010,
                       w = V1032,
                       unwt_n = T) %>% 
    filter(interCultural == 1) %>% 
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  assign(paste0('distCNAEclasse',i),distCNAEclasse)
  
  write_xlsx(distCNAEclasse, paste0('distCNAEclasse_PNAD',i,'.xlsx'))
  
  
  cat('Tabelas de contingência da CNAE da pnad', i,'estão prontas', '\n')
  
  
  
  # 7. renda -----
  # renda geral
  
  rendaGeral <- df %>% 
    reframe(V4010 = NA,
            titulo = NA, 
            grupo = 'geral',
            mediaRenda = weightedMean(rendimento_mensal_hab_2022_IBGE, w = V1032, na.rm = T),
            medianaRenda = weightedMedian(rendimento_mensal_hab_2022_IBGE,w = V1032, na.rm = T),
            desvioRenda = weightedSd(rendimento_mensal_hab_2022_IBGE,w = V1032, na.rm = T),
            nAmostra = n(),
            nPeso = sum(V1032),
            ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  write_xlsx(rendaGeral, paste0('rendaGeral_PNAD',i,'.xlsx'))
  
  # renda na categoria
  rendaClasse <- df %>%
    filter(interCultural == 1) %>% 
    reframe(V4010 = NA,
            titulo = NA, 
            grupo = 'Intermediários culturais',
            mediaRenda = weightedMean(rendimento_mensal_hab_2022_IBGE, w = V1032, na.rm = T),
            medianaRenda = weightedMedian(rendimento_mensal_hab_2022_IBGE,w = V1032, na.rm = T),
            desvioRenda = weightedSd(rendimento_mensal_hab_2022_IBGE,w = V1032, na.rm = T),
            nAmostra = n(),
            nPeso = sum(V1032),
            ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  write_xlsx(rendaClasse, paste0('rendaClasse_PNAD',i,'.xlsx'))
  
  
  # renda nas ocupações
  rendaOcupacoes <- df %>% 
    filter(interCultural == 1) %>% 
    group_by(V4010, titulo) %>% 
    reframe(grupo = 'ocupações da categoria',
            mediaRenda = weightedMean(rendimento_mensal_hab_2022_IBGE, w = V1032, na.rm = T),
            medianaRenda = weightedMedian(rendimento_mensal_hab_2022_IBGE, w = V1032, na.rm = T),
            desvioRenda = weightedSd(rendimento_mensal_hab_2022_IBGE, w = V1032, na.rm = T),
            nAmostra = n(),
            nPeso = sum(V1032),
            ano = i)
  
  assign(paste0('rendaPnad',i), rbind(rendaGeral, rendaClasse, rendaOcupacoes))
  
  write_xlsx(rendaOcupacoes, paste0('rendaOcupacoes_PNAD',i,'.xlsx'))
  
  
  cat('Descritivas de renda da pnad', i,'estão prontas', '\n')
  
  
  
  # 8. educação ----
  # geral com ensino médio completo, superior completo e pós-graduações completas
  
  ensMedioGeral <- df %>% 
    pollster::crosstab(x = VD4001,
             y = ensMedio,
             w = V1032,
             unwt_n = T) %>% 
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  ensSuperiorGeral <- df %>% 
    pollster::crosstab(x = VD4001,
             y = ensSuperior,
             w = V1032,
             unwt_n = T) %>% 
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  posgradGeral <- df %>% 
    pollster::crosstab(x = VD4001,
             y = posgraduacao,
             w = V1032,
             unwt_n = T) %>% 
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  posGradLatoGeral <- df %>% 
    pollster::crosstab(x = VD4001,
             y = posgrad_lato,
             w = V1032,
             unwt_n = T) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  posGradStrictoGeral <- df %>% 
    pollster::crosstab(x = VD4001,
             y = posgrad_stricto,
             w = V1032,
             unwt_n = T) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  escolaridadeGeral <- cbind(ensMedioGeral, ensSuperiorGeral, posgradGeral,
        posGradLatoGeral, posGradStrictoGeral)
  
  assign(paste0('escolaridadeGeralPnad',i), cbind(ensMedioGeral, ensSuperiorGeral, posgradGeral,
                                      posGradLatoGeral, posGradStrictoGeral))
  
  write_xlsx(escolaridadeGeral, paste0('escolaridadeGeral_PNAD',i,'.xlsx'))
  

  # na categoria
  ensMedioClasse <- df %>% 
    pollster::crosstab(x = interCultural,
             y = ensMedio,
             w = V1032,
             unwt_n = T) %>% 
    filter(interCultural == 1) %>% 
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  ensSupClasse <- df %>% 
    pollster::crosstab(x = interCultural,
             y = ensSuperior,
             w = V1032,
             unwt_n = T)%>% 
    filter(interCultural == 1) %>% 
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  posgradClasse <- df %>% 
    pollster::crosstab(x = interCultural,
             y = posgraduacao,
             w = V1032,
             unwt_n = T)%>% 
    filter(interCultural == 1) %>% 
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  posGradLatoClasse <- df %>% 
    pollster::crosstab(x = interCultural,
             y = posgrad_lato,
             w = V1032,
             unwt_n = T)%>% 
    filter(interCultural == 1) %>% 
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  posGradStrictoClasse <- df %>% 
    pollster::crosstab(x = interCultural,
             y = posgrad_stricto,
             w = V1032,
             unwt_n = T)%>% 
    filter(interCultural == 1) %>% 
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  escolaridadeClasse <- cbind(ensMedioClasse, ensSupClasse, posgradClasse,
                             posGradLatoClasse, posGradStrictoClasse)
  
  assign(paste0('escolaridadeClassePnad',i), escolaridadeClasse)
  
  write_xlsx(escolaridadeClasse, paste0('escolaridadeClasse_PNAD',i,'.xlsx'))
  
  
  # nas ocupações
  
  ensMedioOcupacoes <- df %>%
    filter(interCultural == 1) %>% 
    pollster::crosstab(x = V4010,
                       y = ensMedio,
                       w = V1032,
                       unwt_n = T) %>%
    mutate(codigo = as.character(V4010)) %>% 
    left_join(x = .,
              y = ocupacoesCOD,
              by = 'codigo') %>% 
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  ensSuperiorOcupacoes <- df %>%
    filter(interCultural == 1) %>% 
    pollster::crosstab(x = V4010,
                       y = ensSuperior,
                       w = V1032,
                       unwt_n = T) %>%
    mutate(codigo = as.character(V4010)) %>% 
    left_join(x = .,
              y = ocupacoesCOD,
              by = 'codigo') %>% 
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  posgradOcupacoes <- df %>%
    filter(interCultural == 1) %>% 
    pollster::crosstab(x = V4010,
                       y = posgraduacao,
                       w = V1032,
                       unwt_n = T) %>%
    mutate(codigo = as.character(V4010)) %>% 
    left_join(x = .,
              y = ocupacoesCOD,
              by = 'codigo') %>% 
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  posGradLatoOcupacoes <- df %>%
    filter(interCultural == 1) %>% 
    pollster::crosstab(x = V4010,
                       y = posgrad_lato,
                       w = V1032,
                       unwt_n = T) %>%
    mutate(codigo = as.character(V4010)) %>% 
    left_join(x = .,
              y = ocupacoesCOD,
              by = 'codigo') %>% 
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  posGradStrictoOcupacoes <- df %>%
    filter(interCultural == 1) %>% 
    pollster::crosstab(x = V4010,
                       y = posgrad_stricto,
                       w = V1032,
                       unwt_n = T) %>%
    mutate(codigo = as.character(V4010)) %>% 
    left_join(x = .,
              y = ocupacoesCOD,
              by = 'codigo') %>% 
    mutate(ano = i) %>%
    mutate(across(where(is.numeric), round, digits = 2))

  escolaridadeOcupacoes <- cbind(ensMedioOcupacoes, ensSuperiorOcupacoes, 
                                 posgradOcupacoes, posGradLatoOcupacoes, 
                                 posGradStrictoOcupacoes)
  
  assign(paste0('escolaridadeOcupacoes_Pnad',i), escolaridadeOcupacoes)
  
  write_xlsx(escolaridadeOcupacoes, paste0('escolaridadeOcupacoes_PNAD',i,'.xlsx'))
  
  
  cat('Tabelas de contingência da escolaridade da pnad', i,'estão prontas', '\n')
  
  cat('Os procedimentos da Pnad', i, 'estão prontos', '\n')
  
  cat('Dataframes do loop apagados', '\n')
  
  rm(df, df_ocupados, ocupacoesCOD)

}


# 9 - Agregações dos resultados -----


### amostra ----

amostra_Pnad2016$ano <- 2016
amostra_Pnad2017$ano <- 2017
amostra_Pnad2018$ano <- 2018
amostra_Pnad2019$ano <- 2019
amostra_Pnad2022$ano <- 2022

rbind(amostra_Pnad2016,
      amostra_Pnad2017, amostra_Pnad2018, amostra_Pnad2019, amostra_Pnad2022)

amostraGeral <- rbind(amostra_Pnad2016,
                      amostra_Pnad2017, amostra_Pnad2018, amostra_Pnad2019, amostra_Pnad2022)


write_xlsx(amostraGeral, 'amostraGeral.xlsx')


### sexo -----

sexoGeral <- rbind(sexoGeral_Pnad2016, sexoGeral_Pnad2017, sexoGeral_Pnad2018,
                   sexoGeral_Pnad2019, sexoGeral_Pnad2022)

sexoClasse_Pnad2016$ano <- 2016
sexoClasse_Pnad2017$ano <- 2017
sexoClasse_Pnad2018$ano <- 2018
sexoClasse_Pnad2019$ano <- 2019
sexoClasse_Pnad2022$ano <- 2022

sexoClasse <- rbind(sexoClasse_Pnad2016, sexoClasse_Pnad2017, sexoClasse_Pnad2018,
                    sexoClasse_Pnad2019, sexoClasse_Pnad2022)

sexoGeralClasse <- cbind(sexoGeral, sexoClasse)

write_xlsx(sexoGeralClasse, 'sexoGeralClasse.xlsx')


sexoOcupClasse_Pnad2016$ano <- 2016
sexoOcupClasse_Pnad2017$ano <- 2017
sexoOcupClasse_Pnad2018$ano <- 2018
sexoOcupClasse_Pnad2019$ano <- 2019
sexoOcupClasse_Pnad2022$ano <- 2022


sexoOcupClasse <- rbind(sexoOcupClasse_Pnad2016, sexoOcupClasse_Pnad2017,
                        sexoOcupClasse_Pnad2018,sexoOcupClasse_Pnad2019,
                        sexoOcupClasse_Pnad2022)

sexoOcupClasse <- as.data.frame(sexoOcupClasse)

class(sexoOcupClasse)

write_xlsx(sexoOcupClasse, 'sexoOcupClasse.xlsx')

### raça -----

racaGeral_Pnad2016

racaGeral <- rbind(racaGeral_Pnad2016,racaGeral_Pnad2017,racaGeral_Pnad2018,
                   racaGeral_Pnad2019,racaGeral_Pnad2022)

racaClasse_Pnad2016$ano  <- 2016
racaClasse_Pnad2017$ano  <- 2017
racaClasse_Pnad2018$ano  <- 2018
racaClasse_Pnad2019$ano  <- 2019
racaClasse_Pnad2022$ano  <- 2022

racaClasse <- rbind(racaClasse_Pnad2016,racaClasse_Pnad2017,racaClasse_Pnad2018,
                    racaClasse_Pnad2019, racaClasse_Pnad2022)

racaClasseGeral <- cbind(racaGeral, racaClasse)
  
write_xlsx(racaClasseGeral, 'racaClasseGeral.xlsx')
  
  
  
racaOcupClasse_Pnad2016$ano <- 2016
racaOcupClasse_Pnad2017$ano <- 2017
racaOcupClasse_Pnad2018$ano <- 2018
racaOcupClasse_Pnad2019$ano <- 2019
racaOcupClasse_Pnad2022$ano <- 2022


racaOcupClasse <- rbind(racaOcupClasse_Pnad2016,racaOcupClasse_Pnad2017,
                        racaOcupClasse_Pnad2018,racaOcupClasse_Pnad2019,
                        racaOcupClasse_Pnad2022)

write_xlsx(racaOcupClasse, 'racaOcupClasse.xlsx')
  

### idade

