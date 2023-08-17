source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

# Importar banco
#df  <- read_xlsx("C:/Users/Júlia G/Documents/ESTAT/Projetos/PF23003 - Projeto Vesta/df.xlsx")
df <- read_excel("banco/df.xlsx")


# Analisar gruppos a serem estudados
table(df$`GRUPO MÁSCARA`)

# Retirar dropout 
df <- df %>% filter(`GRUPO MÁSCARA`==1 | `GRUPO MÁSCARA`== 2) 

table(df$`Desfecho positivo (COVID-19) durante o acompanhamento?`)
table(df$`Desfecho positivo (Influenza) durante o acompanhamento?`)

# Formatar dados
df$`Desfecho positivo (Influenza) durante o acompanhamento?` <- ifelse(df$`Desfecho positivo (Influenza) durante o acompanhamento?`=="SIM",
                                                                       "Sim",df$`Desfecho positivo (Influenza) durante o acompanhamento?`)
table(df$`Desfecho positivo (Influenza) durante o acompanhamento?`)

# Verificar informações (ignorar)
df1 <- df %>% filter(`GRUPO MÁSCARA`==1)
df2 <- df %>% filter(`GRUPO MÁSCARA`==2)
 
table(df1[,30])
table(df1[,31])

table(df2[,30])
table(df2[,31])
  
# Dados em estudo (42 variaveis finais e grupo máscara)
data_j <- df[,c(2,42:82)]

table(data_j$`GRUPO MÁSCARA`)

# Transformar a variável em fator
data_j$`GRUPO MÁSCARA`<-factor(data_j$`GRUPO MÁSCARA`)
class(data_j$`GRUPO MÁSCARA`)

# Análise das variáveis para os testes
table(data_j$`Tomou a vacina contra a gripe (este ano)? (A2)`)
table(data_j$`Escala de Usabilidade da máscara- A1-Você trocou/emprestou a(s) sua(s) máscara(s) com algum colega em algum momento do estudo?`)
table(data_j$`Data (A1)`)#retira
table(data_j$`Como você descreveria o grau de calor e suor influenciado pela máscara, no seu rosto, durante o uso? (A1)`)
table(data_j$`Como você descreveria a facilidade para respirar, durante o uso da máscara? (A1)`)
table(data_j$`Qual foi o percentual de uso da máscara durante os plantões?  (Inserir número de 0 a 100)- (A1)`)
table(data_j$`Escala de usabilidade da máscara- A2- Data`)#retirar
table(data_j$`Você trocou/emprestou a(s) sua(s) máscara(s) com algum colega em algum momento do estudo?  (A2)`)
table(data_j$`Como você descreveria o grau de calor e suor influenciado pela máscara, no seu rosto, durante o uso? (A2)`)
table(data_j$`Desconforto Musculoesqueletico- Dominância manual`)
table(data_j$`Qual foi o percentual de uso da máscara durante os plantões?  (Inserir número de 0 a 100) - (A2)`)
table(data_j$`Alguma vez você sentiu desconforto (dor, formigamento, perda de força, etc) nos braços ou pescoço?`)
table(data_j$`Você teve desconforto na última semana (últimos sete dias)?  (nos locais indicados na figura abaixo)`)
table(data_j$`Durante o último mês, quantas horas de sono você teve por noite? (esta pode ser diferente do número de horas que você ficou na cama)`)#retirar 45052
table(data_j$`Como você avalia a qualidade geral do seu sono durante o último mês?`)
table(data_j$`Você tem dificuldade em permanecer acordado enquanto está trabalhando, fazendo refeições, ou envolvido em atividades sociais?`)
table(data_j$`Se sim, com qual frequência? (insira o número de vezes por dia)`)
table(data_j$`Se sim, qual(is) modalidade(s) e qual a frequência semanal?`)#retirar
table(data_j$`ICT- Assinale com um número na escala de 0 a 10, quantos pontos você daria para sua capacidade de trabalho atual (no momento presente):`)
table(data_j$`EUROQOL (Utilidade)`)
table(data_j$`JSS- ESCORE ''Demanda psicológica'':`)
table(data_j$`JSS- ESCORE ''Controle no processo de trabalho'':`)
table(data_j$`JSS- ESCORE ''Apoio Social'':`)
table(data_j$`JSS- ESCORE FINAL`)

# Verificando normalidade das numéricas
shapiro.test(data_j$`Como você descreveria o grau de calor e suor influenciado pela máscara, no seu rosto, durante o uso? (A1)`)
shapiro.test(data_j$`Como você descreveria a facilidade para respirar, durante o uso da máscara? (A1)`)
shapiro.test(data_j$`Como você descreveria a facilidade para falar e se comunicar (inteligibilidade da fala), durante o uso da máscara? (A1)`)
shapiro.test(data_j$`Como você descreveria o nível de coceira gerado pelo uso da máscara? (A1)`)
shapiro.test(data_j$`Qual foi o percentual de uso da máscara durante os plantões?  (Inserir número de 0 a 100)- (A1)`)
shapiro.test(data_j$`Durante o último mês, quantas horas de sono você teve por noite? (esta pode ser diferente do número de horas que você ficou na cama)`)
shapiro.test(data_j$`Se sim, com qual frequência? (insira o número de vezes por dia)`)
shapiro.test(data_j$`ICT- Assinale com um número na escala de 0 a 10, quantos pontos você daria para sua capacidade de trabalho atual (no momento presente):`)
shapiro.test(data_j$`JSS- ESCORE ''Demanda psicológica'':`)

             

# Lista de todas as variáveis exceto 'GRUPO MÁSCARA'
other_variables <- setdiff(names(data_j), "GRUPO MÁSCARA")

#result <- list()
# Loop para realizar os testes estatísticos
for (variable_name in other_variables) {
  variable <- data_j[[variable_name]]
  
  if (is.numeric(variable)) {
    # Teste de Kruskal-Wallis para variáveis numéricas
    group_1 <- variable[data_j$`GRUPO MÁSCARA` == 1]
    group_2 <- variable[data_j$`GRUPO MÁSCARA` == 2]
    result <- kruskal.test(list(group_1, group_2))
    test_type <- "Kruskal-Wallis"
  } else {
    # Teste exato de Fisher ou Qui-Quadrado para variáveis categóricas
    contingency_table <- table(variable, data_j$`GRUPO MÁSCARA`)
    if (nrow(contingency_table) == 2 && ncol(contingency_table) == 2) {
      result <- fisher.test(contingency_table)
      test_type <- "Fisher's Exact"
    } else {
      result <- chisq.test(contingency_table)
      test_type <- "Chi-squared"
    }
  }
  
  cat(paste("Variável:", variable_name, "\n"))
  cat(paste("Tipo de teste:", test_type, "\n"))
  cat(paste("P-valor:", result$p.value, "\n"))
  cat("----------------------------\n")
}

table(data_j$`GRUPO MÁSCARA` ,data_j$`Tomou a vacina contra a gripe (este ano)? (A2)`)

data_j$`GRUPO MÁSCARA`

# Removendo colunas inúteis
data_j[,4] <- NULL
data_j[,14] <- NULL

# Transformando em numérico o que não está, mas deveria
data_j[,13]
data_j$`Qual foi o percentual de uso da máscara durante os plantões?  (Inserir número de 0 a 100)- (A1)` <- as.numeric(data_j$`Qual foi o percentual de uso da máscara durante os plantões?  (Inserir número de 0 a 100)- (A1)`)

data_j[,24]
data_j$`Qual foi o percentual de uso da máscara durante os plantões?  (Inserir número de 0 a 100) - (A2)` <- as.numeric(data_j$`Qual foi o percentual de uso da máscara durante os plantões?  (Inserir número de 0 a 100) - (A2)`)

data_j[,36]
data_j$`EUROQOL (Utilidade)` <- as.numeric(data_j$`EUROQOL (Utilidade)`)

numericas <- data_j %>%
  select_if(is.numeric)
numericas$`GRUPO MÁSCARA` <- data_j$`GRUPO MÁSCARA`

categoricas <- data_j %>%
  select_if(~ !is.numeric(.))

# Criando as tabelas de contingência para as variáveis categóricas ----

# Criar uma lista para armazenar as tabelas de contingência
contingency_tables <- list()

# Colunas a serem analisadas (exceto 'GRUPO MÁSCARA')
columns_to_analyze <- colnames(categoricas)[-1]

# Loop pelas colunas e criar tabelas de contingência
for (col in columns_to_analyze) {
  contingency_table <- categoricas %>%
    count(`GRUPO MÁSCARA`, {{col}}) %>%
    spread(`GRUPO MÁSCARA`, n, fill = 0) %>%
    mutate(Total = `1` + `2`)
  
  contingency_tables[[col]] <- contingency_table
}

contingency_tables

# Criando a tabela de medianas para as variáveis numéricas ----

colunas_numericas <- numericas %>%
  select(-`GRUPO MÁSCARA`) %>%
  select(where(is.numeric))

medianas_por_grupo <- numericas %>%
  select(`GRUPO MÁSCARA`, everything()) %>%
  group_by(`GRUPO MÁSCARA`) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))

medianas_por_grupo
