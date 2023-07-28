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

df <- data.frame(read_excel("banco/df.xlsx")) # leitura dos dados ----

#df$GRUPO.MÁSCARA

df <- df |>
  filter(GRUPO.MÁSCARA == 1 | GRUPO.MÁSCARA == 2) # eliminando os dropouts do estudo

df$Desfecho.positivo..Influenza..durante.o.acompanhamento. <- ifelse(df$Desfecho.positivo..Influenza..durante.o.acompanhamento.
 == "SIM","Sim",df$Desfecho.positivo..Influenza..durante.o.acompanhamento.) # ajustando as respostas

table(factor(df[,30])) # Teste positivo para influenza
table(factor(df[,31])) # Teste positivo para covid

df1 <- df |>
  filter(GRUPO.MÁSCARA == 1)

table(factor(df1[,30])) # Teste positivo para influenza
table(factor(df1[,31])) # Teste positivo para covid

df2 <- df |>
  filter(GRUPO.MÁSCARA == 2)

table(factor(df2[,30])) # Teste positivo para influenza
table(factor(df2[,31])) # Teste positivo para covid

# ---------------------------------------------------------------------------- #