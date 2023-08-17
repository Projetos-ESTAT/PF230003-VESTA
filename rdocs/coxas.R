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



df <- read_excel("banco/df.xlsx")

#Limpando o banco de dados

#df$GRUPO.MÁSCARA

df <- df %>%
  filter(`GRUPO MÁSCARA` == 1 | `GRUPO MÁSCARA` == 2) # eliminando os dropouts do estudo

df<- df[, 2:41]

df$`Desfecho positivo (Influenza) durante o acompanhamento?` <- ifelse(df$`Desfecho positivo (Influenza) durante o acompanhamento?`
                                                                     == "SIM","Sim",df$`Desfecho positivo (Influenza) durante o acompanhamento?`) # ajustando as respostas

df$`Estado em que nasceu` <- ifelse(grepl("Distrito Federal", df$`Estado em que nasceu`), "Brasília", "Outros")

df$`Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?` <- gsub("[^0-9]", "", df$`Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?`) 

# Converter os valores para numéricos
df$`Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?` <- as.numeric(df$`Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?`)

#Agrupando em 2 categorias
df$`Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?`  <- ifelse(df$`Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?` <= 20, "Até 20h", "Mais de 20h")


# Limpar os valores para garantir que apenas números estão presentes
df$`Há quanto tempo trabalha no 'Local de trabalho 1'?` <- gsub("[^0-9.]", "", df$`Há quanto tempo trabalha no 'Local de trabalho 1'?`)

# Converter os valores para numéricos
df$`Há quanto tempo trabalha no 'Local de trabalho 1'?` <- as.numeric(df$`Há quanto tempo trabalha no 'Local de trabalho 1'?`)

df$`Há quanto tempo trabalha no 'Local de trabalho 1'?`[df$`Há quanto tempo trabalha no 'Local de trabalho 1'?` > 100] <- NA

summary(df$`Há quanto tempo trabalha no 'Local de trabalho 1'?`, na.rm = TRUE)


df$`Há quanto tempo trabalha no 'Local de trabalho 1'?` <- ifelse(is.na(df$`Há quanto tempo trabalha no 'Local de trabalho 1'?`), NA,
       ifelse(df$`Há quanto tempo trabalha no 'Local de trabalho 1'?` <= 4, "Até 4 anos", "Mais de 4 anos"))


df %>% group_by(Escolaridade)%>% tally()

# Criar uma nova coluna chamada "Categoria de Escolaridade"
df$Escolaridade <- ifelse(grepl("fundamental|1 grau", df$Escolaridade, ignore.case = TRUE), "Ensino Fundamental Completo",
                          ifelse(grepl("médio|2 grau|Ensino Superior incompleto|3 grau incompleto|	
                                         Ensino superior incompleto", df$Escolaridade, ignore.case = TRUE), "Ensino Médio Completo",
                                 ifelse(grepl("superior|3 grau|Pós graduação incompleta|Pós Graduação incompleta", df$Escolaridade, ignore.case = TRUE), "Ensino Superior Completo",
                                        ifelse(grepl("pós graduação completa", df$Escolaridade, ignore.case = TRUE), "Pós Graduação Completa", "Outro"))))


# Converter os valores para numéricos

df$`Peso (em Kg)`=as.numeric(df$`Peso (em Kg)`)
df$`Altura (em cm)`=as.numeric(df$`Altura (em cm)`)
df$IMC= as.numeric(df$IMC)

p_load(compareGroups)

# Transformando as variáveis para o formato correto:
df$`GRUPO MÁSCARA` <- factor(df$`GRUPO MÁSCARA`)
df$Sexo <- factor(df$Sexo)
df$`Estado em que nasceu` <- factor(df$`Estado em que nasceu`)
df$`Estado civil` <- factor(df$`Estado civil`)
df$Escolaridade <- factor(df$Escolaridade)
df$`Raça declarada` <- factor(df$`Raça declarada`)
df$`Profissão:` <- factor(df$`Profissão:`)
df$`Local de trabalho 1` <- factor(df$`Local de trabalho 1`)
df$`Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?` <- factor(df$`Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?`)
df$`Há quanto tempo trabalha no 'Local de trabalho 1'?` <- factor(df$`Há quanto tempo trabalha no 'Local de trabalho 1'?`)
df$`Setor onde trabalha no 'Local de trabalho 1'` <- factor(df$`Setor onde trabalha no 'Local de trabalho 1'`)
df$`Trabalha em mais de um local?` <- factor(df$`Trabalha em mais de um local?`)
df$`Local de trabalho 2` <- factor(df$`Local de trabalho 2`)
df$`Setor onde trabalha no Local de trabalho 2:` <- factor(df$`Setor onde trabalha no Local de trabalho 2:`)
df$`Qual sua carga horária de trabalho semanal no local de trabalho 2?` <- as.numeric(df$`Qual sua carga horária de trabalho semanal no local de trabalho 2?`)
df$`Há quanto tempo (em anos) trabalha no Local de trabalho 2?` <- as.numeric(df$`Há quanto tempo (em anos) trabalha no Local de trabalho 2?`)
df$`É fumante?` <- factor(df$`É fumante?`)
df$Situação <- factor(df$Situação)
df$`Resultado RT-PCR COVID (A1)` <- factor(df$`Resultado RT-PCR COVID (A1)`)
df$`Resultado RT-PCR COVID (A2)` <- factor(df$`Resultado RT-PCR COVID (A2)`)
df$`Resultado RT-PCR Influenza A e painel viral (A1)` <- factor(df$`Resultado RT-PCR Influenza A e painel viral (A1)`)
df$`Resultado RT-PCR Influenza A e painel viral (A2)` <- factor(df$`Resultado RT-PCR Influenza A e painel viral (A2)`)
df$`Resultado RT-PCR Influenza B e painel viral (A1)` <- factor(df$`Resultado RT-PCR Influenza B e painel viral (A1)`)
df$`Resultado RT-PCR Influenza B e painel viral (A2)` <- factor(df$`Resultado RT-PCR Influenza B e painel viral (A2)`)
df$`Desfecho positivo (COVID-19) durante o acompanhamento?` <- factor(df$`Desfecho positivo (COVID-19) durante o acompanhamento?`)
df$`Desfecho positivo (Influenza) durante o acompanhamento?` <- factor(df$`Desfecho positivo (Influenza) durante o acompanhamento?`)
df$`Tem histórico da COVID-19 ou TR (IgG+) positivos?` <- factor(df$`Tem histórico da COVID-19 ou TR (IgG+) positivos?`)
df$`Já foi diagnosticado com COVID-19 mais de uma vez?` <- factor(df$`Já foi diagnosticado com COVID-19 mais de uma vez?`)
df$`STATUS VACINAL- A0: Já tomou vacina contra a COVID-19? (A0)` <- factor(df$`STATUS VACINAL- A0: Já tomou vacina contra a COVID-19? (A0)`)
df$`Quantas doses da vacina contra a COVID-19 já tomou?  (A0)` <- as.numeric(df$`Quantas doses da vacina contra a COVID-19 já tomou?  (A0)`)
df$`Tomou a vacina contra a gripe (este ano)? (A0)` <- factor(df$`Tomou a vacina contra a gripe (este ano)? (A0)`)
df$`STATUS VACINAL- A1: Já tomou vacina contra a COVID-19?` <- factor(df$`STATUS VACINAL- A1: Já tomou vacina contra a COVID-19?`)
df$`Quantas doses da vacina contra a COVID-19 já tomou? (A1)` <- as.numeric(df$`Quantas doses da vacina contra a COVID-19 já tomou? (A1)`)
df$`Tomou a vacina contra a gripe (este ano)? (A1)` <- factor(df$`Tomou a vacina contra a gripe (este ano)? (A1)`)
df$`STATUS VACINAL - A2: Já tomou vacina contra a COVID-19?` <- factor(df$`STATUS VACINAL - A2: Já tomou vacina contra a COVID-19?`)
df$`Quantas doses da vacina contra a COVID-19 já tomou? (A2)` <- as.numeric(df$`Quantas doses da vacina contra a COVID-19 já tomou? (A2)`)

#Fazendo os teste de comparação entre os grupos
result=compareGroups(`GRUPO MÁSCARA` ~`Idade` + `Sexo`+
                `Há quanto tempo trabalha no 'Local de trabalho 1'?` +
                `Estado em que nasceu` +`Estado civil`+ `Escolaridade`+
                `Raça declarada` + `Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?`
              +`Trabalha em mais de um local?`+`É fumante?` +`Peso (em Kg)`+ `Altura (em cm)`+
                `IMC`+`Resultado RT-PCR COVID (A1)`+`Resultado RT-PCR COVID (A2)`+
                `Resultado RT-PCR Influenza A e painel viral (A1)`+
                `Resultado RT-PCR Influenza B e painel viral (A1)`+
                `Resultado RT-PCR Influenza A e painel viral (A2)`+
                `Resultado RT-PCR Influenza B e painel viral (A2)`+
                `Desfecho positivo (Influenza) durante o acompanhamento?`+
                `Desfecho positivo (COVID-19) durante o acompanhamento?`+
                `Tem histórico da COVID-19 ou TR (IgG+) positivos?`+
                `Já foi diagnosticado com COVID-19 mais de uma vez?`+
              `STATUS VACINAL- A0: Já tomou vacina contra a COVID-19? (A0)`+
                `Quantas doses da vacina contra a COVID-19 já tomou?  (A0)`+
                `Tomou a vacina contra a gripe (este ano)? (A0)`+
                `STATUS VACINAL - A2: Já tomou vacina contra a COVID-19?`+
                `Quantas doses da vacina contra a COVID-19 já tomou? (A1)`+
                `Tomou a vacina contra a gripe (este ano)? (A1)`+
                `STATUS VACINAL - A2: Já tomou vacina contra a COVID-19?`+
                `Quantas doses da vacina contra a COVID-19 já tomou? (A2)`,
              data=df,
              method = 4)


# Para as variáveis numericas Idade,peso,altura e IMC o pacote utiliza 
#comparação de médias, variância,anova, teste t.
#Para as categoricas é utilizado qui quadrado e teste exato de Fisher

createTable(result)



