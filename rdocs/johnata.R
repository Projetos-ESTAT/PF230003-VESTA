library(rpart)
library(randomForest)
library(boot)
banco <- read_xlsx("banco/df.xlsx",sheet = 1)
banco2 <- banco %>% filter(`GRUPO MÁSCARA` %in% c(1,2))
banco2$Positivo <- ifelse(banco2$`Desfecho positivo (Influenza) durante o acompanhamento?` == "Não" & banco2$`Desfecho positivo (Influenza) durante o acompanhamento?` == "Não", 0,1)

colSums(is.na(bancomod2))

banco3 <- banco2 %>% filter(!is.na(banco2$`Desfecho positivo (COVID-19) durante o acompanhamento?`))

bancoteste <- banco2


bancomod2 <- banco2 %>% select(Positivo,2,32,34,36,Sexo)
bancomod2[] <- lapply(bancomod2, as.factor)

str(bancomod2)
teste2 <- rfImpute(bancomod2, Positivo ~ .)

table(bancomod2$Positivo)
table(teste$Positivo)
