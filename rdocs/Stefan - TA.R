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
banco <- read_xlsx("banco/df.xlsx",sheet = 1) %>%
  mutate_all(tolower)

#Quem n participou
banco2 <- banco %>% filter(!`GRUPO MÁSCARA` %in% c(1,2)) 
table(banco2$Situação)

#Quem participou
banco2 <- banco %>% filter(`GRUPO MÁSCARA` %in% c(1,2))
unique(banco2$`Desfecho positivo (COVID-19) durante o acompanhamento?`)
unique(banco2$`Desfecho positivo (Influenza) durante o acompanhamento?`)

sum(is.na(banco2$`Desfecho positivo (COVID-19) durante o acompanhamento?`))
sum(is.na(banco2$`Desfecho positivo (Influenza) durante o acompanhamento?`))
table(banco2$`Desfecho positivo (COVID-19) durante o acompanhamento?`)
table(banco2$`Desfecho positivo (Influenza) durante o acompanhamento?`)

#Filtrando 
banco3 <- banco2 %>% filter(!is.na(banco2$`Desfecho positivo (COVID-19) durante o acompanhamento?`))
sum(is.na(banco3$`Desfecho positivo (COVID-19) durante o acompanhamento?`)) #nenhum NA
sum(is.na(banco3$`Desfecho positivo (Influenza) durante o acompanhamento?`)) #nenhum NA

banco3$Positivo <- ifelse(banco3$`Desfecho positivo (Influenza) durante o acompanhamento?` == "não" & banco3$`Desfecho positivo (COVID-19) durante o acompanhamento?` == "não", 0,1)
table(banco3$Positivo)

#Modelo original

#Banco dos positivos
bancoPos <- banco3 %>% filter(Positivo==1)

modelo1 <- glm(Positivo ~ `GRUPO MÁSCARA`,family = "binomial", data=banco3)
summary(modelo1)

#modelo 2
bancomod2 <- banco3 %>% select(Positivo,2,32,34,36,Sexo,Idade)
for (i in 1:6){
  print(table(bancomod2[i+1]))
}
modelo2 <- glm(Positivo ~. ,family = "binomial", data=bancomod2)
summary(modelo2)
cv.glm(na.omit(bancomod2),modelo2)


#modelo 3
bancomod3 <- banco3 %>% dplyr::select(Positivo,2,11,32,36,Sexo,Idade) %>% na.omit()
banco3$`Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?` <- as.numeric(ifelse(banco3$`Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?`=="40h",40,
                                                                                           banco3$`Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?`))
for (i in 1:5){
  print(table(bancomod3[i+1]))
}
modelo3 <- glm(Positivo ~. ,family = "binomial", data=bancomod3)
summary(modelo3)
modeloStep <- step(modelo3)
summary(modeloStep)
#AIC
bancoAIC <- banco3 %>% dplyr::select(Positivo,32) %>% na.omit()
modeloStep <- glm(Positivo ~. ,family = "binomial", data=bancoAIC)
summary(modeloStep)

#modelo 4
bancomod4 <- banco3 %>% select(Positivo,2,11,32) %>% na.omit()
modelo4 <- glm(Positivo ~. ,family = "binomial", data=bancomod4)
summary(modelo4)




pacman::p_load("readxl", "dplyr", "purrr",
               "glue", "ggplot2", "ggpubr",
               "kableExtra", "caret", "pROC",
               "plotROC", "tibble", "reticulate", "glmtoolbox",
               "survey", "MASS", "caret")

#modelo 3
roc(bancomod3$Positivo, predict(modelo3, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

conf_mat <- confusionMatrix(factor(as.numeric(predict(modelo3, type = "response")>=0.1)),
                            factor(bancomod3$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]


conf_mat <- confusionMatrix(factor(as.numeric(predict(modelo3, type = "response")>=0.5)),
                            factor(bancomod3$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]


#modelo 4
roc(bancomod4$Positivo, predict(modelo4, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

conf_mat <- confusionMatrix(factor(as.numeric(predict(modelo4, type = "response")>=0.1)),
                            factor(bancomod4$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]


conf_mat <- confusionMatrix(factor(as.numeric(predict(modelo4, type = "response")>=0.5)),
                            factor(bancomod4$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]



#modelo AIC
roc(bancoAIC$Positivo, predict(modeloStep, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

conf_mat <- confusionMatrix(factor(as.numeric(predict(modeloStep, type = "response")>=0.1)),
                            factor(bancoAIC$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]


conf_mat <- confusionMatrix(factor(as.numeric(predict(modelo4, type = "response")>=0.5)),
                            factor(bancomod4$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]
