source("rdocs/source/packages.R")
p_load(tab)

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
banco <- read_xlsx("banco/df.xlsx",sheet = 1)

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

banco3$Positivo <- ifelse(banco3$`Desfecho positivo (Influenza) durante o acompanhamento?` == "Não" & banco3$`Desfecho positivo (COVID-19) durante o acompanhamento?` == "Não", 0,1)
table(banco3$Positivo)

#Modelo original
banco3$`Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?` <- as.numeric(ifelse(banco3$`Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?`=="40h",40,
                                                                                                   banco3$`Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?`))


#Banco dos positivos
bancoPos <- banco3 %>% filter(Positivo==1)

modelo1 <- glm(Positivo ~ `GRUPO MÁSCARA`,family = "binomial", data=banco3)
summary(modelo1)

#modelo 2
bancomod2 <- banco3 %>% dplyr::select(Positivo,2,11,32,34,36,Sexo,Idade) %>% na.omit()

for (i in 1:6){
  print(table(bancomod2[i+1]))
}
modelo2 <- glm(Positivo ~. ,family = "binomial", data=bancomod2)
summary(modelo2)
exp(modelo2$coefficients[2])
exp(modelo2$coefficients[2]-qnorm(0.975)*5.799e-01)
exp(modelo2$coefficients[2]+qnorm(0.975)*5.799e-01)


#modelo 3
bancomod3 <- banco3 %>% dplyr::select(Positivo,2,11,32,36,Sexo,Idade) %>% na.omit()
banco3$`Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?` <- as.numeric(ifelse(banco3$`Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?`=="40h",40,
                                                                                                   banco3$`Qual sua carga horária de trabalho semanal no 'Local de trabalho 1'?`))
for (i in 1:5){
  print(table(bancomod3[i+1]))
}
modelo3 <- glm(Positivo ~. ,family = "binomial", data=bancomod3)
summary(modelo3)
exp(modelo3$coefficients[2])
exp(modelo3$coefficients[2]-qnorm(0.975)*0.579848)
exp(modelo3$coefficients[2]+qnorm(0.975)*0.579848)

modeloStep <- step(modelo3)
summary(modeloStep)
#AIC
bancoAIC <- banco3 %>% dplyr::select(Positivo,32) %>% na.omit()
modeloStep <- glm(Positivo ~. ,family = "binomial", data=bancoAIC)
summary(modeloStep)

#modelo 4
bancomod4 <- banco3 %>% dplyr::select(Positivo,2,11,32) %>% na.omit()
modelo4 <- glm(Positivo ~. ,family = "binomial", data=bancomod4)
summary(modelo4)
exp(modelo4$coefficients[2])
exp(modelo4$coefficients[2]-qnorm(0.975)*0.56594)
exp(modelo4$coefficients[2]+qnorm(0.975)*0.56594)



pacman::p_load("readxl", "dplyr", "purrr",
               "glue", "ggplot2", "ggpubr",
               "kableExtra", "caret", "pROC",
               "plotROC", "tibble", "reticulate", "glmtoolbox",
               "survey", "MASS", "caret")

#modelo 2

roc(bancomod2$Positivo, predict(modelo2, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

a <- roc(bancomod2$Positivo, predict(modelo2, type = "response"),
         percent=TRUE,
         col = "red",
         print.auc = TRUE,
         plot=T,
         print.thres=T)$threshold

print(a[a>0.08&a<0.1])

conf_mat <- confusionMatrix(factor(as.numeric(predict(modelo2, type = "response")>=0.09863807)),
                            factor(bancomod2$Positivo), positive = "1"); conf_mat$byClass[c("Sensitivity", "Specificity")]

conf_mat$table



#modelo 3
roc(bancomod3$Positivo, predict(modelo3, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

roc(bancomod3$Positivo, predict(modelo3, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)$threshold

conf_mat <- confusionMatrix(factor(as.numeric(predict(modelo3, type = "response")>=0.09654144)),
                            factor(bancomod3$Positivo), positive = "1"); conf_mat$byClass[c("Sensitivity", "Specificity")]

conf_mat$table



#modelo 4
roc(bancomod4$Positivo, predict(modelo4, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

roc(bancomod4$Positivo, predict(modelo4, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)$thresholds

conf_mat <- confusionMatrix(factor(as.numeric(predict(modelo4, type = "response")>=0.09550434)),
                            factor(bancomod4$Positivo), positive = "1"); conf_mat$byClass[c("Sensitivity", "Specificity")]

conf_mat$table


#modelo AIC
roc(bancoAIC$Positivo, predict(modeloStep, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

roc(bancoAIC$Positivo, predict(modeloStep, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)$thresholds


conf_mat <- confusionMatrix(factor(as.numeric(predict(modeloStep, type = "response")>=0.08531646)),
                            factor(bancoAIC$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]
