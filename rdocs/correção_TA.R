source("rdocs/source/packages.R")

pacman::p_load("readxl", "dplyr", "purrr",
               "glue", "ggplot2", "ggpubr",
               "kableExtra", "caret", "pROC",
               "plotROC", "tibble", "reticulate", "glmtoolbox",
               "survey", "MASS", "caret", "xlsx")

bancoOriginal <- read_xlsx("banco/df2.xlsx",sheet = 1)%>%
  mutate_all(tolower) %>% 
  filter(`GRUPO MÁSCARA` %in% c(1,2))
bancoOriginal$`Já foi diagnosticado com COVID-19 mais de uma vez?` <-ifelse(bancoOriginal$`Tem histórico da COVID-19 ou TR (IgG+) positivos?`=="não",
                                                                            "não",bancoOriginal$`Já foi diagnosticado com COVID-19 mais de uma vez?`)
bancoSalvar <- bancoOriginal
bancoSalvar$Positivo <- ifelse(bancoSalvar$`Desfecho positivo (Influenza) durante o acompanhamento?` == "não" &
                                 bancoSalvar$`Desfecho positivo (COVID-19) durante o acompanhamento?` == "não", 0,1)
write.xlsx(bancoSalvar, "resultados/Imputação.xlsx")


banco <- bancoOriginal %>% 
  filter(!is.na(`Desfecho positivo (COVID-19) durante o acompanhamento?`))

banco$Positivo <- ifelse(banco$`Desfecho positivo (Influenza) durante o acompanhamento?` == "não" &
                         banco$`Desfecho positivo (COVID-19) durante o acompanhamento?` == "não", 0,1)
table(banco$Positivo)
table(banco%>% dplyr::select(Positivo,`GRUPO MÁSCARA`))

modelo <- glm(Positivo ~ `GRUPO MÁSCARA`,family = "binomial", 
               data=banco)
summary(modelo)
(OR <-exp(modelo$coefficients[2]))
exp(log(OR)-qnorm(0.975)*sqrt(1/97+1/7+1/98+1/7))
exp(log(OR)+qnorm(0.975)*sqrt(1/97+1/7+1/98+1/7))
#OU
exp(log(OR)-qnorm(0.975)*0.55338)
exp(log(OR)+qnorm(0.975)*0.55338)

#### Modelo completo ####
banco2 <- banco %>% 
  dplyr::select(Positivo,2,4,13,14,33) %>% 
  na.omit()

modelo2 <- glm(Positivo ~ .,family = "binomial", 
               data=banco2) 
summary(modelo2)
(OR2 <- exp(modelo2$coefficients[2]))
exp(log(OR2)-qnorm(0.975)*0.59902)
exp(log(OR2)+qnorm(0.975)*0.59902)


roc(banco2$Positivo, predict(modelo2, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

roc(banco2$Positivo, predict(modelo2, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)$thresholds

conf_mat <- confusionMatrix(factor(as.numeric(predict(modelo2, type = "response")>=0.032595574)),
                            factor(banco2$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")] 

#### Modelo step ####
modeloStep <- step(modelo2, scope=list(lower=Positivo ~ `GRUPO MÁSCARA`, upper=modelo2))
summary(modeloStep)

banco3 <- banco %>% 
  dplyr::select(Positivo,`GRUPO MÁSCARA`,
  `Já foi diagnosticado com COVID-19 mais de uma vez?`) %>% 
  na.omit()
  
modeloStep <- glm(Positivo ~ `GRUPO MÁSCARA` + 
                    `Já foi diagnosticado com COVID-19 mais de uma vez?`,
                  family = "binomial", 
                  data=banco3)  
summary(modeloStep)
(OR3 <- exp(modeloStep$coefficients[2]))
exp(log(OR2)-qnorm(0.975)*0.5642)
exp(log(OR2)+qnorm(0.975)*0.5642)

roc(banco3$Positivo, predict(modeloStep, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)


roc(banco3$Positivo, predict(modeloStep, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)$thresholds

conf_mat <- confusionMatrix(factor(as.numeric(predict(modeloStep, type = "response")>=0.05065026)),
                            factor(banco3$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")] 

##### Tentando prever as observações faltantes #####
 
banco_na <- bancoOriginal %>% 
  filter(is.na(`Desfecho positivo (COVID-19) durante o acompanhamento?`))


predict(modeloStep,banco_na, type = "response")
as.numeric(predict(modeloStep,banco_na, type = "response")>=0.05065026)
as.numeric(predict(modelo,banco_na, type = "response")>=0.0666667)


#### Ajuste com os NA's negativados ####
banco_neg <- bancoOriginal %>% 
  mutate(Positivo=case_when(
    `Desfecho positivo (Influenza) durante o acompanhamento?` == "sim" ~ 1, 
      `Desfecho positivo (COVID-19) durante o acompanhamento?` == "sim" ~ 1,
    is.na(`Desfecho positivo (COVID-19) durante o acompanhamento?`) ~ 0,
    `Desfecho positivo (COVID-19) durante o acompanhamento?` == 'não' ~ 0,
    `Desfecho positivo (Influenza) durante o acompanhamento?` == 'não' ~ 0
  ))

table(banco_neg$Positivo)
table(banco_neg%>% dplyr::select(Positivo,`GRUPO MÁSCARA`))

modelo <- glm(Positivo ~ `GRUPO MÁSCARA`,family = "binomial", 
              data=banco_neg)
summary(modelo)
(OR <-exp(modelo$coefficients[2]))
exp(log(OR)-qnorm(0.975)*sqrt(1/111+1/7+1/105+1/7))
exp(log(OR)+qnorm(0.975)*sqrt(1/111+1/7+1/105+1/7))
#OU
exp(log(OR)-qnorm(0.975)*0.55158)
exp(log(OR)+qnorm(0.975)*0.55158)


#### Modelo completo negativos ####
banco_neg2 <- banco_neg %>% 
  dplyr::select(Positivo,2,4,13,14,33) %>% 
  na.omit()

modelo_neg <- glm(Positivo ~ .,family = "binomial", 
               data=banco_neg2) 
summary(modelo_neg)
(OR2 <- exp(modelo_neg$coefficients[2]))
exp(log(OR2)-qnorm(0.975)*0.60036)
exp(log(OR2)+qnorm(0.975)*0.60036)

roc(banco_neg2$Positivo, predict(modelo_neg, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

roc(banco_neg2$Positivo, predict(modelo_neg, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)$thresholds

conf_mat <- confusionMatrix(factor(as.numeric(predict(modelo_neg, type = "response")>=0.030342316)),
                            factor(banco_neg2$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]  


#### Modelo step ####
modeloStep <- step(modelo_neg, scope=list(lower=Positivo ~ `GRUPO MÁSCARA`, upper=modelo_neg))
summary(modeloStep)

banco3 <- banco_neg %>% 
  dplyr::select(Positivo,`GRUPO MÁSCARA`,
                `Já foi diagnosticado com COVID-19 mais de uma vez?`) %>% 
  na.omit()

modeloStep <- glm(Positivo ~ `GRUPO MÁSCARA` + 
                    `Já foi diagnosticado com COVID-19 mais de uma vez?`,
                  family = "binomial", 
                  data=banco3)  
summary(modeloStep)
(OR3 <- exp(modeloStep$coefficients[2]))
exp(log(OR3)-qnorm(0.975)*0.5609)
exp(log(OR3)+qnorm(0.975)*0.5609)

roc(banco3$Positivo, predict(modeloStep, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

roc(banco3$Positivo, predict(modeloStep, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)$thresholds

conf_mat <- confusionMatrix(factor(as.numeric(predict(modeloStep, type = "response")>=0.04)),
                            factor(banco3$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")] 



#### Ajuste com os NA's positivados ####

banco_pos <- bancoOriginal %>% 
  mutate(Positivo=case_when(
    `Desfecho positivo (Influenza) durante o acompanhamento?` == "sim" ~ 1, 
    `Desfecho positivo (COVID-19) durante o acompanhamento?` == "sim" ~ 1,
    is.na(`Desfecho positivo (COVID-19) durante o acompanhamento?`) ~ 1,
    `Desfecho positivo (COVID-19) durante o acompanhamento?` == 'não' ~ 0,
    `Desfecho positivo (Influenza) durante o acompanhamento?` == 'não' ~ 0
  ))

table(banco_pos$Positivo)
table(banco_pos%>% dplyr::select(Positivo,`GRUPO MÁSCARA`))

modelo <- glm(Positivo ~ `GRUPO MÁSCARA`,family = "binomial", 
              data=banco_pos)
summary(modelo)
(OR <-exp(modelo$coefficients[2]))
exp(log(OR)-qnorm(0.975)*sqrt(1/97+1/21+1/98+1/14))
exp(log(OR)+qnorm(0.975)*sqrt(1/97+1/21+1/98+1/14))
#OU
exp(log(OR)-qnorm(0.975)*0.3736)
exp(log(OR)+qnorm(0.975)*0.3736)


#### Modelo completo positivos ####
banco_pos2 <- banco_pos %>% 
  dplyr::select(Positivo,2,4,13,14,33) %>% 
  na.omit()

modelo_pos <- glm(Positivo ~ .,family = "binomial", 
                  data=banco_pos2) 
summary(modelo_pos)
(OR2 <- exp(modelo_pos$coefficients[2]))
exp(log(OR2)-qnorm(0.975)*0.40787)
exp(log(OR2)+qnorm(0.975)*0.40787)

roc(banco_pos2$Positivo, predict(modelo_pos, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

roc(banco_pos2$Positivo, predict(modelo_pos, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)$thresholds

conf_mat <- confusionMatrix(factor(as.numeric(predict(modelo_pos, type = "response")>=0.09505898)),
                            factor(banco_pos2$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")] 


#### Modelo step ####
modeloStep <- step(modelo_pos, scope=list(lower=Positivo ~ `GRUPO MÁSCARA`, upper=modelo_pos))
summary(modeloStep)

banco3 <- banco_pos %>% 
  dplyr::select(Positivo,`GRUPO MÁSCARA`) %>% 
  na.omit()

modeloStep <- glm(Positivo ~ `GRUPO MÁSCARA`,
                  family = "binomial", 
                  data=banco3)  
summary(modeloStep)
(OR3 <- exp(modeloStep$coefficients[2]))
exp(log(OR3)-qnorm(0.975)*0.3736)
exp(log(OR3)+qnorm(0.975)*0.3736)

roc(banco3$Positivo, predict(modeloStep, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

roc(banco3$Positivo, predict(modeloStep, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)$thresholds

conf_mat <- confusionMatrix(factor(as.numeric(predict(modeloStep, type = "response")>=0.1514831)),
                            factor(banco3$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")] 
