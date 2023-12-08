source("rdocs/source/packages.R")

pacman::p_load("readxl", "dplyr", "purrr",
               "glue", "ggplot2", "ggpubr",
               "kableExtra", "caret", "pROC",
               "plotROC", "tibble", "reticulate", "glmtoolbox",
               "survey", "MASS", "caret")


banco <- read_xlsx("banco/df2.xlsx",sheet = 1) %>%
  mutate_all(tolower) %>% 
  filter(`GRUPO MÁSCARA` %in% c(1,2)) %>% 
  filter(!is.na(`Desfecho positivo (COVID-19) durante o acompanhamento?`))

banco$Positivo <- ifelse(banco$`Desfecho positivo (Influenza) durante o acompanhamento?` == "não" &
                         banco$`Desfecho positivo (COVID-19) durante o acompanhamento?` == "não", 0,1)
table(banco$Positivo)

modelo <- glm(Positivo ~ `GRUPO MÁSCARA`,family = "binomial", 
               data=banco)
summary(modelo)

fisher.test(banco$Positivo,banco$`GRUPO MÁSCARA`) #### OR = 0.9898436 ####
fisher.test(banco$Positivo,banco$`Setor onde trabalha no 'Local de trabalho 1'`)
fisher.test(banco$Positivo,banco$`Trabalha em mais de um local?`)
fisher.test(banco$Positivo,banco$Sexo)
fisher.test(banco$Positivo,banco$`Tem histórico da COVID-19 ou TR (IgG+) positivos?`)

#### Modelo completo ####
banco2 <- banco %>% 
  dplyr::select(Positivo,2,4,13,14,32) %>% 
  na.omit()

modelo2 <- glm(Positivo ~ .,family = "binomial", 
               data=banco2) 
summary(modelo2)
exp(modelo2$coefficients[2]) ##### OR = 0.8615026  ####

roc(banco2$Positivo, predict(modelo2, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

conf_mat <- confusionMatrix(factor(as.numeric(predict(modelo2, type = "response")>=0.1)),
                            factor(banco2$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]  ##### 0.3571429 ; 0.8402062####

#### Modelo step ####
banco3 <- banco %>% 
  dplyr::select(Positivo,`GRUPO MÁSCARA`,
  `Tem histórico da COVID-19 ou TR (IgG+) positivos?`) %>% 
  na.omit()
  
modeloStep <- glm(Positivo ~ `GRUPO MÁSCARA` + 
                    `Tem histórico da COVID-19 ou TR (IgG+) positivos?`,
                  family = "binomial", 
                  data=banco3)  
summary(modeloStep)
exp(modeloStep$coefficients[2])##### OR = 1.044174 ####

roc(banco3$Positivo, predict(modeloStep, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

conf_mat <- confusionMatrix(factor(as.numeric(predict(modeloStep, type = "response")>=0.1)),
                            factor(banco3$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")] #####  0.4285714 ; 0.7731959 #####

##### Tentando prever as observações faltantes #####
 
banco_na <- read_xlsx("banco/df2.xlsx",sheet = 1) %>%
  mutate_all(tolower) %>% 
  filter(`GRUPO MÁSCARA` %in% c(1,2)) %>% 
  filter(is.na(`Desfecho positivo (COVID-19) durante o acompanhamento?`))

predict(modeloStep,banco_na, type = "response")


#### Ajuste com os NA's negativadoos ####

banco_neg <- read_xlsx("banco/df2.xlsx",sheet = 1) %>%
  mutate_all(tolower) %>% 
  filter(`GRUPO MÁSCARA` %in% c(1,2)) 

banco_neg <- banco_neg %>% 
  mutate(Positivo=case_when(
    `Desfecho positivo (Influenza) durante o acompanhamento?` == "sim" ~ 1, 
      `Desfecho positivo (COVID-19) durante o acompanhamento?` == "sim" ~ 1,
    is.na(`Desfecho positivo (COVID-19) durante o acompanhamento?`) ~ 0,
    `Desfecho positivo (COVID-19) durante o acompanhamento?` == 'não' ~ 0,
    `Desfecho positivo (Influenza) durante o acompanhamento?` == 'não' ~ 0
  ))

fisher.test(banco_neg$Positivo,banco_neg$`GRUPO MÁSCARA`) #### OR =  1.056915 ####

#### Modelo completo negativos ####
banco_neg2 <- banco_neg %>% 
  dplyr::select(Positivo,2,4,13,14,32) %>% 
  na.omit()

modelo_neg <- glm(Positivo ~ .,family = "binomial", 
               data=banco_neg2) 
summary(modelo_neg)
exp(modelo_neg$coefficients[2]) ##### OR = 0.8919866  ####

roc(banco_neg2$Positivo, predict(modelo_neg, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

conf_mat <- confusionMatrix(factor(as.numeric(predict(modelo_neg, type = "response")>=0.1)),
                            factor(banco_neg2$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]  ##### Sens 0.2857143 ; Sepec 0.8558140####


#### Modelo step ####
banco_neg3 <- banco_neg %>% 
  dplyr::select(Positivo,`GRUPO MÁSCARA`,
                `Tem histórico da COVID-19 ou TR (IgG+) positivos?`) %>% 
  na.omit()

modeloStep_neg <- glm(Positivo ~ `GRUPO MÁSCARA` + 
                    `Tem histórico da COVID-19 ou TR (IgG+) positivos?`,
                  family = "binomial", 
                  data=banco_neg3)  
summary(modeloStep_neg)
exp(modeloStep_neg$coefficients[2])##### OR = 1.103479 ####

roc(banco_neg3$Positivo, predict(modeloStep_neg, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

conf_mat <- confusionMatrix(factor(as.numeric(predict(modeloStep_neg, type = "response")>=0.1)),
                            factor(banco_neg3$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")] ##### Sens 0.4285714 ; Sepc 0.7720930 #####



#### Ajuste com os NA's positivados ####

banco_pos <- read_xlsx("banco/df2.xlsx",sheet = 1) %>%
  mutate_all(tolower) %>% 
  filter(`GRUPO MÁSCARA` %in% c(1,2)) 

banco_pos <- banco_pos %>% 
  mutate(Positivo=case_when(
    `Desfecho positivo (Influenza) durante o acompanhamento?` == "sim" ~ 1, 
    `Desfecho positivo (COVID-19) durante o acompanhamento?` == "sim" ~ 1,
    is.na(`Desfecho positivo (COVID-19) durante o acompanhamento?`) ~ 1,
    `Desfecho positivo (COVID-19) durante o acompanhamento?` == 'não' ~ 0,
    `Desfecho positivo (Influenza) durante o acompanhamento?` == 'não' ~ 0
  ))

table(banco_pos$Positivo) 
fisher.test(banco_pos$Positivo,banco_pos$`GRUPO MÁSCARA`) #### OR =  0.6610555 ####

#### Modelo completo positivos ####
banco_pos2 <- banco_pos %>% 
  dplyr::select(Positivo,2,4,13,14,32) %>% 
  na.omit()

modelo_pos <- glm(Positivo ~ .,family = "binomial", 
                  data=banco_pos2) 
summary(modelo_pos)
exp(modelo_pos$coefficients[2]) ##### OR = 0.6656736  ####

roc(banco_pos2$Positivo, predict(modelo_pos, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

conf_mat <- confusionMatrix(factor(as.numeric(predict(modelo_pos, type = "response")>=0.2)),
                            factor(banco_pos2$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]  ##### Sens 0.3714286 ; Sepec 0.7938144####


#### Modelo step positivos ####
banco_pos3 <- banco_pos %>% 
  dplyr::select(Positivo,`GRUPO MÁSCARA`,
                `Tem histórico da COVID-19 ou TR (IgG+) positivos?`) %>% 
  na.omit()

modeloStep_pos <- glm(Positivo ~ `GRUPO MÁSCARA` + 
                        `Tem histórico da COVID-19 ou TR (IgG+) positivos?`,
                      family = "binomial", 
                      data=banco_pos3)  
summary(modeloStep_pos)
exp(modeloStep_pos$coefficients[2])##### OR = 0.6743768 ####

roc(banco_pos3$Positivo, predict(modeloStep_pos, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

conf_mat <- confusionMatrix(factor(as.numeric(predict(modeloStep_pos, type = "response")>=0.1)),
                            factor(banco_pos3$Positivo), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")] ##### Sens  ; Sepc  #####




