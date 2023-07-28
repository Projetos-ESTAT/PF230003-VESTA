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
#calculando tamanho amostral
prop1 <- 2/32
prop2 <- 4/31
LSD = abs(prop1 - prop2)
TaxaResposta = c(0.8)
S2 <- prop1*(1-prop1) + prop2*(1-prop2) 
#tamanho amostral
nlinha <- (qnorm(1-0.025)+qnorm(1-0.20))^2*S2/LSD^2

#sem ajuste população finita
#com taxa não resposta
n <- nlinha/TaxaResposta


#com ajuste população funcionários da saúde pública no DF no contato direto com o paciente
N <- 27347
nlinha2 <- N*nlinha/(N+nlinha)
#com taxa não resposta
n <- nlinha2/TaxaResposta


#com ajuste população profissionais da saúde do HRAN
N <- 1968
nlinha2 <- N*nlinha/(N+nlinha)
#com taxa não resposta
n <- nlinha2/TaxaResposta
n


#Confirmação do tamanho amostral
alpha<-0.05               #definir alfa

#bilateral

zcrit1<- -qnorm(alpha/2)  #definir o zcrítico (atenção no sinal) 
beta1 <- pnorm(zcrit1,mean=(abs(prop1-prop2))/sqrt(S2/nlinha)) #calculo do beta
beta1                     #definido como 0.2 pelo pesquisador




#amostra

#usando o teste A1
dados <- read_xlsx("dados/vesta.xlsx")
unique(dados$`GRUPO MÁSCARA`)
dados1 <- filter(dados, `GRUPO MÁSCARA` %in% c("2.0","1.0"))
unique(dados1$`Resultado Exame RT-PCR (A1)`)
dados2 <- filter(dados1, `Resultado Exame RT-PCR (A1)` %in% c("Não detectado","Detectado"))
mean(dados2$`Resultado Exame RT-PCR (A1)` == "Detectado")

#para cada grupo
grupo1 <- filter(dados2, `GRUPO MÁSCARA` == "1.0")
prop1 <- mean(grupo1$`Resultado Exame RT-PCR (A1)` == "Detectado")

grupo2 <- filter(dados2, `GRUPO MÁSCARA` == "2.0")
prop2 <- mean(grupo2$`Resultado Exame RT-PCR (A1)` == "Detectado")

#estatísticas
#LSD = abs(prop1 - prop2)
#S2 <- prop1*(1-prop1) + prop2*(1-prop2)
N <- 1968                      #população
n<-as.numeric(count(dados2)/2) #numero de pessoas participantes para cada grupo
n1 <- N*n/(N-n)                #numero para calculo do poder

#poder do teste
alpha<-0.05               #definir alfa

#bilateral

zcrit1<- -qnorm(alpha/2)  #definir o zcrítico (atenção no sinal) 
beta1 <- pnorm(zcrit1,mean=(LSD)/sqrt(S2/n1)) #calculo do beta
1- beta1                     #poder é 1- beta






# #usando o teste A2
# unique(dados1$`Resultado Exame RT-PCR (A2)`)
# dados2 <- filter(dados1, `Resultado Exame RT-PCR (A2)` %in% c("Não detectado","Detectado"))
# mean(dados2$`Resultado Exame RT-PCR (A2)` == "Detectado")
# 
# #para cada grupo
# grupo1 <- filter(dados2, `GRUPO MÁSCARA` == "1.0")
# prop1 <- mean(grupo1$`Resultado Exame RT-PCR (A2)` == "Detectado")
# 
# grupo2 <- filter(dados2, `GRUPO MÁSCARA` == "2.0")
# prop2 <- mean(grupo2$`Resultado Exame RT-PCR (A2)` == "Detectado")
# 
# #estatísticas
# LSD = abs(prop1 - prop2)
# TaxaResposta = as.numeric(count(dados2)/count(dados1))
# S2 <- prop1*(1-prop1) + prop2*(1-prop2)
# 
# #tamanho amostral
# nlinha <- (qnorm(1-0.025)+qnorm(1-0.20))^2*S2/LSD^2
# #com taxa não resposta
# n <- nlinha/TaxaResposta
# 
# 
# 
# 
# 
# #usando presença em qualquer teste
# dados <- read_xlsx("dados/vesta.xlsx")
# unique(dados$`GRUPO MÁSCARA`)
# dados1 <- filter(dados, `GRUPO MÁSCARA` %in% c("2.0","1.0"))
# unique(dados1$`GRUPO MÁSCARA`)
# 
# unique(dados1$`Desfecho positivo durante o acompanhamento?`)
# dados2 <- filter(dados1, `Desfecho positivo durante o acompanhamento?` %in% c("Não","SIM"))
# mean(dados2$`Desfecho positivo durante o acompanhamento?` == "SIM")
# 
# #para cada grupo
# grupo1 <- filter(dados2, `GRUPO MÁSCARA` == "1.0")
# prop1 <- mean(grupo1$`Desfecho positivo durante o acompanhamento?` == "SIM")
# 
# grupo2 <- filter(dados2, `GRUPO MÁSCARA` == "2.0")
# prop2 <- mean(grupo2$`Desfecho positivo durante o acompanhamento?` == "SIM")
# 
# #estatísticas
# LSD = abs(prop1 - prop2)
# TaxaResposta = as.numeric(count(dados2)/count(dados1))
# S2 <- prop1*(1-prop1) + prop2*(1-prop2)
# 
# #tamanho amostral
# nlinha <- (qnorm(1-0.025)+qnorm(1-0.20))^2*S2/LSD^2
# #com taxa não resposta
# n <- nlinha/TaxaResposta