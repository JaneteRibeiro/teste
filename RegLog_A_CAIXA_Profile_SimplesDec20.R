#cat("\014") # clc
#rm(list = ls()) # clear all
setwd("~/R")
### 0. ImportaÃÂ§ÃÂ£o da base de dados

#save.image("~/R/KTA.RData")
install.packages("tabplot") # instala a biblioteca
install.packages("MASS")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("pbkrtest")
install.packages("caret")
library(tabplot) # carrega a biblioteca
library(MASS)
library(pbkrtest)
library(caret) # biblioteca da validacao cruzada
library(corrplot) # biblioteca para plotar matriz de correlacao
library(ggplot2) # biblioteca para graficos


tabela=read.table("Amostra95.csv",header= T,sep=";",dec=",")




head(tabela) #apresenta as primeiras variaveis
str(tabela) # tipo das variaveis
summary(tabela) # estatisticas descritivas da tabela
write.csv(tabela,file ="tabela.csv")



####### Parte 1

### 1. Execucao do modelo

library(MASS)
tabela$RATING = relevel(tabela$RATING, ref = "A")


### 1.1 Estimativa do modelo de regressao logistica
RLSIMPLES = glm(RATING ~ ESTADO_CIVIL + RENDA_MUTUARIO + SISTEMA_AMORT + SEXO_MUTUARIO +
				IDADE_MUTUARIO + FPOC_VLR_DIVIDA_VENCIDA + VLR_ENC_ATR + REGIAO,
                family="binomial" , data = tabela)

### 1.2 Metricas
predRL = predict(RLSIMPLES, newdata = tabela, type = "response")
show(predRL)
tabela$RATING_RL = "predRL"
write.csv(predRL,file ="predRL_profile.csv")

### 1.3 Matriz de confusÃÂ£o
tabela$RATING_PREDRL = predRL > 0.45 # faz a limiarizaÃÂ§ao 
MC = table(tabela$RATING, tabela$RATING_PREDRL, deparse.level = 2) # montar a matriz de confusao
show(MC)
ACC = sum(diag(MC))/sum(MC)
show(ACC)

### Parte 2

### 1. ValidaÃÂ§ao cruzada
### 1.1 InstalaÃÂ§ÃÂ£o da biblioteca

### 1.2 Gerar os Indices das pastas
num_pastas = 3 # 3-fold-cross validation estratificado
idxteste = createFolds(tabela$RATING, k = num_pastas)

### 1.3 Gerar as pastas
tabTR1 = tabela[-idxteste$Fold1, ] # treino 1
tabTR2 = tabela[-idxteste$Fold2, ] # treino 2
tabTR3 = tabela[-idxteste$Fold3, ] # treino 3
write.csv(tabTR1,file="tabTR1.csv")
write.csv(tabTR2,file="tabTR2.csv")
write.csv(tabTR3,file="tabTR3.csv")

tabTS1 = tabela[idxteste$Fold1, ] # teste 1
tabTS2 = tabela[idxteste$Fold2, ] # teste 2
tabTS3 = tabela[idxteste$Fold3, ] # teste 3
write.csv(tabTR1,file="tabTS1.csv")
write.csv(tabTR2,file="tabTS2.csv")
write.csv(tabTR3,file="tabTS3.csv")


### 1.1 Estimativa do modelo de regressao logistica
RL1 = glm(RATING ~ ESTADO_CIVIL + RENDA_MUTUARIO + SISTEMA_AMORT + SEXO_MUTUARIO +
				  IDADE_MUTUARIO + FPOC_VLR_DIVIDA_VENCIDA + VLR_ENC_ATR + REGIAO,
                family="binomial" , data = tabela)

RL2 = glm(RATING ~ ESTADO_CIVIL + RENDA_MUTUARIO + SISTEMA_AMORT + SEXO_MUTUARIO +
				  IDADE_MUTUARIO + FPOC_VLR_DIVIDA_VENCIDA + VLR_ENC_ATR + REGIAO,
          family="binomial" , data = tabTR2)

RL3 = glm(RATING ~ ESTADO_CIVIL + RENDA_MUTUARIO + SISTEMA_AMORT + SEXO_MUTUARIO +
				  IDADE_MUTUARIO + FPOC_VLR_DIVIDA_VENCIDA + VLR_ENC_ATR + REGIAO,
          family="binomial" , data = tabTR3)

### 1.2 Metricas
predRL1 = predict(RL1, newdata = tabTS1, type = "response")
predRL2 = predict(RL2, newdata = tabTS2, type = "response")
predRL3 = predict(RL3, newdata = tabTS3, type = "response")

#tabela$RATING_RL = predRL

### 1.3 Matriz de confusÃÂ£o
predRL1 = predRL1 > 0.5 # faz a limiarizaÃÂ§ao
predRL2 = predRL2 > 0.5 # faz a limiarizaÃÂ§ao
predRL3 = predRL3 > 0.5 # faz a limiarizaÃÂ§ao

MC1 = table(tabTS1$RATING, 
           predRL1, deparse.level = 2) # montar a matriz de confusaoo
MC2 = table(tabTS2$RATING, 
            predRL2, deparse.level = 2) # montar a matriz de confusao
MC3 = table(tabTS3$RATING, 
            predRL3, deparse.level = 2) # montar a matriz de confusao

ACC1 = sum(diag(MC1))/sum(MC1)
ACC2 = sum(diag(MC2))/sum(MC2)
ACC3 = sum(diag(MC3))/sum(MC3)

ACC = (ACC1+ACC2+ACC)/3

### Parte 3: Selecao de variaveis

### 1 Pre seleÃÂ§ÃÂ£o de variaveis

### 1.1 Instalacao da biblioteca

### 1.2 Remover variaveis por separacao
estetica = aes(x = log(IDADE_MUTUARIO), fill = RATING) # estetica do grafico
grafico = ggplot(data = tabela, estetica)
grafico + geom_density(alpha = 0.4) + facet_wrap(~ ESTADO_CIVIL)

estetica = aes(y = log(SEXO_MUTUARIO), x = RATING) # estetica do grafico
grafico = ggplot(data = tabela, estetica)
grafico + geom_violin(alpha = 0.4) + facet_wrap(~ ESTADO_CIVIL) 

### 1.3 Remover variaveis por similaridade
tabcont = tabela[, c(4, 6, 7, 8, 10, 11, 12)] # variaveis continuas
C = cor(tabcont) # matriz de correlacao
corrplot(C)


### 1.1 Estimativa do modelo de regressao logistica
RL1 = glm(RATING ~ ESTADO_CIVIL + RENDA_MUTUARIO + SISTEMA_AMORT + SEXO_MUTUARIO +
				  IDADE_MUTUARIO + FPOC_VLR_DIVIDA_VENCIDA + VLR_ENC_ATR + REGIAO,  
          family="binomial" , data = tabTR1)
RL1 = step(RL1, direction = "backward")

RL2 = glm(RATING ~ ESTADO_CIVIL + RENDA_MUTUARIO + SISTEMA_AMORT + SEXO_MUTUARIO +
				  IDADE_MUTUARIO + FPOC_VLR_DIVIDA_VENCIDA + VLR_ENC_ATR + REGIAO,  
          family="binomial" , data = tabTR2)
RL2 = step(RL2, direction = "backward")

RL3 = glm(RATING ~ ESTADO_CIVIL + RENDA_MUTUARIO + SISTEMA_AMORT + SEXO_MUTUARIO +
				  IDADE_MUTUARIO + FPOC_VLR_DIVIDA_VENCIDA + VLR_ENC_ATR + REGIAO, 
          family="binomial" , data = tabTR3)
RL3 = step(RL3, direction = "backward")

### 1.2 Metricas
predRL1 = predict(RL1, newdata = tabTS1, type = "response")
predRL2 = predict(RL2, newdata = tabTS2, type = "response")
predRL3 = predict(RL3, newdata = tabTS3, type = "response")

#tabela$RATING_RL = predRL

### 1.3 Matriz de confusao
predRL1 = predRL1 > 0.5 # faz a limiarizacao
predRL2 = predRL2 > 0.5 # faz a limiarizacao
predRL3 = predRL3 > 0.5 # faz a limiarizacao

MC1 = table(tabTS1$RATING, 
            predRL1, deparse.level = 2) # montar a matriz de confusao
MC2 = table(tabTS2$RATING, 
            predRL2, deparse.level = 2) # montar a matriz de confusao
MC3 = table(tabTS3$RATING, 
            predRL3, deparse.level = 2) # montar a matriz de confusao

ACC1 = sum(diag(MC1))/sum(MC1)
ACC2 = sum(diag(MC2))/sum(MC2)
ACC3 = sum(diag(MC3))/sum(MC3)

ACC = (ACC1+ACC2+ACC)/3

summary(RL2)