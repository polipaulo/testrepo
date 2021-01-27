


### pacotes

library(dplyr)
library(gbm)
library(excel.link)
library(lubridate)
library(clipr)


###increase max print

options(max.print=10000)


### increase memory

#memory.limit(50000)


### tira cientifico

options(scipen = 999)


###########################################################################################################3



#################################  Alternando  ##############################################

### baixa arquivo

setwd("C:/Bitcoin2020/Testes Boosting")
load(file = "DadosSemNas.RData")

names(dados)

origin = dados

dados = select(dados, Data, Close1min)
rownames(dados) = 1:nrow(dados)
head(dados)
tail(dados)

### modelo

direcao = 1
stopporc = 0.01

matrizprecos = dados[1,]
matrizprecos$Sinal = direcao
matrizprecos

price = dados$Close1min[1]
priceloss = price*(1-direcao*stopporc) # fazer por valor fixo
#gatilho = price*(1+direcao*stopporc) # fazer por valor fixo

  
for (lin in 1:nrow(dados)) { 
  
  # lin = lin + 1
  # dados$Close1min[lin]
  
  if (dados$Close1min[lin]*direcao > price*direcao) {  # & dados$Close1min[lin]*direcao > gatilho*direcao
    price = dados$Close1min[lin]
    priceloss = dados$Close1min[lin]*(1-direcao*stopporc)
  }
  
  if (dados$Close1min[lin]*(-direcao) > priceloss*(-direcao)) {
    direcao = direcao #*(-1) ################## ver separar stop gain com stop loss
    price = dados$Close1min[lin]
    priceloss = dados$Close1min[lin]*(1-direcao*stopporc)
    matrizprecos = rbind(matrizprecos,data.frame(dados[lin,],Sinal = direcao))
    
  }
  
}

head(matrizprecos)


### resultados

result = matrizprecos$Close1min
result = ((result[2:length(result)]-lag(result,1)[2:length(result)]))*matrizprecos$Sinal[1:(length(result)-1)]
head(result)

hist(result, breaks = 100)
sum(result)

# perda máxima limitada

min(result)
min(result)/stopporc

data.frame(head(matrizprecos), head(result), head(result/matrizprecos$Close1min))

result[length(result)+1] = 0 # adiciona ultima linha pra ficar igual 
tail(result)

porc = result/matrizprecos$Close1min
sum(porc<(-stopporc)) 

porc[porc<(-stopporc)] = -stopporc # por limite mais conservador
result = porc*matrizprecos$Close1min
sum(porc<(-stopporc))

data.frame(head(matrizprecos), head(result), head(porc))

# novo resultado

sum(result) # contra -45283.68 3107.51

plot(matrizprecos$Close1min, ylim = c(min(min(cumsum(result)),min(matrizprecos$Close1min)), max(max(cumsum(result)),max(matrizprecos$Close1min))))
lines(cumsum(result), col=2)



### inversão 

head(result)
result3 = -result
head(result3)

sum(result3)

hist(result3, breaks = 100)

plot(matrizprecos$Close1min, ylim = c(min(min(cumsum(result3)),min(matrizprecos$Close1min)), max(max(cumsum(result3)),max(matrizprecos$Close1min))))
lines(cumsum(result3), col=2)



### análise result invertido

sum(result3)

sum(result3>0) 
sum(result3<0)
mean(result3>0) # porc
mean(result3<0) # porc

mean(result3[result3<0])
mean(result3[result3>0])
hist(result3[result3>0], breaks = 100)
hist(result3[result3<0], breaks = 100, col = 2)
