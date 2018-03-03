# Forecast da Surpresa

# limpa workspace
rm(list=ls())

library(readxl)
library(dplyr)

# Importa base de dados ####
this.dir <- getwd()
pos = gregexpr('/', this.dir)
ultima_barra = unlist(pos)[length(unlist(pos))]

data.dir = paste(substr(this.dir,1,ultima_barra),"Data/Analistas_BBG",sep="")
setwd(data.dir)

bbg1 = read_excel("bbg1_C.xlsx", sheet = 1, skip = 2, na = c("NA","VAZIO","#NOME?","#N/A N/A","--"))
bbg2 = read_excel("bbg2_C.xlsx", sheet = 1, skip = 2, na = c("NA","VAZIO","#NOME?","#N/A N/A","--"))
consenso = read_excel("consenso.xlsx", sheet = 1, na = c("NA","VAZIO","#NOME?","#N/A N/A","--"))

analistas = bbg1 %>% select(-starts_with("As of"))
colnames(analistas)[3] = "Actual"
colnames(consenso) = c("Data","Consenso")

analistas = analistas[-4] # exclui a coluna "Firm"

drop_cols = c("Observation Date","X__1","Firm")
outros_analistas = bbg2 %>% select(-starts_with("As of")) %>% select(-one_of(drop_cols))

analistas_total = analistas %>% left_join(outros_analistas, by = "Release Date")

df_analistas = analistas_total %>% left_join(consenso, by = c("Observation Date"="Data"))

# Fun??es ####

pesos_forecast = function(score_analistas,numero_analistas) {
  melhores_analistas = order(score_analistas, decreasing = TRUE)
  pesos = seq(from = numero_analistas, to = 1, by = -1)/sum(seq(1:numero_analistas))
  return(pesos)
}

# Par?metros ####
inicio = 18
fim = nrow(df_analistas)
num_analistas = 1

# Constr?i Forecast da Surpresa ####
surpresa = df_analistas[,3] - df_analistas[,ncol(df_analistas)]
n = ncol(df_analistas[,4:ncol(df_analistas)])
surpresa_analistas = df_analistas[,4:ncol(df_analistas)] - 
  matrix(rep(df_analistas[,ncol(df_analistas)],each=n), ncol = n, byrow = TRUE)

acertos = matrix(0,nrow = nrow(surpresa_analistas), ncol = ncol(surpresa_analistas))
erros = matrix(0,nrow = nrow(surpresa_analistas), ncol = ncol(surpresa_analistas))
lado_surpresa = matrix(0,nrow = nrow(surpresa_analistas), ncol = ncol(surpresa_analistas))
n_surpresa = do.call("cbind", replicate(n, surpresa, simplify = FALSE))

teste = sign(surpresa_analistas) == sign(n_surpresa)

lado_surpresa[teste] = 1
lado_surpresa[!teste] = -1
lado_surpresa[is.na(teste)] = 0

forecast_ipca = matrix(0,nrow = nrow(df_analistas), ncol = 1)
ea_forecast = matrix(0,nrow = nrow(df_analistas), ncol = 1)

for (ii in inicio:fim){
  # cria forecast
  score = apply(lado_surpresa[1:ii,],2,cumsum)
  ind = is.na(df_analistas[ii,4:ncol(df_analistas)])
  score[ii,ind] = 0
  melhores_analistas = order(score[ii,], decreasing = TRUE) + 3
  pesos = seq(from = num_analistas, to = 1, by = -1)/sum(seq(1:num_analistas))
  forecast_ipca[ii,1] = sum(pesos * df_analistas[ii,melhores_analistas[1:num_analistas]])
  # calcula o Erro Absoluto
  ea_forecast[ii,1] = abs(df_analistas$Actual[ii] - forecast_ipca[ii,1])
}

(mean(ea_forecast[inicio:fim], na.rm = TRUE))
(mean((abs(surpresa[inicio:fim,1])), na.rm = TRUE))


