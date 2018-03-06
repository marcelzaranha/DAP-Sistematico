# Índice de Surpresa e backtest

# limpa workspace
rm(list=ls())

library(readxl)
library(dplyr)
library(quantmod)
library(lubridate)
library(ggplot2)

# Importa base de dados ####
this.dir <- getwd()
source(paste(this.dir,"/zscore_no_tempo.R", sep = ""))

pos = gregexpr('/', this.dir)
ultima_barra = unlist(pos)[length(unlist(pos))]

data.dir = paste(substr(this.dir,1,ultima_barra),"Data",sep="")

file = paste(data.dir,"/INFL_geral_bbg.xlsx", sep = "")

df_ipca = read_excel(file, sheet = "IPCA", skip = 0, na = c("NA","VAZIO","#NOME?","#N/A N/A"))
df_ipca15 = read_excel(file, sheet = "IPCA15", skip = 0, na = c("NA","VAZIO","#NOME?","#N/A N/A"))
df_swap = read_excel(file, sheet = "SWAP", skip = 0, na = c("NA","VAZIO","#NOME?","#N/A N/A"))
df_juros = read_excel(file, sheet = "PREDI", skip = 0, na = c("NA","VAZIO","#NOME?","#N/A N/A"))

colnames(df_ipca) = c("Data","IPCA","Consenso","Release")
colnames(df_ipca15) = c("Data","IPCA","Consenso","Release")
colnames(df_swap) = c("Data","Swap")
colnames(df_juros) = c("Data","PREDI360")

df_swap$Data = as.Date(df_swap$Data, format="Y%-m%-d%")
df_juros$Data = as.Date(df_juros$Data, format="Y%-m%-d%")

# Transforma Release (bbg) em data & trata NA's ####
# IPCA
df_dt_release = df_ipca %>% mutate(dt_release = as.Date(paste(substr(Release,7,8), substr(Release,5,6),
                                                              substr(Release,1,4),sep="/"),format="%d/%m/%Y"))

df_ipca_new = df_ipca
df_ipca_new$Data = df_dt_release$dt_release
df_ipca_new = df_ipca_new %>% mutate(surp_IPCA = IPCA - Consenso)

# IPCA-15
df_dt_release = df_ipca15 %>% mutate(dt_release = as.Date(paste(substr(Release,7,8), substr(Release,5,6),
                                                                substr(Release,1,4),sep="/"),format="%d/%m/%Y"))

df_ipca15_new = df_ipca15
df_ipca15_new$Data = df_dt_release$dt_release
df_ipca15_new = df_ipca15_new %>% mutate(surp_IPCA15 = IPCA - Consenso)

# Outros...

# Trata NA's
teste_logico = which(is.na(df_ipca15_new$surp_IPCA15))==seq(1:nrow(df_ipca15_new))
local_inicio = min(which(teste_logico==FALSE))
df_ipca15_new = df_ipca15_new[(local_inicio):nrow(df_ipca15_new),]

df_ipca_new$surp_IPCA = na.locf(df_ipca_new$surp_IPCA)
df_ipca15_new$surp_IPCA15 = na.locf(df_ipca15_new$surp_IPCA15)

# Contrói Índice de Surpresa
zscore_surp_IPCA = zscore_no_tempo(df_ipca_new$surp_IPCA)
zscore_surp_IPCA15 = zscore_no_tempo(df_ipca15_new$surp_IPCA15)

df_ipca_new = df_ipca_new %>% bind_cols(zscore_surp_IPCA)
colnames(df_ipca_new) = c("Data","IPCA","Consenso",
                          "Release","surp_IPCA","zscore_surp_ipca")
df_ipca15_new = df_ipca15_new %>% bind_cols(zscore_surp_IPCA5)
colnames(df_ipca15_new) = c("Data","IPCA15","Consenso",
                          "Release","surp_IPCA15","zscore_surp_ipca15")

# Join dos data.frames
df_backtest = left_join(df_juros, df_swap, by = "Data")
df_backtest = left_join(df_backtest, df_ipca_new, by = "Data")
df_backtest = left_join(df_backtest, df_ipca15_new, by = "Data")

df_backtest_final = df_backtest %>% select(c("Data","Swap", "zscore_surp_ipca",
                                             "zscore_surp_ipca15")) %>% filter(Data >= df_swap$Data[2])

df_backtest_final$zscore_surp_ipca[1] = 0
df_backtest_final$zscore_surp_ipca15[1] = 0

df_backtest_final$zscore_surp_ipca = na.locf(df_backtest_final$zscore_surp_ipca)
df_backtest_final$zscore_surp_ipca15 = na.locf(df_backtest_final$zscore_surp_ipca15)
df_backtest_final$Swap = na.locf(df_backtest_final$Swap)

# Backtest
ret = c(0,diff((df_backtest_final$Swap)))
retorno_estrategia = matrix(0,nrow = nrow(df_backtest_final),ncol = 1)

inicio = 252
fim = nrow(df_backtest_final)-1

for (ii in inicio:fim){
  position = sign(df_backtest_final[ii,3])
  retorno_estrategia[ii+1] = as.numeric(position) * ret[ii+1]
}

df_graf = data.frame("Data" = df_backtest_final$Data, "Retorno" = retorno_estrategia)
plot(df_backtest_final$Data,cumsum(retorno_estrategia),type = "line")

df_graf %>% summarize(sharpe = mean(Retorno)/sd(Retorno)*sqrt(252))

