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

df_swap = read_excel(file, sheet = "SWAP", skip = 0, na = c("NA","VAZIO","#NOME?","#N/A N/A"))

colnames(df_ipca) = c("Data","IPCA","Consenso","Release")
colnames(df_swap) = c("Data","Swap")

df_swap$Data = as.Date(df_swap$Data, format="Y%-m%-d%")




#....

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

