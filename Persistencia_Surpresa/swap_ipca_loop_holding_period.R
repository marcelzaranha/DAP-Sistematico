# Estratégia SWAP

# Limpa workspace
rm(list=ls())

library(ggplot2)
library(readxl)
library(quantmod)
library(dplyr)
library(tidyr)
library(purrr)

# Importa base ####
this.dir <- dirname(parent.frame(2)$ofile)
pos = gregexpr('/', this.dir)
ultima_barra = unlist(pos)[length(unlist(pos))]

data.dir = paste(substr(this.dir,1,ultima_barra),"Data",sep="")

setwd(data.dir)


df_ipca = read_excel("IPCA_SWAP_bbg.xlsx", sheet = "1", na = c("NA","VAZIO","#NOME?","#N/A N/A"))
df_swap = read_excel("IPCA_SWAP_bbg.xlsx", sheet = "2", na = c("NA","VAZIO","#NOME?","#N/A N/A"))

df_ipca = df_ipca[-1,]
df_swap = df_swap[-1,]

colnames(df_ipca) = c("data_mensal","IPCA","Consenso","Release")
colnames(df_swap) = c("data_diaria","Swap")
df_swap$data_diaria = as.Date(df_swap$data_diaria, format="Y%-m%-d%")

# Transforma Release (bbg) em data
df_dt_release = df_ipca %>% mutate(dt_release = as.Date(paste(substr(Release,7,8), substr(Release,5,6),
                                              substr(Release,1,4),sep="/"),format="%d/%m/%Y"))

df_ipca_new = df_ipca
df_ipca_new$data_mensal = df_dt_release$dt_release
df_ipca_new = df_ipca_new %>% mutate(surpresa = IPCA - Consenso)

ret = c(0,diff(log(df_swap$Swap)))

# Par?metros ####
inicio = min(which(df_ipca_new$data_mensal > df_swap$data_diaria[1]))
fim = nrow(df_ipca_new)-2
retorno_amostra_inteira = matrix(0,length(1:21),1)
sharpe = matrix(0,length(1:21),1)


for (jj in 2:21) { # 
  holding_period = jj
  
  # Matrizes auxiliares
  pnl_surpresa = matrix(0,nrow = nrow(df_ipca_new),ncol = holding_period)
  pnl_surpresa_acumulado = matrix(0,nrow = nrow(df_ipca_new), ncol = (1+holding_period))
  position_surpresa = matrix(0,nrow = nrow(df_ipca_new),1)
  pnl_encadeado = matrix(0,nrow = (nrow(df_ipca_new)*holding_period), ncol = 1)
  
  # Loop backtest ####
  
  for (ii in inicio:fim){
    local = which(df_swap$data_diaria == df_ipca_new$data_mensal[ii])
    position_surpresa[ii,1] = sign(df_ipca_new$surpresa[ii])
    pnl_surpresa[ii,] = position_surpresa[ii,1] * ret[(local+1):(local+holding_period)]
    pnl_surpresa_acumulado[ii,2:ncol(pnl_surpresa_acumulado)] = cumsum(pnl_surpresa[ii,])
  }
  
  retorno = rowSums(pnl_surpresa[inicio:fim,], na.rm = TRUE)
  retorno_amostra_inteira[jj,1] = (sum(retorno))
  # hist(retorno)
  
  pnl_encadeado = as.vector(t(pnl_surpresa_acumulado[inicio:fim,]))
  sharpe[jj,1] = mean(pnl_encadeado, na.rm = TRUE)/sd(pnl_encadeado, na.rm = TRUE)*sqrt(252)
  plot(cumsum(pnl_encadeado), type="lyne")
  title(paste("Holding Perido de ",toString(holding_period)," dias"))
  Sys.sleep(0.5)

}

# Relat?rios ####

df_pnl = data.frame("PnL" = t(pnl_surpresa_acumulado[inicio:fim,]))
df_pnl_plot = gather(df_pnl,Evento,Valor)
df_pnl_plot = cbind(df_pnl_plot,rep(rownames(df_pnl),NCOL(df_pnl)))
colnames(df_pnl_plot) = c("Evento","Valor","Dia")
ggplot(df_pnl_plot, aes(x = Dia, y = Valor, color = Evento)) + geom_line()

# df_sharpe = data.frame("Sharpe" = sharpe)
# ggplot(df_sharpe, aes(sharpe) + geom_density(alpha = 0.3, fill = "blue") + theme_bw()

# for (ii in inicio:fim){
#   plot(pnl_surpresa_acumulado[ii,], type="lyne")
#   Sys.sleep(0.5)
# }

