# Estratégia SWAP

# Limpa workspace
rm(list=ls())

library(readxl)
library(dplyr)

# Importa base ####
this.dir <- getwd()
pos = gregexpr('/', this.dir)
ultima_barra = unlist(pos)[length(unlist(pos))]

data.dir = paste(substr(this.dir,1,ultima_barra),"Data",sep="")

setwd(data.dir)


df_ipca = read_excel("IPCA_SWAP_bbg.xlsx", sheet = "1", na = c("NA","VAZIO","#NOME?","#N/A N/A"))
df_ipca15 = read_excel("IPCA_SWAP_bbg.xlsx", sheet = "3", na = c("NA","VAZIO","#NOME?","#N/A N/A"))
df_swap = read_excel("IPCA_SWAP_bbg.xlsx", sheet = "2", na = c("NA","VAZIO","#NOME?","#N/A N/A"))

df_ipca = df_ipca[-1,]
df_ipca15 = df_ipca15[-1,]
df_swap = df_swap[-1,]

colnames(df_ipca) = c("data_mensal","IPCA","Consenso","Release")
colnames(df_ipca15) = c("data_mensal","IPCA_15","Consenso","Release")
colnames(df_swap) = c("data_diaria","Swap")
df_swap$data_diaria = as.Date(df_swap$data_diaria, format="Y%-m%-d%")

# Transforma Release (bbg) em data
df_dt_release = df_ipca %>% mutate(dt_release = as.Date(paste(substr(Release,7,8), substr(Release,5,6),
                                              substr(Release,1,4),sep="/"),format="%d/%m/%Y"))

df_dt_release_15 = df_ipca15 %>% mutate(dt_release = as.Date(paste(substr(Release,7,8), substr(Release,5,6),
                                                              substr(Release,1,4),sep="/"),format="%d/%m/%Y"))

df_ipca_new = df_ipca
df_ipca_new$data_mensal = df_dt_release$dt_release
df_ipca15$data_mensal = df_dt_release_15$dt_release
df_ipca_new = df_ipca_new %>% mutate(surpresa = IPCA - Consenso)
df_ipca15_new = df_ipca15 %>% mutate(supresa15 = IPCA_15 - Consenso)

ret = c(0,diff(log(df_swap$Swap)))

# Parâmetros ####
inicio = min(which(df_ipca_new$data_mensal > df_swap$data_diaria[1]))
fim = nrow(df_ipca_new)-1
retorno_amostra_inteira = matrix(0,length(1:21),1)
sharpe = matrix(0,length(1:21),1)


for (jj in 2:21) { # 
  holding_period = jj
  
  # Matrizes auxiliares
  pnl_surpresa = matrix(0,nrow = nrow(df_ipca_new),ncol = holding_period)
  pnl_surpresa_acumulado = matrix(0,nrow = nrow(df_ipca_new), ncol = (1+holding_period))
  position_surpresa = matrix(0,nrow = nrow(df_ipca_new),1)
  pnl_encadeado = matrix(0,nrow = (nrow(df_ipca_new)*holding_period), ncol = 1)
  data_surpresa = data.frame("Data"=rep(as.Date(NA),(nrow(df_ipca_new))))
  data_surpresa_diaria = data.frame("Data"=rep(as.Date(NA),(nrow(df_ipca_new))))
  
  # Loop backtest ####
  
  for (ii in inicio:fim){
    local = which(df_swap$data_diaria == df_ipca_new$data_mensal[ii])
    position_surpresa[ii,1] = sign(df_ipca_new$surpresa[ii])
    pnl_surpresa[ii,] = position_surpresa[ii,1] * ret[(local+1):(local+holding_period)]
    pnl_surpresa_acumulado[ii,2:ncol(pnl_surpresa_acumulado)] = cumsum(pnl_surpresa[ii,])
    # check de datas
    data_surpresa$Data[ii] = df_ipca_new$data_mensal[ii]
    data_surpresa_diaria[ii,1] = df_swap$data_diaria[local]
    }
  
  retorno = rowSums(pnl_surpresa[inicio:fim,], na.rm = TRUE)
  retorno_amostra_inteira[jj,1] = (sum(retorno))
  # hist(retorno)
  
  pnl_encadeado = as.vector(t(pnl_surpresa_acumulado[inicio:fim,]))
  sharpe[jj,1] = mean(pnl_encadeado, na.rm = TRUE)/sd(pnl_encadeado, na.rm = TRUE)*sqrt(252)
  plot(cumsum(pnl_encadeado), type="lyne")
  title(paste("Holding Period de ",toString(holding_period)," dias"))
  Sys.sleep(0.5)

}

# Relatórios ####

matplot(t(pnl_surpresa_acumulado[inicio:fim,]), type = 'l', 
        lty = 1, color = "black", xlab = "Dias", ylab = "Retorno Acumulado", 
        main = "Trajetórias por release")

hist(pnl_surpresa_acumulado[inicio:fim,ncol(pnl_surpresa_acumulado)], 
     main = "Distribuição por release do IPCA", xlab = "PnL Acumulado")

# Excluindo outliers
x = pnl_surpresa_acumulado[inicio:fim,ncol(pnl_surpresa_acumulado)]
qnt = quantile(x, probs=c(.25, .75), na.rm = TRUE)
H = 1.5 * IQR(x, na.rm = TRUE)
y = x
y[x < (qnt[1] - H)] <- NA
y[x > (qnt[2] + H)] <- NA

hist(y,breaks = seq(from = -0.15, to = 0.15, by = 0.025),
     main = "Distribuição por release do IPCA ex-outliers", 
     xlab = "PnL Acumulado")

(retorno_medio = mean(pnl_surpresa_acumulado[inicio:fim,ncol(pnl_surpresa_acumulado)],na.rm = TRUE))
(retorno_medio_exoutlier = mean(y,na.rm = TRUE))

