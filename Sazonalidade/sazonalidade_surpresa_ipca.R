# Sazonalidade da infla??o - Su?cia

# limpa workspace
rm(list=ls())

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

# Importa base de dados
this.dir <- getwd()
pos = gregexpr('/', this.dir)
ultima_barra = unlist(pos)[length(unlist(pos))]

data.dir = paste(substr(this.dir,1,ultima_barra),"Data",sep="")

df_ipca = read_excel("IPCA_SWAP_bbg.xlsx", sheet = "1", na = c("NA","VAZIO","#NOME?","#N/A N/A"))
df_ipca15 = read_excel("IPCA_SWAP_bbg.xlsx", sheet = "3", na = c("NA","VAZIO","#NOME?","#N/A N/A"))

df_ipca = df_ipca[,-4]
df_ipca15 = df_ipca15[,-4]

colnames(df_ipca) = c("Data","IPCA","Consenso")
colnames(df_ipca15) = c("Data","IPCA15","Consenso")

# PArametro ####
# mes_teste = 1

# Ajusta e limpa a base
df_ipca = df_ipca %>% mutate(surpresa = IPCA - Consenso)
df_ipca = df_ipca %>% mutate(mes = month(Data))

df_ipca15 = df_ipca15 %>% mutate(surpresa15 = IPCA15 - Consenso)
df_ipca15 = df_ipca15 %>% mutate(mes = month(Data))
df_ipca15 = df_ipca15[!is.na(df_ipca15$surpresa15),]

# Compara quando tem surpresa != ZERO no IPCA-15
df_analise = df_ipca15 %>% inner_join(df_ipca, by = "Data")
df_analise = df_analise %>% select(Data, surpresa, surpresa15) %>% filter(surpresa15 !=0)
# df_analise %>% filter(surpresa15 > 0) %>% lm(surpresa ~ surpresa15, data = .) %>% summary()

# Relat?rios ####
df_analise %>% ggplot(aes(x = surpresa15, y = surpresa)) + geom_point() + geom_smooth(method = "lm") +
  ggtitle("Impacto da surpresa de IPCA-15 no IPCA") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("IPCA-15") + ylab("IPCA")
regress = lm(surpresa ~ surpresa15, data = df_analise)
summary(regress)

## testar reg logistica
mes_teste = 9
df_ipca %>% mutate(mes = (month(Data) == mes_teste)) %>% ggplot(aes(x = surpresa)) + theme_bw() +
  geom_density(fill = "green", alpha = 0.3) + facet_grid(mes~.) + ggtitle(paste("Mes ",mes_teste)) +
  theme(plot.title = element_text(hjust = 0.5))

(tabela_ipca = df_ipca %>% group_by(mes) %>%
  summarize(media = mean(surpresa, na.rm = TRUE), mediana = median(surpresa, na.rm = TRUE),
            desvio = sd(surpresa, na.rm = TRUE), prop_surp_positiva = mean(surpresa>0, na.rm = TRUE),
            prop_surp_negativa = mean(surpresa<0, na.rm = TRUE)))

inicio_dap = as.Date("03/03/2015", format = "%d/%m/%Y")
local_inicio_dap = min(which(df_ipca$Data >= inicio_dap))
ipca_reduzido = df_ipca[1:local_inicio_dap,]
(tabela_ipca_reduzido = ipca_reduzido %>% group_by(mes) %>%
    summarize(media = mean(surpresa, na.rm = TRUE), mediana = median(surpresa, na.rm = TRUE),
              desvio = sd(surpresa, na.rm = TRUE), prop_surp_positiva = mean(surpresa>0, na.rm = TRUE),
              prop_surp_negativa = mean(surpresa<0, na.rm = TRUE)))

ipca_pos_swap = df_ipca[(local_inicio_dap+1):nrow(df_ipca),]
(tabela_ipca_pos_swap = ipca_pos_swap %>% group_by(mes) %>%
    summarize(media = mean(surpresa, na.rm = TRUE), mediana = median(surpresa, na.rm = TRUE),
              desvio = sd(surpresa, na.rm = TRUE), prop_surp_positiva = mean(surpresa>0, na.rm = TRUE),
              prop_surp_negativa = mean(surpresa<0, na.rm = TRUE)))

(tabela_ipca15 = df_ipca15 %>% group_by(mes) %>%
  summarize(media = mean(surpresa15, na.rm = TRUE), mediana = median(surpresa15, na.rm = TRUE),
            desvio = sd(surpresa15, na.rm = TRUE), prop_surp_positiva = mean(surpresa15>0, na.rm = TRUE),
            prop_surp_negativa = mean(surpresa15<0, na.rm = TRUE)))

