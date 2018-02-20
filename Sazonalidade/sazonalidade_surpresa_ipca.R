# Sazonalidade da inflação - Suécia

# limpa workspace
rm(list=ls())

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

# Importa base de dados
setwd("Z:/Economia/Marcel/Inflation/IPCA/Code/Surpresa")
df_ipca = read_excel("BZ_INFL.xlsx", sheet = "IPCA", skip = 0, na = c("NA","VAZIO","#NOME?","#N/A N/A"))
df_ipca15 = read_excel("BZ_INFL.xlsx", sheet = "IPCA15", skip = 0, na = c("NA","VAZIO","#NOME?","#N/A N/A"))

# df_ipca = df_ipca[-1,]
colnames(df_ipca) = c("Data","IPCA","Consenso")
colnames(df_ipca15) = c("Data","IPCA15","Consenso")

# PArâmetro ####
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

# Relatórios ####
df_analise %>% ggplot(aes(x = surpresa15, y = surpresa)) + geom_point() + geom_smooth(method = "lm") +
  ggtitle("Impacto da surpresa de IPCA-15 no IPCA") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("IPCA-15") + ylab("IPCA")
regress = lm(surpresa ~ surpresa15, data = df_analise)
summary(regress)

## testar reg logística
mes_teste = 9
df_ipca %>% mutate(mes = (month(Data) == mes_teste)) %>% ggplot(aes(x = surpresa)) + theme_bw() +
  geom_density(fill = "green", alpha = 0.3) + facet_grid(mes~.) + ggtitle(paste("Mês ",mes_teste)) +
  theme(plot.title = element_text(hjust = 0.5))

(tabela_ipca = df_ipca %>% group_by(mes) %>%
  summarize(media = mean(surpresa, na.rm = TRUE), mediana = median(surpresa, na.rm = TRUE),
            desvio = sd(surpresa, na.rm = TRUE), prop_surp_positiva = mean(surpresa>0, na.rm = TRUE),
            prop_surp_negativa = mean(surpresa<0, na.rm = TRUE)))

ipca_reduzido = df_ipca[1:96,]
(tabela_ipca_reduzido = ipca_reduzido %>% group_by(mes) %>%
    summarize(media = mean(surpresa, na.rm = TRUE), mediana = median(surpresa, na.rm = TRUE),
              desvio = sd(surpresa, na.rm = TRUE), prop_surp_positiva = mean(surpresa>0, na.rm = TRUE),
              prop_surp_negativa = mean(surpresa<0, na.rm = TRUE)))

ipca_post_swap = df_ipca[96:nrow(df_ipca),]
(tabela_ipca_post_swap = ipca_post_swap %>% group_by(mes) %>%
    summarize(media = mean(surpresa, na.rm = TRUE), mediana = median(surpresa, na.rm = TRUE),
              desvio = sd(surpresa, na.rm = TRUE), prop_surp_positiva = mean(surpresa>0, na.rm = TRUE),
              prop_surp_negativa = mean(surpresa<0, na.rm = TRUE)))

(tabela_ipca15 = df_ipca15 %>% group_by(mes) %>%
  summarize(media = mean(surpresa15, na.rm = TRUE), mediana = median(surpresa15, na.rm = TRUE),
            desvio = sd(surpresa15, na.rm = TRUE), prop_surp_positiva = mean(surpresa15>0, na.rm = TRUE),
            prop_surp_negativa = mean(surpresa15<0, na.rm = TRUE)))

