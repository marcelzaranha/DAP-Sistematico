# Top5 ####
# as série de forecast pode ser usada até 1 dia antes do release
# ex. release dt 08/12/2017 usar série para prever até 07/12/2017
# a partir daí a série referente ao release de jan/18

# limpa workspace ####
rm(list=ls())

library(dplyr)

# importa base de dados ####
# setwd("C:/Users/Marce/Desktop/Git_Rep/DAP-Sistematico/Data/Expectativa_BC")

ajusta_top5 = function(nome) {
  df = read.csv(paste(nome,".csv",sep=""))
  df$data = as.Date(df$data, format = "%Y-%m-%d")
  output = matrix(NA, nrow = nrow(df), ncol = 12)
  
  nomes_colunas = c("Data","t+1","t+2","t+3","t+4","t+5","t+6",
                    "t+7","t+8","t+9","t+10","t+11","t+12")
  
  ind = is.na(df[,2])
  output[!ind,] = as.matrix(df[!ind,2:13])
  output[ind,] = as.matrix(df[ind,3:14])
  
  output_top5 = data.frame(df$data, output)
  colnames(output_top5) = nomes_colunas
  return(output_top5)
}

# medianas
df_mediana_cp = ajusta_top5("mediana_c")
df_mediana_mp = ajusta_top5("mediana_m")
df_mediana_lp = ajusta_top5("mediana_l")

# medias
df_media_cp = ajusta_top5("media_c")
df_media_mp = ajusta_top5("media_m")
df_media_lp = ajusta_top5("media_l")

# desvios
df_desvio_cp = ajusta_top5("desvio_c")
df_desvio_mp = ajusta_top5("desvio_m")
df_desvio_lp = ajusta_top5("desvio_l")

# Join dos data.frames's ####

top5_lp = top5_mediana_cp %>% left_join(top5_media_cp, by = "Data") %>%
  left_join(top5_desvio_cp, by = "Data")



# rm(list = setdiff(ls(),"top5_mp"))
# 
# save.image("top5_mp.RData")
