# Função que cria z-score ao longo do tempo

zscore_no_tempo = function(df) {
  serie_vazia = matrix(NA,length(df),1)
  serie_zscore = data.frame("ZScore" = serie_vazia)
  for (ii in 12:nrow(serie_zscore)){
    media = mean(df[1:ii-1], na.rm = TRUE)
    desvio = sd(df[1:ii-1], na.rm = TRUE)
    serie_zscore[ii,1] = (df[ii]-media)/desvio
  }
  return(serie_zscore)
}