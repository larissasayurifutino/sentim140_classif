rm(list = ls()); gc()
load(file = "/home/leste/DBSET/Doutorado/BaseDadosTese/06_AplicacaoSentimento/01_Bases/freqMatDTMAuxPalComFreqMin.RData")

library(tm)
library(tidytext)


tCom <- Sys.time(); tCom
matDTMCast <- cast_dtm(data = freqMatDTMAuxPalComFreqMin, 
                       document = "id",
                       term =  "word", 
                       value = "n", 
                       weighting = function(x) weightSMART(x, spec = "bnn"))
tFim <- Sys.time(); tFim

save(matDTMCast, file = "/home/leste/DBSET/Doutorado/BaseDadosTese/06_AplicacaoSentimento/01_Bases/matDTMCast2000Terms.RData")
