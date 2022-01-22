rm(list = ls()); gc()
numCluster <- 7

## -- Dados ----
cam <- "/media/luis/5B8F-C6B3/06_AplicacaoSentimento/01_Bases" #"/home/leste/06_AplicacaoSentimento/01_Bases"
camSalvar <- "/media/luis/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/matDTM" #"/home/leste/06_AplicacaoSentimento/01_Bases/matDTM"
# cluster
load(file.path(cam, "cluster980000DefMelhorClusterInduzido20000.RData"))
# features do Info Gain
load(file = file.path(cam, "featuresManter.RData"))
# df com termos por id
load(file = file.path(cam, "freqMatDTMAuxPalComFreqMin09.RData"))     # AuxPalComFreqMin

## -- Pacotes ----     
library(data.table)

## -- Separando bases por Grupos/Clusters ----
aux <- data.frame(matDissimilUnseenDataDT[, c("rn", "medoid")])
aux$"medoid" <- factor(x = aux$"medoid", levels = seq(numCluster), labels = seq(numCluster))
aux$"rn" <- as.character(aux$"rn")
aux$"rn" <- formatC(x = aux$"rn", digits = 0, format = "f")

## -- Mantendo apenas as features Info Gain nos Dados empilhados ----
length(unique(freqMatDTMAuxPalComFreqMin$"word"))

entradasTabelaoManter <- which(freqMatDTMAuxPalComFreqMin$"word" %in% featuresManter$"term")
freqMatDTMInfoGain <- freqMatDTMAuxPalComFreqMin[entradasTabelaoManter,]
freqMatDTMInfoGain$"id" <- as.character(freqMatDTMInfoGain$"id")
freqMatDTMInfoGain$"id" <- formatC(x = freqMatDTMInfoGain$"id", digits = 0, format = "f")
length(unique(freqMatDTMInfoGain$"id"))
length(unique(freqMatDTMInfoGain$"word"))

rm(freqMatDTMAuxPalComFreqMin); rm(entradasTabelaoManter); gc(reset = TRUE)

## -- Juntando ----
dfComplete <- merge(freqMatDTMInfoGain, aux, by.x = "id", by.y = "rn")
dfComplete$"id" <- formatC(x = as.numeric(dfComplete$"id"), digits = 0, format = "f")

length(unique(dfComplete$'id'))
length(unique(dfComplete$'word'))

listaGruposDados <- split(x = dfComplete, f = dfComplete$"medoid")

rm(dfComplete); rm(matDissimilUnseenDataDT); rm(freqMatDTMInfoGain); gc(reset = TRUE)

## -- Matrizes DTM ----
grupo <- 2
for(grupo in seq(numCluster)[-c(1, 2)]){
        df <- listaGruposDados[[grupo]] 
        df$"medoid" <- NULL
        df <- data.table(df)
  
        tCom <- Sys.time(); tCom
        matDTMCast <- dcast.data.table(df,
                                       id ~ word, fun = sum,
                                       value.var = "n")
        tFim <- Sys.time(); tFim
        tFim - tCom
        
        matDTMCast <- as.data.frame(matDTMCast)
        
        ## -- Salvando ----
        grupoCh <- ifelse(nchar(grupo) == 1, paste0("0", grupo), as.character(grupo))
        nomeArqSalvar <- paste0("matDTMCast", grupoCh, ".RData", collapse = "")
        save(matDTMCast, file = file.path(camSalvar, nomeArqSalvar))
        
        rm(matDTMCast); rm(df); gc()
}

