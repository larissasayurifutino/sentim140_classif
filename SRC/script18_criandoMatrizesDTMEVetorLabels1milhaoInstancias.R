rm(list = ls()); gc()
numCluster <- 7

## -- Pacotes ----     
library(data.table)
library(dplyr)

## -- Dados ----
cam <- "/media/luis/5B8F-C6B3/06_AplicacaoSentimento/01_Bases" #"/home/leste/06_AplicacaoSentimento/01_Bases"
camSalvar <- "/media/luis/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/matDTM" #"/home/leste/06_AplicacaoSentimento/01_Bases/matDTM"
camMelhorIter <- file.path(cam, "resultClaraAdap", "descritiva")
camAtribInst <- file.path(cam, "resultClaraAdap", "atribInstancias")

# melhorIter
load(file = file.path(camMelhorIter, "melhorIter.RData"))


arqsAtribInst <- list.files(camAtribInst)
arqsAtribInstLoad <- arqsAtribInst[stringr::str_detect(string = arqsAtribInst, pattern = as.character(melhorIter$"iteracao"))]

# cluster
load(file.path(camAtribInst, arqsAtribInstLoad)) # dfCompleto
# features do Info Gain
load(file = file.path(cam, "featuresManter.RData"))
# df com termos por id
load(file = file.path(cam, "freqMatDTMAuxPalComFreqMin09.RData"))     # AuxPalComFreqMin
# Id/document e target/label
load(file.path(cam, "arqClassifRedSemDuplic2.RData"))

## -- Id/document e target/label ----
docTarget <- arqClassifRedSemDuplic2[, c("id", "target")]
docTarget <- unique(docTarget)
rm(arqClassifRedSemDuplic2); gc(reset = TRUE)
docTarget$"id" <- formatC(x = as.numeric(docTarget$"id"), digits = 0, format = "f")


## -- Separando bases por Grupos/Clusters ----
cl <- data.frame(dfCompleto[, c("document", "medoid")])
cl$"medoid" <- factor(x = cl$"medoid", levels = unique(cl$medoid), labels = unique(cl$medoid))
#cl$"document" <- formatC(x = cl$"document", digits = 0, format = "f")
cl$"document" <- as.character(cl$"document")


## -- Mantendo apenas as features Info Gain nos Dados empilhados ----
length(unique(freqMatDTMAuxPalComFreqMin$"word"))

entradasTabelaoManter <- which(freqMatDTMAuxPalComFreqMin$"word" %in% featuresManter$"term")
freqMatDTMInfoGain <- freqMatDTMAuxPalComFreqMin[entradasTabelaoManter,]
freqMatDTMInfoGain$"id" <- as.character(freqMatDTMInfoGain$"id")
#freqMatDTMInfoGain$"id" <- formatC(x = freqMatDTMInfoGain$"id", digits = 0, format = "f")
#length(unique(freqMatDTMInfoGain$"id"))
#length(unique(freqMatDTMInfoGain$"word"))

rm(freqMatDTMAuxPalComFreqMin); rm(entradasTabelaoManter); gc(reset = TRUE)

## -- Juntando freq por termo e medoid ----
if(class(freqMatDTMInfoGain$"id") == class(cl$"document")){
  dfFreqTermoMedoid <- merge(freqMatDTMInfoGain, cl, by.x = "id", by.y = "document")
}
dfFreqTermoMedoid$"id" <- formatC(x = as.numeric(dfFreqTermoMedoid$"id"), digits = 0, format = "f")

length(unique(cl$'document')) # número IDs originalmente
length(unique(dfFreqTermoMedoid$'id')) # número IDs no fim das contas
1 - (length(unique(dfFreqTermoMedoid$'id')) / length(unique(cl$'document'))) # Percentual perdido
length(unique(dfFreqTermoMedoid$'word')) # número de features

## -- Juntando target/label e medoid ----
if(class(docTarget$"id") == class(cl$"document")){
  dfTargetMedoid <- merge(docTarget, cl, by.x = "id", by.y = "document")
}
# length(which(is.na(dfTargetMedoid$target))) = 4623 #se colocar all.y = TRUE
dfTargetMedoid$"id" <- formatC(x = dfTargetMedoid$"id", digits = 0, format = "f")
rm(dfCompleto); rm(cl); rm(freqMatDTMInfoGain); rm(melhorIter); gc(reset = TRUE)

## 
dfFreqTermoTargetMedoid <- merge(dfTargetMedoid, dfFreqTermoMedoid, by = c("id", "medoid"))
length(unique(dfFreqTermoTargetMedoid$"id")) # 970785
rm(dfTargetMedoid); rm(dfFreqTermoMedoid); gc(reset = TRUE)

# 1000000 - 970785 = 29215

## -- Dividindo a base por Grupos/Clusters ----
# listaGruposDados <- split(x = dfFreqTermoTargetMedoid, 
#                           f = dfFreqTermoTargetMedoid$"medoid")
# rm(dfFreqTermoTargetMedoid); gc(reset = TRUE)

## -- Matrizes DTM ----
grupo <- 1
for(grupo in seq(numCluster)[-c(1)]){
  ## -- matriz ----
  dfDTM <- dfFreqTermoTargetMedoid %>%
    filter(medoid == grupo)
  dfDTM$"medoid" <- NULL
  
  
  ## -- Labels ----
  labelsTr <- dfDTM %>%
    select(id, target)
  dfDTM$"target" <- NULL
  labelsTr <- unique(labelsTr)
  dfDTM <- data.table(dfDTM)
  
  ## -- Pivotagem ----
  tCom <- Sys.time(); tCom
  matDTMCast <- dcast.data.table(dfDTM,
                                 id ~ word, fun = sum,
                                 value.var = "n")
  tFim <- Sys.time(); tFim
  tFim - tCom
  
  matDTMCast <- as.data.frame(matDTMCast)
  
  ## -- Salvando ----
  grupoCh <- ifelse(nchar(grupo) == 1, paste0("0", grupo), as.character(grupo))
  nomeArqMatDTMSalvar <- paste0("matDTMCast", grupoCh, ".RData", collapse = "")
  save(matDTMCast, file = file.path(camSalvar, nomeArqMatDTMSalvar))
  
  nomeArqLabelsTrSalvar <- paste0("labelsTr", grupoCh, ".RData", collapse = "")
  save(labelsTr, file = file.path(camSalvar, nomeArqLabelsTrSalvar))
  
  rm(matDTMCast); rm(dfDTM); rm(labelsTr); gc()
}

