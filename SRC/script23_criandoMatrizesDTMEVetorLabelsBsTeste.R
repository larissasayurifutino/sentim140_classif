rm(list = ls()); gc()

## -- Pacotes ----     
library(data.table)
library(dplyr)

## -- Dados ----
cam <- "/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases" #"/home/leste/06_AplicacaoSentimento/01_Bases"
camSalvar <- camMatDTM <- "/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/matDTM" #"/home/leste/06_AplicacaoSentimento/01_Bases/matDTM"

# Cluster por documento
load(file.path(cam, "bsTstPorCluster.RData"))
# features do Info Gain
load(file = file.path(cam, "featuresManter.RData"))
# df com termos por id - base COMPLETA
load(file = file.path(cam, "freqMatDTMAuxPalComFreqMin09.RData"))
# "target" "id"
load(file.path(cam, "arqClassifRedSemDuplic2.RData")) # arqClassifRedSemDuplic2
length(unique(arqClassifRedSemDuplic2$"id"))

numCluster <- length(unique(bsTstPorCluster$"medoid"))

## -- Mantendo apenas as features Info Gain nos Dados empilhados ----
entradasTabelaoManter <- which(freqMatDTMAuxPalComFreqMin$"word" %in% featuresManter$"term")
freqMatDTMInfoGain <- freqMatDTMAuxPalComFreqMin[entradasTabelaoManter,]
freqMatDTMInfoGain$"id" <- as.character(freqMatDTMInfoGain$"id")
#length(unique(freqMatDTMInfoGain$"id"))
#length(unique(freqMatDTMInfoGain$"word"))

rm(freqMatDTMAuxPalComFreqMin); rm(entradasTabelaoManter); rm(featuresManter); gc(reset = TRUE)

## -- Id/document e target/label ----
docTarget <- arqClassifRedSemDuplic2[, c("id", "target")]
docTarget <- unique(docTarget)
rm(arqClassifRedSemDuplic2); gc(reset = TRUE)
docTarget$"id" <- formatC(x = as.numeric(docTarget$"id"), digits = 0, format = "f")


## -- Separando bases por Grupos/Clusters ----
cl <- data.frame(bsTstPorCluster[, c("document", "medoid")])
rm(bsTstPorCluster); gc()
cl$"medoid" <- factor(x = cl$"medoid", levels = seq(numCluster), labels = seq(numCluster))
cl$"document" <- as.character(cl$"document")


## -- Juntando freq por termo e medoid ----
if(class(freqMatDTMInfoGain$"id") == class(cl$"document")){
  dfFreqTermoMedoid <- merge(freqMatDTMInfoGain, cl, by.x = "id", by.y = "document")
}
dfFreqTermoMedoid$"id" <- formatC(x = as.numeric(dfFreqTermoMedoid$"id"), digits = 0, format = "f")

length(unique(cl$'document')) # número IDs originalmente
length(unique(dfFreqTermoMedoid$'id')) # número IDs no fim das contas
length(unique(cl$'document')) - length(unique(dfFreqTermoMedoid$'id')) 
1 - (length(unique(dfFreqTermoMedoid$'id')) / length(unique(cl$'document'))) # Percentual perdido
length(unique(dfFreqTermoMedoid$'word')) # número de features

rm(freqMatDTMInfoGain); gc(reset = TRUE)

## -- Juntando target/label e medoid ----
if(class(docTarget$"id") == class(cl$"document")){
  dfTargetMedoid <- merge(docTarget, cl, by.x = "id", by.y = "document")
}

## -- Juntando freq por termo, target/label e medoid ----
dfFreqTermoTargetMedoid <- merge(dfTargetMedoid, dfFreqTermoMedoid, by = c("id", "medoid"))
length(unique(dfFreqTermoTargetMedoid$"id")) # 538062
rm(dfTargetMedoid); rm(dfFreqTermoMedoid); rm(docTarget); rm(cl); gc(reset = TRUE)

## -- Lista por Cluster/Medoid ----
# listaGruposDados <- split(x = dfFreqTermoMedoid, f = dfFreqTermoMedoid$"medoid")
# rm(dfComplete); gc(reset = TRUE)

# length(unique(listaGruposDados[[1]]$id)) + length(unique(listaGruposDados[[2]]$id)) +
# length(unique(listaGruposDados[[3]]$id)) + length(unique(listaGruposDados[[4]]$id)) +
# length(unique(listaGruposDados[[5]]$id)) + length(unique(listaGruposDados[[6]]$id)) +
# length(unique(listaGruposDados[[7]]$id))

## -- Arquivos Matrizes DTM de Treino ---- 
arqsMatDTMTr <- list.files(camMatDTM)
posMatDTMCast <- stringr::str_detect(string = arqsMatDTMTr, pattern = "matDTMCast")
arqsMatDTMTr <- arqsMatDTMTr[posMatDTMCast]
posMatDTMCast <- stringr::str_detect(string = arqsMatDTMTr, pattern = "matDTMCastTst")
arqsMatDTMTr <- arqsMatDTMTr[!posMatDTMCast]

## -- Matrizes DTM ----
grupo <- 4
for(grupo in 5:7){
  # Base de Teste
  #df <- listaGruposDados[[grupo]] # dfFreqTermoMedoid
  #df$"medoid" <- NULL
  
  ## -- matriz ----
  dfDTM <- dfFreqTermoTargetMedoid %>%
    filter(medoid == grupo)
  dfDTM$"medoid" <- NULL
  
  ## -- Labels ----
  labelsTst <- dfDTM %>%
    select(id, target)
  labelsTst <- unique(labelsTst)
  dfDTM$"target" <- NULL
  
  # Carregando Matriz DTM de Treino
  grupoCh <- ifelse(nchar(grupo) == 1, paste0("0", grupo), as.character(grupo))
  posArqLoad <- stringr::str_detect(string = arqsMatDTMTr, pattern = grupoCh)
  arqLoad <- arqsMatDTMTr[posArqLoad]
  load(file.path(camMatDTM, arqLoad))
  
  featuresTr <- names(matDTMCast)
  obsFake <- data.frame(id = 9999999999,
                        word = featuresTr[-1],
                        n = 0)
  rm(matDTMCast); gc()
  
  # Quais linhas da base de Teste empilhada manter
  tstLogFeatTstKeep <- which(dfDTM$"word" %in% featuresTr)
  dfDTM <- dfDTM[tstLogFeatTstKeep,]
  
  dfFake <- rbind(dfDTM, obsFake)
  dfFake <- data.table(dfFake)
  
  rm(dfDTM); gc(reset = TRUE)
  
  # Pivotagem
  tCom <- Sys.time(); tCom
  matDTMCastTst <- dcast.data.table(dfFake,
                                    id ~ word, fun = sum,
                                    value.var = "n")
  tFim <- Sys.time(); tFim
  tFim - tCom
  
  rm(dfFake); gc(reset = TRUE)
  
  # Tirando obs Fake
  posIdFake <- which(matDTMCastTst$"id" == 9999999999)
  matDTMCastTst <- matDTMCastTst[- posIdFake,]
  
  # Reordenando as colunas para ficar como base de Treino
  setcolorder(x = matDTMCastTst, neworder = featuresTr)
  matDTMCastTst <- as.data.frame(matDTMCastTst)
  
  ## -- Salvando ----
  grupoCh <- ifelse(nchar(grupo) == 1, paste0("0", grupo), as.character(grupo))
  
  # Matriz DTM
  if(isTRUE(all.equal(names(matDTMCastTst), featuresTr))){
    nomeArqSalvar <- paste0("matDTMCastTst", grupoCh, ".RData", collapse = "")
    save(matDTMCastTst, file = file.path(camSalvar, nomeArqSalvar))
    #rm(df); 
    gc()  
  }else{
    print("Deu ruim!")
  }
  
  # Vetor com Labels
  if(isTRUE(all.equal(matDTMCastTst$"id", labelsTst$"id"))){
    rm(matDTMCastTst)
    nomeArqLabelsSalvar <- paste0("labelsTst", grupoCh, ".RData", collapse = "")
    save(labelsTst, file = file.path(camSalvar, nomeArqLabelsSalvar))
    rm(labelsTst); gc()
  }else{
    posmanter <- which(labelsTst$"id" %in% matDTMCastTst$"id")
    labelsTst <- labelsTst[posmanter,]
    nomeArqLabelsSalvar <- paste0("labelsTst", grupoCh, ".RData", collapse = "")
    save(labelsTst, file = file.path(camSalvar, nomeArqLabelsSalvar))
    rm(labelsTst); gc()
  }
  
}

