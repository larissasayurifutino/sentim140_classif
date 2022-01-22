rm(list = ls()); gc()
numCluster <- 7

## -- Dados ----
cam <- "/media/luis/5B8F-C6B3/06_AplicacaoSentimento/01_Bases" #"/home/leste/06_AplicacaoSentimento/01_Bases"
camSalvar <- camMatDTM <- "/media/luis/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/matDTM" #"/home/leste/06_AplicacaoSentimento/01_Bases/matDTM"
# Cluster por documento
load(file.path(cam, "bsTstPorCluster.RData"))
# features do Info Gain
load(file = file.path(cam, "featuresManter.RData"))
# df com termos por id - base COMPLETA
load(file = file.path(cam, "freqMatDTMAuxPalComFreqMin09.RData"))

## -- Pacotes ----     
library(data.table)

numCluster <- length(unique(bsTstPorCluster$"medoid"))

## -- Separando bases por Grupos/Clusters ----
aux <- data.frame(bsTstPorCluster[, c("document", "medoid")])
rm(bsTstPorCluster); gc()
aux$"medoid" <- factor(x = aux$"medoid", levels = seq(numCluster), labels = seq(numCluster))


## -- Mantendo apenas as features Info Gain nos Dados empilhados ----
entradasTabelaoManter <- which(freqMatDTMAuxPalComFreqMin$"word" %in% featuresManter$"term")
freqMatDTMInfoGain <- freqMatDTMAuxPalComFreqMin[entradasTabelaoManter,]
length(unique(freqMatDTMInfoGain$"id"))
length(unique(freqMatDTMInfoGain$"word"))

rm(freqMatDTMAuxPalComFreqMin); rm(entradasTabelaoManter); gc(reset = TRUE)

## -- Juntando ----
dfComplete <- merge(freqMatDTMInfoGain, aux, by.x = "id", by.y = "document")
dfComplete$"id" <- formatC(x = as.numeric(dfComplete$"id"), digits = 0, format = "f")

rm(freqMatDTMInfoGain); rm(aux); rm(featuresManter); gc(reset = TRUE)


## -- Lista por Cluster/Medoid ----
listaGruposDados <- split(x = dfComplete, f = dfComplete$"medoid")
rm(dfComplete); gc(reset = TRUE)

# length(unique(listaGruposDados[[1]]$id)) + length(unique(listaGruposDados[[2]]$id)) +
# length(unique(listaGruposDados[[3]]$id)) + length(unique(listaGruposDados[[4]]$id)) +
# length(unique(listaGruposDados[[5]]$id)) + length(unique(listaGruposDados[[6]]$id)) +
# length(unique(listaGruposDados[[7]]$id))

## -- Arquivos Matrizes DTM de Treino ---- 
arqsMatDTMTr <- list.files(camMatDTM)
posMatDTMCast <- stringr::str_detect(string = arqsMatDTMTr, pattern = "matDTMCast")
arqsMatDTMTr <- arqsMatDTMTr[posMatDTMCast]

## -- Matrizes DTM ----
grupo <- 1
for(grupo in seq(numCluster)[-1]){
  # Base de Teste
  df <- listaGruposDados[[grupo]] 
  df$"medoid" <- NULL
  
  # Carregando Matriz DTM de Treino
  grupoCh <- ifelse(nchar(grupo) == 1, paste0("0", grupo), as.character(grupo))
  posArqLoad <- stringr::str_detect(string = arqsMatDTMTr, pattern = grupoCh)
  arqLoad <- arqsMatDTMTr[posArqLoad]
  load(file.path(camMatDTM, arqLoad))
  
  featuresTr <- names(matDTMCast)
  obsFake <- data.frame(id = 9999999999,
                        word = featuresTr[-1],
                        n = 0)
  
  # Quais linhas da base de Teste empilhada manter
  tstLogFeatTstKeep <- which(df$"word" %in% featuresTr)
  df <- df[tstLogFeatTstKeep,]
  
  dfFake <- rbind(df, obsFake)
  
  dfFake <- data.table(dfFake)
  
  # Pivotagem
  tCom <- Sys.time(); tCom
  matDTMCastTst <- dcast.data.table(dfFake,
                                    id ~ word, fun = sum,
                                    value.var = "n")
  tFim <- Sys.time(); tFim
  tFim - tCom
  
  posIdFake <- which(matDTMCastTst$"id" == 9999999999)
  matDTMCastTst <- matDTMCastTst[- posIdFake,]
  
  # Reordenando as colunas para ficar como base de Treino
  setcolorder(x = matDTMCastTst, neworder = featuresTr)
  matDTMCastTst <- as.data.frame(matDTMCastTst)
  
  
  if(all.equal(names(matDTMCastTst), featuresTr)){
    ## -- Salvando ----
    grupoCh <- ifelse(nchar(grupo) == 1, paste0("0", grupo), as.character(grupo))
    nomeArqSalvar <- paste0("matDTMCastTst", grupoCh, ".RData", collapse = "")
    save(matDTMCastTst, file = file.path(camSalvar, nomeArqSalvar))
    
    rm(matDTMCastTst); rm(df); gc()  
  }else{
    print("Deu ruim!")
  }
  
  
}

