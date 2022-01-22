rm(list = ls()); gc()

## -- Dados ----
cam <- "/media/luis/5B8F-C6B3/06_AplicacaoSentimento/01_Bases"
camSalvar <- camMatDTM <- "/media/luis/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/matDTM"
# cluster
#load(file.path(cam, "cluster980000DefMelhorClusterInduzido20000.RData"))
# "target" "id"
load(file.path(cam, "arqClassifRedSemDuplic2.RData")) # arqClassifRedSemDuplic2
length(unique(arqClassifRedSemDuplic2$"id"))
# Base de Teste: document e SeqAnot
#load(file.path(cam, "baseTst.RData")) # dfSeqAnotTst
# Base de Teste: "document" "term"     "count"
#load(file.path(cam, "dfTst.RData")) # dfTst
# Base de Teste: "document"  "medoid"    "distancia"
#load(file.path(cam, "bsTstPorCluster.RData")) # bsTstPorCluster
#length(unique(bsTstPorCluster$"document"))

#bsTstPorCluster$"document" <- formatC(x = as.numeric(bsTstPorCluster$"document"), digits = 0, format = "f")
#arqClassifRedSemDuplic2$"id" <- formatC(x = arqClassifRedSemDuplic2$"document", digits = 0, format = "f")

arqsMatDTM <- list.files(camMatDTM)
arqsMatDTMTr <- arqsMatDTM[!stringr::str_detect(string = arqsMatDTM, pattern = "Tst")]
arqsMatDTMTr <- arqsMatDTMTr[!stringr::str_detect(string = arqsMatDTMTr, pattern = "label")]


## -- Vetor de Labels ----
grupo <- 1
grupo <- grupo + 1; grupo
for(grupo in seq(numCluster)[-1]){
  grupoCh <- ifelse(nchar(grupo) == 1, paste0("0", grupo), as.character(grupo))
  
  arqsMatDTMTr <- arqsMatDTMTr[stringr::str_detect(string = arqsMatDTMTr, pattern = "matDTMCast")]
  arqMatDTMTrLoad <- arqsMatDTMTr[stringr::str_detect(string = arqsMatDTMTr, pattern = grupoCh)]
  
  
  load(file = file.path(camMatDTM, arqMatDTMTrLoad))
  idsTr <- data.frame(id = unique(matDTMCast$"id"))
  
  
  ## -- Juntando ----
  posDfBsTr1 <- merge(arqClassifRedSemDuplic2, 
                       idsTr, 
                       by = "id")
  
  posDfBsTr2 <- merge(arqClassifRedSemDuplic2, 
                       idsTr, 
                       by = "id", all.y = TRUE)
  
  aa <- posDfBsTr2[!(posDfBsTr2$id %in% posDfBsTr1$id), ]
  
  if(nrow(aa) != 0) {
    labelsTr <- posDfBsTr2[(posDfBsTr2$id %in% posDfBsTr1$id), ]
    row.names(labelsTr) <- labelsTr$"id"
    names(labelsTr) <- c("document", "target")
    
    matDTMCast <- matDTMCast[!(matDTMCast$"id" %in% aa$"id"),]
    
    if(all.equal(matDTMCast$"id", labelsTr$"document")){
      ## -- Salvando ----
      nomeArqMatDTMSalvar <- paste0("matDTMCastTr", grupoCh, ".RData", collapse = "")
      save(matDTMCast, file = file.path(camSalvar, nomeArqMatDTMSalvar))
      
      nomeArqLabelsSalvar <- paste0("labelTr", grupoCh, ".RData", collapse = "")
      save(labelsTr, file = file.path(camSalvar, nomeArqLabelsSalvar))
      
      rm(matDTMCast); rm(labelsTr); rm(aa); rm(idsTr); rm(posDfBsTr1); rm(posDfBsTr2); gc()      
    }
  }
  
  # ‘sf’, ‘ggthemes’, ‘viridis’
  
}

