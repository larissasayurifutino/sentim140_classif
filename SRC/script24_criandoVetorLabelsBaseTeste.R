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

arqsMatDTMTst <- list.files(camMatDTM)
arqsMatDTMTst <- arqsMatDTMTst[stringr::str_detect(string = arqsMatDTMTst, pattern = "Tst")]



## -- Vetor de Labels ----
grupo <- grupo + 1; grupo
for(grupo in seq(numCluster)[-1]){
  grupoCh <- ifelse(nchar(grupo) == 1, paste0("0", grupo), as.character(grupo))
  arqsMatDTMTst <- arqsMatDTMTst[stringr::str_detect(string = arqsMatDTMTst, pattern = "matDTMCast")]
  arqMatDTMTstLoad <- arqsMatDTMTst[stringr::str_detect(string = arqsMatDTMTst, pattern = grupoCh)]
  
  
  load(file = file.path(camMatDTM, arqMatDTMTstLoad))
  idsTst <- data.frame(id = unique(matDTMCastTst$"id"))
  
  
  ## -- Juntando ----
  posDfBsTst1 <- merge(arqClassifRedSemDuplic2, 
                      idsTst, 
                      by = "id")
  
  posDfBsTst2 <- merge(arqClassifRedSemDuplic2, 
                      idsTst, 
                      by = "id", all.y = TRUE)
  
  aa <- posDfBsTst2[!(posDfBsTst2$id %in% posDfBsTst1$id), ]
  
  if(nrow(aa) != 0) {
    labelsTst <- posDfBsTst2[(posDfBsTst2$id %in% posDfBsTst1$id), ]
    row.names(labelsTst) <- labelsTst$"id"
    names(labelsTst) <- c("document", "target")
    
    matDTMCastTst <- matDTMCastTst[!(matDTMCastTst$"id" %in% aa$"id"),]
    
    if(all.equal(matDTMCastTst$"id", labelsTst$"document")){
      ## -- Salvando ----
      nomeArqMatDTMSalvar <- paste0("matDTMCastTst", grupoCh, ".RData", collapse = "")
      save(matDTMCastTst, file = file.path(camSalvar, nomeArqMatDTMSalvar))
      
      nomeArqLabelsSalvar <- paste0("labelTst", grupoCh, ".RData", collapse = "")
      save(labelsTst, file = file.path(camSalvar, nomeArqLabelsSalvar))
      
      rm(matDTMCastTst); rm(labelsTst); rm(aa); rm(idsTst); rm(posDfBsTst1); rm(posDfBsTst2); gc()      
    }
  }
  
}

