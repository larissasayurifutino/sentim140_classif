rm(list = ls()); gc()

## -- Arquivos ----

#cam <- "/media/luis/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/matDTM"
cam <- "/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/matDTM"

arqs <- list.files(cam)

#arqsMatDTMTr <- arqs[stringr::str_detect(string = arqs, pattern = "matDTMCast")]
#arqsMatDTMTr <- arqsMatDTMTr[!stringr::str_detect(string = arqsMatDTMTr, pattern = "Tst")]
arqsMatDTMTst <- arqs[stringr::str_detect(string = arqs, pattern = "matDTMCastTst")]
#arqsLabelsTr <- arqs[stringr::str_detect(string = arqs, pattern = "labelsTr")]
#arqsLabelsTst <- arqs[stringr::str_detect(string = arqs, pattern = "labelsTst")]
rm(arqs)


CLUSTER <- 4
for(CLUSTER in seq(length(arqsMatDTMTst))[-1]){
  ## -- Carregando objeto Treino ----
  load(file = file.path(cam , arqsMatDTMTst[CLUSTER])) # matDTMCastTst
  
  
  ## -- Padronizando variaveis ----
  #tCom <- Sys.time(); tCom
  bsTeste <- apply(X = matDTMCastTst, MARGIN = 2, FUN = function(x) as.numeric(x))
  rm(matDTMCastTst); gc(reset = TRUE)
  #tFim <- Sys.time(); tFim
  
  listaTst <- vector(mode = "list", length = 3)
  names(listaTst) <- c("bsTeste")
  listaTst$"bsTeste" <- bsTeste
  
  CLUSTERCH <- ifelse(nchar(CLUSTER)!=2, paste0("0", CLUSTER), as.character(CLUSTER))
  save(listaTst, file = file.path(cam, paste0("listaTst", CLUSTERCH, ".RData")))
  rm(bsTeste); rm(listaTst); gc(reset = TRUE)
}
