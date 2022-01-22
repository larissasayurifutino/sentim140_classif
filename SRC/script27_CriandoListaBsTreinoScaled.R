rm(list = ls()); gc()

## -- Arquivos ----

#cam <- "/media/luis/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/matDTM"
cam <- "/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/matDTM"

arqs <- list.files(cam)

arqsMatDTMTr <- arqs[stringr::str_detect(string = arqs, pattern = "matDTMCast")]
arqsMatDTMTr <- arqsMatDTMTr[!stringr::str_detect(string = arqsMatDTMTr, pattern = "Tst")]
arqsMatDTMTst <- arqs[stringr::str_detect(string = arqs, pattern = "matDTMCastTst")]
arqsLabelsTr <- arqs[stringr::str_detect(string = arqs, pattern = "labelsTr")]
arqsLabelsTst <- arqs[stringr::str_detect(string = arqs, pattern = "labelsTst")]
rm(arqs)


CLUSTER <- 1
for(CLUSTER in seq(length(arqsMatDTMTr))[-1]){
  ## -- Carregando objeto Treino ----
  load(file = file.path(cam , arqsMatDTMTr[CLUSTER])) # matDTMCast
  
  
  ## -- Padronizando variaveis ----
  #tCom <- Sys.time(); tCom
  bsTreino <- apply(X = matDTMCast, MARGIN = 2, FUN = function(x) as.numeric(x))
  rm(matDTMCast); gc(reset = TRUE)
  bsTreinoScaled <- scale(bsTreino, center = TRUE, scale = TRUE)
  #tFim <- Sys.time(); tFim
  
  vetMedias <- apply(bsTreino, 2, mean)
  vetDesvios <- apply(bsTreino, 2, sd)
  
  rm(bsTreino); gc(reset = TRUE)
  
  listaTr <- vector(mode = "list", length = 3)
  names(listaTr) <- c("bsTreinoScaled", "vetMedias", "vetDesvios")
  listaTr$"bsTreinoScaled" <- bsTreinoScaled
  listaTr$"vetMedias" <- vetMedias
  listaTr$"vetDesvios" <- vetDesvios
  
  CLUSTERCH <- ifelse(nchar(CLUSTER)!=2, paste0("0", CLUSTER), as.character(CLUSTER))
  save(listaTr, file = file.path(cam, paste0("listaTr", CLUSTERCH, ".RData")))
rm(bsTreinoScaled); rm(vetMedias); rm(vetDesvios); rm(listaTr); gc(reset = TRUE)
}
