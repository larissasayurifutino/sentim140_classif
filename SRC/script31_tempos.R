## -- Funcao ----
saberTemposMinMaxEDif <- function(ID, CAM, ARQS){
  caminho <- file.path(CAM, ARQS)
  tempos <- file.mtime(caminho)
  tMin <- min(tempos)
  tMax <- max(tempos)
  
  dif <- tMax - tMin
  
  return(data.frame(id = ID, tMin = tMin, tMax = tMax, dif = dif))
}


## -- Tempo de Classificacao ----

camResClassif <- "/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/resClassif"

arqsClassif <- list.files(camResClassif)

arqsCl01 <- arqsClassif[stringr::str_detect(string = arqsClassif, pattern = "custo0.1.RData")]
arqsCl1 <- arqsClassif[stringr::str_detect(string = arqsClassif, pattern = "custo1.RData")]
arqsCl10 <- arqsClassif[stringr::str_detect(string = arqsClassif, pattern = "custo10.RData")]


temposClassif <- data.frame(rbind(saberTemposMinMaxEDif(ID = "Cl01", CAM = camResClassif, ARQS = arqsCl01),
                                  saberTemposMinMaxEDif(ID = "Cl1", CAM = camResClassif, ARQS = arqsCl1),
                                  saberTemposMinMaxEDif(ID = "Cl10", CAM = camResClassif, ARQS = arqsCl10)))



save(temposClassif, file = file.path("/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/tempos/temposClassif.RData"))



## -- Matriz DTM ----
camMatDTM <- "/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/matDTM"

arqsMatDTM <- list.files(camMatDTM)

arqsMatDTMTr <- arqsMatDTM[stringr::str_detect(string = arqsMatDTM, pattern = "matDTMCast[0-9]{2}")]
arqsMatDTMTst <- arqsMatDTM[stringr::str_detect(string = arqsMatDTM, pattern = "matDTMCastTst[0-9]{2}")]
arqsLabelsTr <- arqsMatDTM[stringr::str_detect(string = arqsMatDTM, pattern = "labelsTr[0-9]{2}")]
arqsLabelsTst <- arqsMatDTM[stringr::str_detect(string = arqsMatDTM, pattern = "labelsTst[0-9]{2}")]
arqsListaTr <- arqsMatDTM[stringr::str_detect(string = arqsMatDTM, pattern = "listaTr[0-9]{2}")]
arqsListaTst <- arqsMatDTM[stringr::str_detect(string = arqsMatDTM, pattern = "listaTst[0-9]{2}")]


matDTM <- data.frame(cbind(saberTemposMinMaxEDif(ID = "MatDTMTr", CAM = camMatDTM, ARQS = arqsMatDTMTr),
                           saberTemposMinMaxEDif(ID = "MatDTMTst", CAM = camMatDTM, ARQS = arqsMatDTMTst),
                           saberTemposMinMaxEDif(ID = "LabelsTr", CAM = camMatDTM, ARQS = arqsLabelsTr),
                           saberTemposMinMaxEDif(ID = "LabelsTst", CAM = camMatDTM, ARQS = arqsLabelsTst),
                           saberTemposMinMaxEDif(ID = "ListaTr", CAM = camMatDTM, ARQS = arqsListaTr),
                           saberTemposMinMaxEDif(ID = "ListaTst", CAM = camMatDTM, ARQS = arqsListaTst)))

save(matDTM, file = file.path("/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/tempos/matDTM.RData"))



## -- Cluster Clara ----

camClara <- "/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/clara20000"
arqsClara <- list.files(camClara)
clara <- saberTemposMinMaxEDif(ID = "Clara", CAM = camClara, ARQS = arqsClara)
save(clara, file = file.path("/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/tempos/clara.RData"))
#Clara 2019-05-15 11:57:26 2019-05-15 14:39:58 2.708889 hours