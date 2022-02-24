rm(list = ls()); gc()

library(FSelectorRcpp)
library(parallel)
library(parallelML)

numCoresUsar <- detectCores() - 1
registerCores(numberCores = numCoresUsar)


camDados <- "/home/leste/DBSET/Doutorado/BaseDadosTese/06_AplicacaoSentimento/01_Bases"
load(file = file.path(camDados, "matDTMCast.RData"))
load(file = file.path(camDados, "fileTargetNoDuplic.RData"))


matDadosEsparsa <- sparseMatrix(i = matDTMCast2$"i",
                                j = matDTMCast2$"j",
                                x = matDTMCast2$"v",
                                dimnames = matDTMCast2$"dimnames")


tCom <- Sys.time(); tCom
weightsInfoGain <- FSelectorRcpp::information_gain(x = matDadosEsparsa, 
                                                   y = arqClassifRedSemDuplic2$"target",  
                                                   type = "infogain", 
                                                   threads = numCoresUsar)
tFim <- Sys.time(); tFim
tFim- tCom
