args = commandArgs(trailingOnly=TRUE)
#numClusters <- 7
#iteracao <- 1
#tamAm <- 20000

#system("R CMD BATCH '--args iteracao=1 numClusters=7 tamAm=20000' scriptImplementClaraEficiente.R")

library(stringdist)
library(cluster)
library(dplyr)
library(doParallel)
library(data.table)

if(length(args)==0){
  print("No arguments supplied.")
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}



## -- Funcoes ----
acharMedoidMenorDist <- function(x){
  medoidMenorDist <- which.min(x)
  distancia <- x[medoidMenorDist]
  
  return(c(medoidMenorDist, distancia))
}


## -- Carregando Bases de Dados ----
cam <- "/home/leste/06_AplicacaoSentimento/01_Bases"
camSalvar <- file.path(cam, "clara")


load(file = file.path(cam, "baseTr.RData"))
numTotInstancesTr <- length(baseTr)


idsTr <- names(baseTr)
numInst <- length(unique(names(baseTr)))

registerDoParallel(cores = numClusters)

#numReplicacoes <- numTotInstancesTr/tamAm

  ## -- Create randomly, from the original dataset, multiple subsets with fixed size (sampsize) ----
  set.seed(1071717171 + iteracao)
  amostra <- sample(x = idsTr, size = tamAm)
  
  testeLogico <- which(idsTr %in% amostra)
  dataCluster <- baseTr[testeLogico]
  unseenData <- baseTr[-testeLogico]
  
  rm(amostra); rm(testeLogico); gc()
  
  
  ## -- Matriz de Dissimilaridades ----
  tComMatDissimil <- Sys.time()
  matDissimil <- stringdist::stringdistmatrix(a = dataCluster,
                                              method = "lv",
                                              weight = c(d = 1, i = 1, s = 1, t = 1),
                                              useNames = "names",
                                              nthread = numClusters)
  tFimMatDissimil <- Sys.time()
  matDissimilAsDist <- as.dist(matDissimil)
  
  rm(matDissimil); gc(reset = TRUE)
  
  ## -- Compute PAM algorithm on each subset ----
  tComPAM <- Sys.time()
  resultPAM <- cluster::pam(x = matDissimilAsDist, 
                            k = numClusters, 
                            diss = TRUE,
                            pamonce = 3) # reduces the runtime by a factor of O(k) by exploiting that points cannot be closest to all current medoids at the same time.
  tFimPAM <- Sys.time()
  rm(matDissimilAsDist); gc(reset = TRUE)
  
  ## -- Choose the corresponding k representative objects (medoids) ----
  indMedoids <- resultPAM$"medoids" # labels of observations 
  indice <- which(names(dataCluster) %in% indMedoids)
  medoids <- dataCluster[indice]
  
  
  ## -- Dissimilaridades ----
  tComDissUnseen <- Sys.time()
  matDissimilUnseenData <- stringdist::stringdistmatrix(a = unseenData,
                                                        b = medoids,
                                                        method = "lv",
                                                        weight = c(d = 1, i = 1, s = 1, t = 1),
                                                        useNames = "names",
                                                        nthread = numClusters)
  tFimDissUnseen <- Sys.time()
  gc(reset = TRUE)
    
  ## -- Qual o medoide de menor distancia por instancia de unseenData ----
  matDissimilUnseenDataDT <- data.table(matDissimilUnseenData, keep.rownames = T)  
  
  tComDistMedMaisProx <- Sys.time()
  matDissimilUnseenDataDT[, "medoid" := apply(matDissimilUnseenDataDT[,-1], 1, which.min)]
  matDissimilUnseenDataDT[, "distancia" := apply(matDissimilUnseenDataDT[,-c("rn", "medoid")], 1, min)]

  distMedoideMaisProx <- data.frame(medoid = matDissimilUnseenDataDT$medoid,
                                    distancia = matDissimilUnseenDataDT$distancia)

  rm(matDissimilUnseenDataDT); rm(matDissimilUnseenData); gc()

  tFimDistMedMaisProx <- Sys.time()  
  
  ## -- Calculate the mean: Goodness of the clustering ----
  meanOfDissimilarities <- distMedoideMaisProx %>%
    group_by(medoid) %>%
    summarise(distMedia = mean(as.numeric(as.character(distancia))))
  
  
  numObsPorCl <- distMedoideMaisProx %>%
    group_by(medoid) %>%
    summarise(numObs = n())
  
  
  ## -- Retorno ----
  listaRetorno <- vector(mode = "list", length = 8)
  names(listaRetorno) <- c("tDfMatDissimil", "tDfPAM", 
                           "tDfDissUnseen", "tDfDistMedMaisProx", 
                           "meanOfDissimilarities", "numObsPorCl",
                           "resultPAM", "distMedoideMaisProx")
  listaRetorno$"tDfMatDissimil" <- data.frame(tComMatDissimil = tComMatDissimil, 
                                              tFimMatDissimil = tFimMatDissimil)
  listaRetorno$"tDfPAM" <- data.frame(tComPAM = tComPAM, 
                                      tFimPAM = tFimPAM)
  listaRetorno$"tDfDissUnseen" <- data.frame(tComDissUnseen = tComDissUnseen, 
                                             tFimDissUnseen = tFimDissUnseen)
  listaRetorno$"tDfDistMedMaisProx" <- data.frame(tComDistMedMaisProx = tComDistMedMaisProx, 
                                                  tFimDistMedMaisProx = tFimDistMedMaisProx)
  listaRetorno$"meanOfDissimilarities" <- meanOfDissimilarities
  listaRetorno$"numObsPorCl" <- numObsPorCl
  listaRetorno$"resultPAM" <- resultPAM
  listaRetorno$"distMedoideMaisProx" <- distMedoideMaisProx  
  
  iterCh <- ifelse(nchar(iteracao) == 1, paste0("0", iteracao), as.character(iteracao))
  nomeArqSalvar <- paste0("Iter", iterCh, ".RData", collapse = "")
  
  diretSalvar <- file.path(camSalvar, nomeArqSalvar)
  save(listaRetorno, file = diretSalvar)
  
  rm(listaRetorno); rm(distMedoideMaisProx);  rm(meanOfDissimilarities); rm(numObsPorCl); 
  gc(reset = TRUE, full = TRUE)
  gc(reset = TRUE, full = TRUE)
  
  print(iteracao)
q("no")
