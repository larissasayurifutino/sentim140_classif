args = commandArgs(trailingOnly = TRUE)

# R CMD BATCH '--args iteracao=1 numClusters=7 tamAm=20000 numTotIteracoes=50' script11_ClusterFinal.R

if(length(args)==0){
  print("No arguments supplied.")
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

library(dplyr)
library(data.table)
library(stringdist)
library(cluster)

## -- Funcoes ----
determinarMinimoTratandoEmpates <- function(vetX){
  minimo <- min(vetX)
  clustersMaisProx <- which(vetX == minimo)
  numClMaisProx <- length(clustersMaisProx)
  if(numClMaisProx == 1){
    clusterAtribuir <- clustersMaisProx
  }else{
    clusterAtribuir <- sample(clustersMaisProx, size = 1)
  }
  return(clusterAtribuir)
}

## -- base ----
cam <- "/media/luis/5B8F-C6B3/06_AplicacaoSentimento/01_Bases"
camSalvar <- "/media/luis/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/resultClaraAdap"
load(file = file.path(cam, "baseTr.RData"))

idsTr <- names(baseTr)

# iteracao <- 1
# tamAm <- 20000 #10000
# numClusters <- 7
# numTotIteracoes <- 50

#for(iteracao in seq(numTotIteracoes)){
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
                                              nthread = 3)
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
  tFimPAM - tComPAM
  
  ## -- Choose the corresponding k representative objects (medoids) ----
  indMedoids <- resultPAM$"medoids" # labels of observations 
  indice <- which(names(dataCluster) %in% indMedoids)
  medoids <- dataCluster[indice]
  medoidsDf <- data.frame(document = names(medoids),
                          seq = medoids)
  
  dfAux <- data.frame(cluster = seq(length(indice)),
                      document = indMedoids)
  # Associando documento, cluster e sequencia de Anotacoes
  dfMedoids <- merge(dfAux, medoidsDf, by = "document")
  dfMedoids <- dfMedoids[order(dfMedoids$cluster, decreasing = FALSE),]
  
  # Vai para atribuir as demais observacoes aos clusters definidos
  medoidsOrd <- as.character(dfMedoids$"seq")
  names(medoidsOrd) <- dfMedoids$"cluster"
  
  
  
  cluster20Mil <- data.frame(document = names(resultPAM$"clustering"),
                             medoid = resultPAM$"clustering")
  
  
  ## -- Dissimilaridades ----
  tComDissUnseen <- Sys.time()
  matDissimilUnseenData <- stringdist::stringdistmatrix(a = unseenData,
                                                        b = medoidsOrd,
                                                        method = "lv",
                                                        weight = c(d = 1, i = 1, s = 1, t = 1),
                                                        useNames = "names",
                                                        nthread = numClusters)
  tFimDissUnseen <- Sys.time()
  gc(reset = TRUE)
  
  ##
  
  #xx <- head(matDissimilUnseenData)
  #matDissimilUnseenDataDT <- data.table(xx, keep.rownames = T)  
  
  matDissimilUnseenDataDT <- data.table(matDissimilUnseenData, keep.rownames = T) 
  
  # matDissimilUnseenDataDT <- data.table(data.frame(rbind( c(1467810369, 18, 12, 6, 5, 11, 6, 5),
  #                                              c(1467810672, 1, 1, 6, 5, 11, 6, 5),
  #                                              c(1467810917, 2, 12, 6, 2, 2, 2, 5))))
  # 
  # vetX <- c(18, 12, 6, 5, 11, 6, 5)
  
  
  #matDissimilUnseenDataDT[, "medoid" := apply(matDissimilUnseenDataDT[,-1], 1, fc)]
  
  tComDistMedMaisProx <- Sys.time(); tComDistMedMaisProx
  matDissimilUnseenDataDT[, "medoid" := apply(matDissimilUnseenDataDT[,-1], 1, which.min)]
  #matDissimilUnseenDataDT[, "medoid" := apply(matDissimilUnseenDataDT[,-1], 1, determinarMinimoTratandoEmpates)]
  #matDissimilUnseenDataDT[, "distancia" := apply(matDissimilUnseenDataDT[,-c("rn", "medoid")], 1, min)]
  
  distMedoideMaisProx <- data.frame(document = matDissimilUnseenDataDT$"rn" ,
                                    medoid = matDissimilUnseenDataDT$"medoid")
  tFimDistMedMaisProx <- Sys.time(); tFimDistMedMaisProx
  
  #rm(matDissimilUnseenDataDT); rm(matDissimilUnseenData); gc()
  
  dfCompleto <- data.frame(rbind(cluster20Mil, distMedoideMaisProx))
  
  ## -- Calculate the mean: Goodness of the clustering ----
  descritivaCl <- dfCompleto %>%
    group_by(medoid) %>%
    summarise(numObs = n()) %>% #,
    #distMedia = mean(as.numeric(as.character(distancia)))) %>%
    mutate(numTotObs = sum(numObs),
           perc = numObs/numTotObs,
           iteracao = iteracao)
  
  
  ## -- Salvando ----
  iteracaoCh <- ifelse(nchar(iteracao) != 2, paste0("0", iteracao), as.character(iteracao))
  
  nomeArqSalvar <- paste0(iteracaoCh, ".RData", collapse = "")
  
  save(descritivaCl, file = file.path(camSalvar, "descritiva", nomeArqSalvar))
  save(dfMedoids, file = file.path(camSalvar, "medoids", nomeArqSalvar))
  save(resultPAM, file = file.path(camSalvar, "cluster", nomeArqSalvar))
  save(dfCompleto, file = file.path(camSalvar, "atribInstancias", nomeArqSalvar))
  
#}

