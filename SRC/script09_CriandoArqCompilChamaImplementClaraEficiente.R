rm(list = ls()); gc(reset = TRUE)


#cam <- "/home/luis/06_AplicacaoSentimento/01_Bases"
camSalvar <- "/media/luis/5B8F-C6B3/06_AplicacaoSentimento/00_Scripts"


#load(file = file.path(cam, "baseTr.RData"))
#numTotInstancesTr <- length(baseTr)


idsTr <- names(baseTr)
#numInst <- length(unique(names(baseTr)))

pTamAm <- 20000

numReplicacoes <- 50#numTotInstancesTr/pTamAm

pIter <- seq(numReplicacoes)

arqCompil <- c("#!/bin/bash",
               "",
               paste0("R CMD BATCH \'--args iteracao=",
                      pIter,
                      " numClusters=7 tamAm=",
                      pTamAm,
                      "\' script11_ClusterFinal.R")
                      #"\' scriptImplementClaraEficiente.R")
)

#nomeArq <- paste0("chamandoImplementClaraEficiente", pTamAm, ".sh")
nomeArq <- paste0("chamandoClusterFinal", pTamAm, ".sh")
write.table(x = arqCompil, file = file.path(camSalvar, nomeArq), quote = F, row.names = F, col.names = F)
