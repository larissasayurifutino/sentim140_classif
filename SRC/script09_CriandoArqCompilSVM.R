rm(list = ls()); gc(reset = TRUE)

#R CMD BATCH '--args CLUSTER=2 TIPOSVM=5 NUMEROCLUSTERS=7 CUSTO=0.1' script26_SVM.R
camSalvar <- "/home/leste/06_AplicacaoSentimento/00_Scripts"

idsTr <- names(baseTr)

numTotCluster <- 7#numTotInstancesTr/pTamAm
pCluster <- seq(numTotCluster)
pCusto <- 10
arqCompil <- c("#!/bin/bash",
               "",
               paste0("R CMD BATCH \'--args CLUSTER=",
                      pCluster,
                      " TIPOSVM=5 NUMEROCLUSTERS=7 CUSTO=",
                      pCusto,
                      "\' script26_SVM.R")
               #"\' scriptImplementClaraEficiente.R")
)

#nomeArq <- paste0("chamandoImplementClaraEficiente", pTamAm, ".sh")
pCustoCh <- as.character(pCusto)
if(pCusto == 0.1){
  pCustoCh <- "01"
}
nomeArq <- paste0("chamandoSVMCusto", pCustoCh, ".sh")
write.table(x = arqCompil, file = file.path(camSalvar, nomeArq), quote = F, row.names = F, col.names = F)
