## -- This script reads .txt files with the labeled data by linguakit and it saves in a single RData file
rm(list = ls()); gc()

library(stringr)
library(future)
library(future.apply)
library(doParallel)
options(scipen = 999)


## ------------- ##
## -- Funções -- ##
## ------------- ##


readAlrAnnotated <- function(arquivo, 
                             caractComID = nchar("/home/larissa/ArqsLinguakitSentiment140/02_JaAnotados/jaAnotArqID")){
  id <- substr(arquivo, caractComID + 1, nchar(arquivo) - 4)
  
  t1 <- Sys.time()
  arqLido <- try(read.table(arquivo, header = FALSE, 
                            sep = "", stringsAsFactors = FALSE,
                            quote = "&&"),
                 silent = TRUE) 
  t2 <- Sys.time() 
  
  if("try-error" %in% class(arqLido)){
    message(paste('Erro no id:', id))
    #message("Mensagem de erro original:")
    #message(e)
    
    arqFinal <- 999999
    names(arqFinal) <- id
  }else{
    arqFinal <- paste0(arqLido$V3[-length(arqLido$V3)], collapse = "")
    names(arqFinal) <- id
  }
  
  return(arqFinal)
  
}  
## ------------------------------ ##
## -- Quais arquivos vamos ler -- ##
## ------------------------------ ##
cam <- "/home/larissa/Documents/repos/sentim140_classif/DATA/FilesLinguakitSentiment140/02_AlreadyAnnot/"
todosOsArqsAnotados <- system("ls /home/larissa/Documents/repos/sentim140_classif/DATA/FilesLinguakitSentiment140/02_AlreadyAnnot/", intern = TRUE)


cl <- makeCluster(2)
registerDoParallel(cores = 2)
plan(multiprocess) ## Run in parallel on local computer



tCom <- Sys.time(); tCom
lista <- future_lapply(X = file.path(cam, todosOsArqsAnotados), FUN = readAlrAnnotated)
tFim <- Sys.time(); tFim

caminhoSalvarRData <- "/home/larissa/DBSET/Doutorado/BaseDadosTese/06_AplicacaoSentimento/01_Bases/LendoCodsAnotados"

save(lista,
     file = paste(caminhoSalvarRData, "listaSeqArqsAnotados.RData", sep = "/"))