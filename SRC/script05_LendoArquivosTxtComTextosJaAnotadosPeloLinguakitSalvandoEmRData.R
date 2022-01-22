## -- Esse script tem o objetivo de ler os arquivos com os textos dos 26.503 tweets 
## anotados pelo Linguakit e juntar em um único arquivo RData -- ##
## -- No proximo script, eu vou excluir os tweets considerados duplicados 
## com base em um arquivo com esses ids.
rm(list = ls()); gc()
library(stringr)
library(future)
library(future.apply)
library(doParallel)
options(scipen = 999)
#caminhoSalvar <- "~/Dropbox/Doutorado/BaseDadosTese/RData/bases26503Tweets/Dados/"
#load(paste0(caminhoSalvar, "df26503ArqsComClassifETextoLimpo.RData"))

## ------------- ##
## -- Funções -- ##
## ------------- ##
#arquivo <- file.path(cam, todosOsArqsAnotados[2])

# funcaoLeArquivoAnotado <- function(arquivo, 
#                                    caractComID = nchar("/home/larissa/ArqsLinguakitSentiment140/02_JaAnotados/jaAnotArqID")){
#   tryCatch({
#     aux <- readLines(arquivo)
#     
#     
#     testesColuna <- str_match_all(aux, "[[:blank:]]{2}")
#     numEspacosDuplosPorLinha <- unlist(lapply(X = testesColuna, FUN = function(x) length(x)))
#     numTotalLinhas <- length(numEspacosDuplosPorLinha)
#     numTotalLinhas <- numTotalLinhas - 1
#     
#     #temLinhaComMenosTresCol <- sum(numEspacosDuplosPorLinha)
#     linha1 <- read.table(arquivo, header = FALSE, 
#                          sep = "", stringsAsFactors = FALSE,
#                          quote = "&&", 
#                          nrows = 1)  
#     if(length(linha1) == 3){
#       arqLido <- linha1
#     }else{
#       controle <<- controle + 1 
#       linha1$V3 <- linha1$V2
#       linha1$V2 <- ""    
#       arqLido <- linha1
#     }
#     
#     for(pLinha in 2:numTotalLinhas){
#       pLinha
#       linhaLida <- read.table(arquivo, header = FALSE, 
#                               sep = "", stringsAsFactors = FALSE,
#                               quote = "&&", 
#                               skip = pLinha-1, nrows = 1)  
#       if(length(linhaLida) == 3){
#         arqLido <- rbind(arqLido, linhaLida)
#       }else{
#         linhaLida$V3 <- linhaLida$V2
#         linhaLida$V2 <- ""    
#         arqLido <- rbind(arqLido, linhaLida)
#       }
#     }
#     
#     
#     arqFinal <- data.frame(id = substr(arquivo, caractComID + 1, nchar(arquivo)-4),
#                            termo = arqLido$V1,
#                            lema = arqLido$V2,
#                            codAnot = arqLido$V3)
#     
#     return(arqFinal)
#     
#   }, error = function(e) {
#     message('Ocorreu algum erro')
#     message("Mensagem de erro original:")
#     message(e)
#     return(-1)
#   },
#   finally = {
#     message("\n finalizado")
#   })
# }  



funcaoLeArquivoAnotado <- function(arquivo, 
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
cam <- "/home/larissa/ArqsLinguakitSentiment140/02_JaAnotados"
todosOsArqsAnotados <- system("ls /home/larissa/ArqsLinguakitSentiment140/02_JaAnotados", intern = TRUE)


cl <- makeCluster(2)
registerDoParallel(cores = 2)
plan(multiprocess) ## Run in parallel on local computer

#com = 1
#fim = length(todosOsArqsAnotados)#/3

#aux <- c("jaAnotArqID1467810369.txt", "jaAnotArqID1564431842.txt", "jaAnotArqID1467810672.txt")

tCom <- Sys.time(); tCom
lista <- future_lapply(X = file.path(cam, todosOsArqsAnotados), FUN = funcaoLeArquivoAnotado)
tFim <- Sys.time(); tFim

caminhoSalvarRData <- "/home/larissa/DBSET/Doutorado/BaseDadosTese/06_AplicacaoSentimento/01_Bases/LendoCodsAnotados"

save(lista,
     file = paste(caminhoSalvarRData, "listaSeqArqsAnotados.RData", sep = "/"))

# > tCom <- Sys.time(); tCom
# [1] "2019-04-28 23:09:31 BST"
# > lista <- future_lapply(X = file.path(cam, todosOsArqsAnotados), FUN = funcaoLeArquivoAnotado)
# Erro no id: 1564431842
# Erro no id: 1675792908
# Erro no id: 1675792945
# Erro no id: 1675793081
# Erro no id: 1675793166
# Erro no id: 1675793185
# Erro no id: 1675793211
# Erro no id: 1675793253
# Erro no id: 1675793284
# Erro no id: 1675793285
# Erro no id: 1675793322
# Erro no id: 1675793402
# Erro no id: 1675793429
# Erro no id: 1675793484
# Erro no id: 1675793619
# Erro no id: 1675793636
# Erro no id: 1694857326
# Erro no id: 1977747730
# Erro no id: 2049614334
# Erro no id: 2183251565
# Erro no id: 2183251680
# Erro no id: 2183252750
# Erro no id: 2183252784
# Erro no id: 2183253266
# Erro no id: 2183253413
# Erro no id: 2183253640
# Erro no id: 2183266053
# Erro no id: 2183266171
# Erro no id: 2183266414
# Erro no id: 2183266523
# Erro no id: 2183266658
# Erro no id: 2183267081
# Erro no id: 2183267661
# Erro no id: 2183267664
# Erro no id: 2183267738
# Erro no id: 2183267850
# Erro no id: 2183267872
# Erro no id: 2183267880
# Erro no id: 2183268264
# Erro no id: 2183268596
# Erro no id: 2183268792
# Erro no id: 2183268803
# Erro no id: 2183268909
# Erro no id: 2183269297
# Erro no id: 2183269464
# Erro no id: 2183269639
# Erro no id: 2183269753
# Erro no id: 2183269758
# Erro no id: 2183269793
# Erro no id: 2183269817
# Erro no id: 2183270040
# Erro no id: 2183270352
# Erro no id: 2183270509
# Erro no id: 2183270720
# Erro no id: 2183270997
# Erro no id: 2183271176
# Erro no id: 2183271289
# Erro no id: 2183271706
# Erro no id: 2191354092
# Erro no id: 2298753577
# > tFim <- Sys.time(); tFim
# [1] "2019-04-29 00:42:36 BST"