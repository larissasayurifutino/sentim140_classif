## -- Esse script tem o objetivo de criar arquivos .txt com os textos dos 26.503 tweets 
## para que possam ser "lidos" pelo Linguakit -- ##

rm(list = ls()); gc()

## -- Carregando Arquivos com textos da dissertacao sem duplicacoes -- ##
dirBase <- "/home/larissa/DBSET/Doutorado/BaseDadosTese/06_AplicacaoSentimento/01_Bases"
sentiment140 <- readRDS(file.path(dirBase, "sentiment140.rds"))

## -- Onde ficam os arquivos que serão anotados -- ##
enderecoParaAnotar <- "~/ArqsLinguakitSentiment140/01_ParaAnotar/"
arqsParaAnotar <- list.files(enderecoParaAnotar)
idArqsParaAnotar <- substr(x = arqsParaAnotar, 
                           start = nchar("paraAnotArqID") + 1,
                           stop =  nchar(arqsParaAnotar) - nchar(".txt"))

notacaoCientifica <- stringr::str_detect(string = idArqsParaAnotar, pattern = "e")

enderecoAnotados <- "~/ArqsLinguakitSentiment140/02_JaAnotados"
arqsAnotados <- list.files(enderecoAnotados)
idArqsAnotados <- substr(x = arqsAnotados, 
                           start = nchar("jaAnotArqID") + 1,
                           stop =  nchar(arqsAnotados) - nchar(".txt"))

# Quais IDs ainda faltam anotar?
arqsJahAnotados <- which(idArqsParaAnotar %in% idArqsAnotados)
arqsNaoAnotados <- idArqsParaAnotar[-arqsJahAnotados]


tamPartes <- length(arqsNaoAnotados)/3
tamPartes <- floor(tamPartes)

arqsNaoAnotados1 <- arqsNaoAnotados[seq(tamPartes)]
arqsNaoAnotados2 <- arqsNaoAnotados[(tamPartes + 1): (2*tamPartes)]
arqsNaoAnotados3 <- arqsNaoAnotados[((2*tamPartes) + 1): (3*tamPartes)]

setwd(enderecoParaAnotar)

## -- Gerando UM arquivo para descrevendo todos os documentos que devem ser anotados -- ##
## -- Esse arquivo "vamos passar" no terminal do Linux -- ##
nomeArqParaAnot1 <- paste0("./linguakit tagger en /home/larissa/ArqsLinguakitSentiment140/01_ParaAnotar/paraAnotArqID", 
                          arqsNaoAnotados1, ".txt ")
nomeArqJaAnot1 <- paste0("> /home/larissa/ArqsLinguakitSentiment140/02_JaAnotados/jaAnotArqID", 
                        arqsNaoAnotados1, ".txt")

arqparaBatch1 <- paste0(nomeArqParaAnot1, nomeArqJaAnot1)
arqparaBatch1 <- c("#!/bin/sh", "", arqparaBatch1)
arqparaBatch1 <- data.frame(arqparaBatch1)

write.table(x = arqparaBatch1,
            file = "/home/larissa/ArqsLinguakitSentiment140/arqParaBatch1.sh",
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            fileEncoding = "UTF-8")


###

nomeArqParaAnot2 <- paste0("./linguakit tagger en /home/larissa/ArqsLinguakitSentiment140/01_ParaAnotar/paraAnotArqID", 
                           arqsNaoAnotados2, ".txt ")
nomeArqJaAnot2 <- paste0("> /home/larissa/ArqsLinguakitSentiment140/02_JaAnotados/jaAnotArqID", 
                         arqsNaoAnotados2, ".txt")

arqparaBatch2 <- paste0(nomeArqParaAnot2, nomeArqJaAnot2)
arqparaBatch2 <- c("#!/bin/sh", "", arqparaBatch2)
arqparaBatch2 <- data.frame(arqparaBatch2)

write.table(x = arqparaBatch2,
            file = "/home/larissa/ArqsLinguakitSentiment140/arqParaBatch2.sh",
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            fileEncoding = "UTF-8")


###

nomeArqParaAnot3 <- paste0("./linguakit tagger en /home/larissa/ArqsLinguakitSentiment140/01_ParaAnotar/paraAnotArqID", 
                           arqsNaoAnotados3, ".txt ")
nomeArqJaAnot3 <- paste0("> /home/larissa/ArqsLinguakitSentiment140/02_JaAnotados/jaAnotArqID", 
                         arqsNaoAnotados3, ".txt")

arqparaBatch3 <- paste0(nomeArqParaAnot3, nomeArqJaAnot3)
arqparaBatch3 <- c("#!/bin/sh", "", arqparaBatch3)
arqparaBatch3 <- data.frame(arqparaBatch3)

write.table(x = arqparaBatch3,
            file = "/home/larissa/ArqsLinguakitSentiment140/arqParaBatch3.sh",
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            fileEncoding = "UTF-8")
