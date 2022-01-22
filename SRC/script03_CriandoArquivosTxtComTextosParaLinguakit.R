## -- Esse script tem o objetivo de criar arquivos .txt com os textos dos 26.503 tweets 
## para que possam ser "lidos" pelo Linguakit -- ##

rm(list = ls()); gc()

## -- Carregando Arquivos com textos da dissertacao sem duplicacoes -- ##
dirBase <- "/home/larissa/DBSET/Doutorado/BaseDadosTese/06_AplicacaoSentimento/01_Bases"
sentiment140 <- readRDS(file.path(dirBase, "sentiment140.rds"))

## -- Onde ficam os arquivos que serão anotados -- ##
enderecoParaAnotar <- "~/ArqsLinguakitSentiment140/01_ParaAnotar/"
setwd(enderecoParaAnotar)

## ------------------------------------------------------------------------- ##
## -- Gerando arquivos em .txt para cada um dos textos que serão anotados -- ##
## ------------------------------------------------------------------------- ##
qntsTweets <- nrow(sentiment140)

# Notebook
i = 1
tempo <- system.time(
  for(i in 1:qntsTweets){
    nomeArq <- paste0("paraAnotArqID", sentiment140$"id"[i], ".txt")
    texto <- paste(sentiment140$"text"[i])
    write.table(x = texto,
                file = nomeArq,
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE,
                fileEncoding = "UTF-8")
  }
)
tempo
# Essa tarefa demorou 1515.565 segundos

## -- Gerando UM arquivo para descrevendo todos os documentos que devem ser anotados -- ##
## -- Esse arquivo "vamos passar" no terminal do Linux -- ##
nomeArqParaAnot <- paste0("./linguakit tagger en /home/larissa/ArqsLinguakitSentiment140/01_ParaAnotar/paraAnotArqID", 
                          sentiment140$"id", ".txt ")
nomeArqJaAnot <- paste0("> /home/larissa/ArqsLinguakitSentiment140/02_JaAnotados/jaAnotArqID", 
                        sentiment140$"id", ".txt")

arqparaBatch <- paste0(nomeArqParaAnot, nomeArqJaAnot)
arqparaBatch <- c("#!/bin/sh", "", arqparaBatch)
arqparaBatch <- data.frame(arqparaBatch)

write.table(x = arqparaBatch,
            file = "/home/larissa/ArqsLinguakitSentiment140/arqParaBatch.sh",
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            fileEncoding = "UTF-8")
