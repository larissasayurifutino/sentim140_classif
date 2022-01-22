rm(list = ls()); gc(reset = TRUE)

library(stringr)

cam <- "/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/clara20000"
arqs <- list.files(cam)

numArqs <- length(arqs)
listaDistribObsPorCl <- listaDistMediaPorCl <- vector(mode = "list", length = numArqs)

i <- 1
for(i in seq(numArqs)){
  arq <- arqs[i]

  load(file.path(cam, arq))
  
  numObs <- listaRetorno$"numObsPorCl"$numObs
  numTotObs <- sum(numObs)
  distribObsPorCl <- numObs/numTotObs
  distMediaPorCl <- mean(listaRetorno$"meanOfDissimilarities"$distMedia)
  
  
  listaDistribObsPorCl[[i]] <- distribObsPorCl
  listaDistMediaPorCl [[i]]<- distMediaPorCl
}

iterAux <- str_extract_all(string = arqs, pattern = "[0-9]")
iteracao <- lapply(X = iterAux, FUN = function(x) paste0(x, collapse = ""))
iteracao <- unlist(iteracao)

dfDistribObsPorCl <- data.frame(do.call(rbind, listaDistribObsPorCl))
dfDistribObsPorCl$"id" <- iteracao
dfDistMediaPorCl <- data.frame(do.call(rbind, listaDistMediaPorCl))
dfDistMediaPorCl$"id" <- iteracao

## Qual o cenario que ficou com a distribuição mais homogenea de instancias por cluster
percMaxInstPorCl <- apply(X = dfDistribObsPorCl[, -ncol(dfDistribObsPorCl)], MARGIN = 1, FUN = max)
percMaxInstPorCl <- data.frame(cl = dfDistribObsPorCl[, ncol(dfDistribObsPorCl)],
                               percMax = percMaxInstPorCl)

percMaxInstPorCl <- percMaxInstPorCl[order(percMaxInstPorCl$"percMax", decreasing = FALSE),]

## Dissimilaridade media por cluster
dfDistMediaPorCl <- dfDistMediaPorCl[order(dfDistMediaPorCl$"do.call.rbind..listaDistMediaPorCl."), ]


## 10.000: Iteracao 95
## 20.000: Iteracao 3
