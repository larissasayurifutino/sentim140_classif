rm(list = ls()); gc(reset = TRUE)

library(stringr)
library(ggplot2)

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

## Dissimilaridade media por cluster
dfDistMediaPorCl <- dfDistMediaPorCl[order(dfDistMediaPorCl$do.call.rbind..listaDistMediaPorCl.), ]

## Grafico de Barras

dfDescCl <- merge(dfDistribObsPorCl, percMaxInstPorCl, by.x = "id", by.y = "cl")
dfDescCl <- dfDescCl[order(dfDescCl$percMax, decreasing = FALSE),]

save(dfDescCl, file = "/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/dfDescCl.RData")


dfDescCl[which(dfDescCl$id %in% c("27", "03", "07", "16", "13", "37")),]

dfGrafBarras <- dfDistribObsPorCl[which(dfDistribObsPorCl$id %in% c("27", "03", "07", "16", "13", "37")),]

dfGrafBarras <- reshape2::melt(data = dfGrafBarras)

dfGrafBarras$"variable" <- str_replace_all(string = dfGrafBarras$"variable", pattern = "X", replacement = "0")

custom.col <- c("#FFDB6D", "#C4961A", 
                "#D16103", "#C3D7A4", "#52854C",
                "#4E84C4", "#293352")

dfGrafBarras$"idFactor" = factor(dfGrafBarras$"id", 
                                 levels = c("27", "03", "07", "16", "13", "37"))

gBarplot <- ggplot(dfGrafBarras, aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat="identity") + xlab("Cluster") + ylab("Relative Frequency") +
  facet_wrap(~ idFactor, ncol = 2) +
  scale_fill_manual( values = custom.col) +
  guides(fill = FALSE) 

## 10.000: Iteracao 95
## 20.000: Iteracao 3
