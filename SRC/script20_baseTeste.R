rm(list = ls()); gc()

library(tidytext)

## -- Dados ----
#cam <- "/home/luis/06_AplicacaoSentimento/01_Bases"
cam <- "/media/luis/5B8F-C6B3/06_AplicacaoSentimento/01_Bases"
camSalvar <- "/home/luis/06_AplicacaoSentimento/01_Bases/matDTM"


# arqsAnot
load("/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/listaSeqArqsAnotados.RData")
seqAnot <- do.call(rbind, lista)
document <- lapply(X = lista, FUN = function(x) names(x))
document <- do.call(rbind, document)

dfSeqAnot <- data.frame(document = document[, 1],
                        seqAnot = seqAnot[, 1])
rm(document); rm(seqAnot); rm(lista); gc()

# dados de Treino
load(file = file.path(cam, "baseTr.RData"))
refBsTr <- unique(names(baseTr))
rm(baseTr); gc(reset = TRUE)

## Sequencia de Anotacoes: Base de Teste
testeLogico <- dfSeqAnot$"document" %in% refBsTr
dfSeqAnotTst <- dfSeqAnot[!testeLogico,]

save(dfSeqAnotTst, file = file.path(cam, "baseTst.RData"))
rm(dfSeqAnotTst); rm(dfSeqAnot); gc()

# todos os documentos em formato DTM
load("/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/matDTMCast.RData")

## desempilhar todos os dados
dadosEmpil <- tidytext::tidy(matDTMCast)
rm(matDTMCast); gc()

## Base de Teste
testeLogico <- dadosEmpil$"document" %in% refBsTr
rm(refBsTr); gc(reset = TRUE)
dfTst <- dadosEmpil[!testeLogico,]

save(dfTst, file = file.path(cam, "dfTst.RData"))

## Labels
load(file.path(cam, "arqClassifRedSemDuplic2.RData"))

## -- Id/document e label ----
docTarget <- df[, c("document", "target")]
docTarget <- unique(docTarget)
rm(df); gc(reset = TRUE)
docTarget$"document" <- formatC(x = as.numeric(docTarget$"document"), digits = 0, format = "f")
