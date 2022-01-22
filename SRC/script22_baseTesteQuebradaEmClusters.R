rm(list = ls()); gc(reset = TRUE)

## -- Pacotes ----
library(stringdist)
library(data.table)

## -- Dados ----
cam <- "/media/luis/5B8F-C6B3/06_AplicacaoSentimento/01_Bases"
camMelhorIter <- file.path(cam, "resultClaraAdap", "descritiva")
camAtribInst <- file.path(cam, "resultClaraAdap", "atribInstancias")
camMedoids <- file.path(cam, "resultClaraAdap", "medoids")
#cam <- "/home/leste/06_AplicacaoSentimento/01_Bases"

# Sequencia de Anotacoes
load(file.path(cam, "baseTst.RData")) # dfSeqAnotTst ("document" "seqAnot")
# Termos
load(file.path(cam, "dfTst.RData")) # dfTst ("document" "term"     "count")

# melhorIter
load(file = file.path(camMelhorIter, "melhorIter.RData"))

arqsAtribInst <- list.files(camAtribInst)
arqsAtribInstLoad <- arqsAtribInst[stringr::str_detect(string = arqsAtribInst, pattern = as.character(melhorIter$"iteracao"))]


# cluster
load(file.path(camAtribInst, arqsAtribInstLoad)) # dfCompleto

arqscamMedoids <- list.files(camMedoids)
arqMedoidsLoad <- arqscamMedoids[stringr::str_detect(string = arqscamMedoids, pattern = as.character(melhorIter$"iteracao"))]


# Medoids
load(file.path(camMedoids, arqMedoidsLoad)) # dfMedoids (document cluster seq)



numClusters <- nrow(dfMedoids)

## -- Passando objeto para character ----
dfSeqAnotTstAsChar <- as.character(dfSeqAnotTst$"seqAnot")
names(dfSeqAnotTstAsChar) <- dfSeqAnotTst$"document"
rm(dfSeqAnotTst); gc(reset = TRUE)

medoidsAsChar <- as.character(dfMedoids$"seq")
names(medoidsAsChar) <- dfMedoids$"cluster"

## -- Distancia das Instancias aos Medoids ----
tComDissUnseen <- Sys.time()
matDissimilUnseenData <- stringdist::stringdistmatrix(a = dfSeqAnotTstAsChar,
                                                      b = medoidsAsChar,
                                                      method = "lv",
                                                      weight = c(d = 1, i = 1, s = 1, t = 1),
                                                      useNames = "names",
                                                      nthread = 3)
tFimDissUnseen <- Sys.time()
gc(reset = TRUE)

## -- Qual o medoide de menor distancia por instancia de unseenData ----
matDissimilUnseenDataDT <- data.table(matDissimilUnseenData, keep.rownames = T)  

#tComDistMedMaisProx <- Sys.time()
matDissimilUnseenDataDT[, "medoid" := apply(matDissimilUnseenDataDT[,-1], 1, which.min)]
#matDissimilUnseenDataDT[, "distancia" := apply(matDissimilUnseenDataDT[,-c("rn", "medoid")], 1, min)]

bsTstPorCluster <- data.frame(document = matDissimilUnseenDataDT$"rn",
                              medoid = matDissimilUnseenDataDT$"medoid")#,
                              #distancia = matDissimilUnseenDataDT$distancia)

rm(matDissimilUnseenDataDT); rm(matDissimilUnseenData); gc()



## -- salvando ----
save(bsTstPorCluster, file = file.path(cam, "bsTstPorCluster.RData"))

#table(bsTstPorCluster$medoid)
#1     2     3     4     5     6     7 
#90402 87951 89841 85297 66663 72162 61933 

#prop.table(table(bsTstPorCluster$medoid))
#1         2         3         4         5         6         7 
#0.1631072 0.1586850 0.1620950 0.1538965 0.1202763 0.1301978 0.1117422 
