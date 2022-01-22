rm(list = ls()); gc()
require(LiblineaR)
require(SparseM)
require(tm)

## -- Caminho Dados ----
camDados <- "/home/leste/DBSET/Doutorado/BaseDadosTese/06_AplicacaoSentimento/01_Bases"
load(file = file.path(camDados, "matDTMCast2.RData"))
load(file = file.path(camDados, "arqClassifRedSemDuplic2.RData"))

numInst <- length(unique(arqClassifRedSemDuplic2$id))
tamAm <- 250000 #floor(0.8 * numInst)

## -- Amostra Treino/Teste ----
set.seed(10)
amostra <- sample(x = arqClassifRedSemDuplic2$"id", size = tamAm)


idsMatDTM <- data.frame(id = matDTMCast2$dimnames$Docs)

testeLogico <- which(idsMatDTM$"id" %in% amostra)
trMatDTM <- matDTMCast2[testeLogico,]
tstMatDTM <- matDTMCast2[-testeLogico,]


indClassif <- arqClassifRedSemDuplic2$"id" %in% amostra
trClassif <- arqClassifRedSemDuplic2[indClassif,]
tstClassif <- arqClassifRedSemDuplic2[!indClassif,]


#01_Vocab/01_VectorSpaceModel/01_Bases/4779Termos/02_RandomSplit/01_Balanced/07_DfParametrosLoopCenarios
# idArqDfParam <- str_detect(string = arqDfParam, pattern = nomeArqDfParam)
# arqDfParam <- arqDfParam[idArqDfParam]
# load(file.path(camDfParam, arqDfParam))

# trMatDTM <- apply(X = trMatDTM, MARGIN = 2, FUN = function(x) as.numeric(x))
# trMatDTMScaled <- scale(trMatDTM, center = TRUE, scale = TRUE)

trClassif$"target"[which(trClassif$"target" == 4)] <- 1

varResp <- factor(x = trClassif$"target", levels = c(0, 1), labels = c(0, 1))

trMatDTM <- as.matrix(trMatDTM)

# 1.000.000 ---> Error: cannot allocate vector of size 607.2 Gb
# 750.000 ---> Error: cannot allocate vector of size 455.4 Gb
# 500.000 ---> Error: cannot allocate vector of size 303.6 Gb
# 200.000 ---> Error: cannot allocate vector of size 121.4 Gb
# 100.000 ---> Error: cannot allocate vector of size 60.7 Gb
# 50.000 ---> Error: cannot allocate vector of size 30.4 Gb
# 20.000 ---> Error: cannot allocate vector of size 12.1 Gb
# 15.000 ---> Error: cannot allocate vector of size 9.1 Gb

## -- SVM ----
t1 <- Sys.time(); t1
# Parametro do LiblineaR
TIPOSVM <- 5
CUSTO <- 1
classificationModel <- LiblineaR(data = trMatDTM,
                                 target = varResp,
                                 type = TIPOSVM,
                                 cost = CUSTO,
                                 bias = TRUE,
                                 verbose = FALSE)
t2 <- Sys.time(); t2

t2 - t1

#################