rm(list = ls()); gc()

library(doParallel); library(stringdist)

## -- Funcoes ----
#seqNumInst
#seqNumTotalInst
# t1 <- Sys.time(); t1
# aux <- try(stringdistmatrix(a = trMatDTM[seqNumInst], 
#                             b = trMatDTM[seqNumTotalInst],
#                             method = "lv",
#                             weight = c(d = 1, i = 1, s = 1, t = 1),
#                             useNames = "names"),
#            silent = TRUE)
# t2 <- Sys.time(); t2

passarArgsStringdistmatrix <- function(bsTrMatDTM = trMatDTM, 
                                       seqNumInst = seqNumInst, 
                                       indice){
  aux <- try(stringdistmatrix(a = bsTrMatDTM[indice], 
                              b = bsTrMatDTM[seqNumInst],
                              method = "lv",
                              weight = c(d = 1, i = 1, s = 1, t = 1),
                              useNames = "names"),
             silent = TRUE)
  
  if("try-error" %in% class(aux)){
    message(paste('Erro no indice:', indice))
    aux <- 999999
  }else{
    aux <- aux
  }
  return(aux)
}

## -- Carregando Bases de Dados ----
cam <- "/home/leste/DBSET/Doutorado/BaseDadosTese/06_AplicacaoSentimento/01_Bases"
load(file.path(cam, "listaSeqArqsAnotados.RData"))

tEmpilCom <- Sys.time(); tEmpilCom
dfSeqArqsAnotados <- do.call(what = rbind, lista)
tEmpilFim <- Sys.time(); tEmpilFim

row.names(dfSeqArqsAnotados) <- lapply(X = lista, FUN = function(x) return(names(x)))

## -- Matriz de distâncias ----

NUMCORES <- 3
cl <- makeCluster(NUMCORES)
registerDoParallel(cores = NUMCORES)
getDoParWorkers()
#plan(multiprocess)

ids <- row.names(dfSeqArqsAnotados)
numInst <- length(unique(ids))
tamAm <- 100000#0 #floor(0.8 * numInst)

## -- Amostra Treino/Teste ----
set.seed(10)
amostra <- sample(x = ids, size = tamAm)

testeLogico <- which(ids %in% amostra)
trMatDTM <- dfSeqArqsAnotados[testeLogico,]

rm(lista); rm(amostra); rm(testeLogico); rm(dfSeqArqsAnotados); rm(ids); gc()

## 

# 100 ---> Time difference of 0.1490564 secs
# 1000 ---> Time difference of 6.983595 secs
# 10000 ---> Time difference of 10.56993 mins

for(parte in 2:3){
  t1 <- Sys.time(); t1
  if(parte == 1){numInstanciasCom <- 1; numInstanciasFim <- 10000}
  if(parte == 2){numInstanciasCom <- 10001; numInstanciasFim <- 20000}
  if(parte == 3){numInstanciasCom <- 20001; numInstanciasFim <- 30000}
  if(parte == 4){numInstanciasCom <- 30001; numInstanciasFim <- 40000}
  if(parte == 5){numInstanciasCom <- 40001; numInstanciasFim <- 50000}
  if(parte == 6){numInstanciasCom <- 50001; numInstanciasFim <- 60000}
  if(parte == 7){numInstanciasCom <- 60001; numInstanciasFim <- 70000}
  if(parte == 8){numInstanciasCom <- 70001; numInstanciasFim <- 80000}
  if(parte == 9){numInstanciasCom <- 80001; numInstanciasFim <- 90000}
  if(parte == 10){numInstanciasCom <- 90001; numInstanciasFim <- 100000}
  
  seqNumInst <- numInstanciasCom:numInstanciasFim
  
  numTotalInstancias <- 100000
  seqNumTotalInst <- seq(numTotalInstancias)

  listaMatDist <- mclapply(X = seqNumInst, 
                           FUN = passarArgsStringdistmatrix, 
                           bsTrMatDTM = trMatDTM, 
                           seqNumInst = seqNumTotalInst,
                           mc.cores = NUMCORES)
  t2 <- Sys.time(); t2
  t2 - t1
  
  
  camSalvarListas <- "/home/leste/DBSET/Doutorado/BaseDadosTese/06_AplicacaoSentimento/01_Bases/DistLev/Listas"
  parteCh <- ifelse(nchar(parte) == 1, paste0("0", parte), as.character(parte))
  nomeArq <- paste0("listaMatDist_", parteCh, ".RData", collapse = "")
  save(listaMatDist, file = file.path(camSalvarListas, nomeArq))
}



t3 <- Sys.time(); t3
dfMatDistLev <- do.call(rbind, listaMatDist)
dfMatDistLevAsDist <- as.dist(dfMatDistLev)
t4 <- Sys.time(); t4
t4 - t3
#Error: Failed to retrieve the result of MulticoreFuture (<none>) from the 
#forked worker (on localhost; PID 5828). Post-mortem diagnostic: Failed to 
#determined whether a process with this PID exists or not, i.e. cannot infer
#whether the forked localhost worker is alive or not.
camSalvarListas <- "/home/leste/DBSET/Doutorado/BaseDadosTese/06_AplicacaoSentimento/01_Bases/DistLev/matAsDist"
save(dfMatDistLevAsDist, file = file.path(camSalvar, "dfMatDistLevAsDist10000.RData"))
