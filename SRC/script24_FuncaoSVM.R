## Esse script tem as funcoes necessarias para fazer a analise SVM.

#library(stringr); library(e1071); library(foreach); library(iterators); library(parallel); library(doParallel)

## -- Funcoes ----

## -- funcaoRetornaCenarios -- ##
funcaoRetornaCenarios <- function(pNumero, vNumero, vCusto){
  cenarios <- expand.grid(base = vNumero, custo = vCusto)
  cenarios <- data.frame(cenarios)
  cenarios$"numGrupos" <- pNumero
  
  ## -- Quantos sao os cenarios (qnts SVM) a serem feitos -- ##
  ordem <- order(cenarios$"numGrupos", cenarios$"base")
  cenComBase <- cenarios[ordem,]
  cenComBase <- cenComBase[, c("numGrupos", "base", "custo")]
  cenComBase <- data.frame(cenComBase)
  cenComBase$"base" <- as.character(cenComBase$"base")
  return(cenComBase)
}


## -------------------------------------- ##
## -- funcaoParalKMeansMatrizDTMESalva -- ##
## -------------------------------------- ##
# Com LiblineaR

funcaoParalKMeansMatrizDTMESalva <- function(bsTrSplit, bsTstSplit,
                                             PARTE, CLUSTER, NUMEROCLUSTERS, CUSTO, CAMSALVAR, CAMSALVARITER,
                                             NOMEVARID = NOMEVARID,
                                             NOMEVARCLUSTER = NOMEVARCLUSTER,
                                             NOMEVARCLASSIF = NOMEVARCLASSIF,
                                             ITERACAO,
                                             TECNICA){
  
  TEMPOCOMITER <- Sys.time()
  
  bsDaIt <- CLUSTER
  ## -- Matrizes ----
  # Selecionar na Base de Treino apenas aquelas linhas em que o cluster predito é o mesmo do cenario da vez
  entBsTreino <- which(names(bsTrSplit) == bsDaIt)
  bsTreino <- bsTrSplit[[ entBsTreino ]]
  
  # Argumento de ponderacao das classes para o SVM
  idVarClassifBsTr <- which(names(bsTreino) %in% c(NOMEVARCLASSIF))
  numClCrime <- sum(as.numeric(as.character(bsTreino[, idVarClassifBsTr])))
  numObsBsTreino <- nrow(bsTreino)
  numClNaoCrime <- numObsBsTreino - numClCrime
  
  # Selecionar na Base de Teste apenas aquelas linhas em que o cluster predito é o mesmo do cenario da vez
  entBsTeste <- which(names(bsTstSplit) == bsDaIt)
  
  bsTeste <- bsTstSplit[[ entBsTeste ]]
  
  # Argumento de ponderacao das classes para o SVM
  idVarClassifBsTst <- which(names(bsTeste) %in% c(NOMEVARCLASSIF))
  numClCrimeTst <- sum(as.numeric(as.character(bsTeste[, idVarClassifBsTst])))
  numObsBsTeste <- nrow(bsTeste)
  numClNaoCrimeTst <- numObsBsTeste - numClCrimeTst
  
  gc()
  ## Base identificacao
  idVarIDBsTst <- which(names(bsTeste) %in% NOMEVARID)
  if(all.equal(row.names(bsTeste), bsTeste[, idVarIDBsTst])){
    identifTst <- data.frame(idTweet = bsTeste[, idVarIDBsTst],
                             Classif = bsTeste[, idVarClassifBsTst])
  }
  
  # Excluindo colunas desnecessarias
  idVarBsTrExcluir <- which(names(bsTreino) %in% c(NOMEVARID, NOMEVARCLUSTER))
  bsTreino[, idVarBsTrExcluir] <- NULL
  idVarBsTstExcluir <- which(names(bsTeste) %in% c(NOMEVARID, NOMEVARCLUSTER, NOMEVARCLASSIF))
  bsTeste[, idVarBsTstExcluir] <- NULL
  
  if(TECNICA == "SVM"){
    print(paste("Tecnica =", TECNICA))
    
    # Parametro do LiblineaR
    TIPOSVM <- 5
    
    ## Separando coluna de classificacao da base de Treino
    idVarBsTrClassif <- which(names(bsTreino) %in% NOMEVARCLASSIF)
    varBsTreinoLabel <- bsTreino[, idVarBsTrClassif]
    bsTreino[, idVarBsTrClassif] <- NULL
    
    ## Tirando colunas que soh tem um nivel
    colTreino <- apply(X = bsTreino, MARGIN = 2, FUN = function(x) length(unique(x)))
    colTreinoSoh1Nivel <- which(colTreino == 1)
    
    nomeColExcluir <- dimnames(bsTreino)[[2]][colTreinoSoh1Nivel]
    if(length(colTreinoSoh1Nivel) != 0){bsTreino <- bsTreino[, -colTreinoSoh1Nivel]}
    
    idColTstExcluir <- which(names(bsTeste) %in% nomeColExcluir)
    if(length(idColTstExcluir) != 0){bsTeste <- bsTeste[, -idColTstExcluir]}
    
    bsTreino <- apply(X = bsTreino, MARGIN = 2, FUN = function(x) as.numeric(x))
    
    bsTreinoScaled <- scale(bsTreino, center = TRUE, scale = TRUE)
    
    numInstancesBsTeste <- nrow(bsTeste)
    numFeaturesBsTeste <- ncol(bsTeste)
    
    bsTeste <- apply(X = bsTeste, MARGIN = 2, FUN = function(x) as.numeric(x))
    
    if(class(bsTeste) != "matrix"){
      bsTeste <- matrix(bsTeste, nrow = numInstancesBsTeste, ncol = numFeaturesBsTeste, byrow = T)
    }
    
    bsTesteScaled <- scale(x = bsTeste,
                           center = attr(bsTreinoScaled,"scaled:center"),
                           scale = attr(bsTreinoScaled,"scaled:scale"))
    
    ## -- SVM ----
    t1 <- Sys.time(); t1
    classificationModel <- try(classificationModel <- LiblineaR(data = bsTreinoScaled,
                                                                target = varBsTreinoLabel,
                                                                type = TIPOSVM,
                                                                cost = CUSTO,
                                                                bias = TRUE,
                                                                verbose = FALSE),
                               silent = TRUE)
    t2 <- Sys.time(); t2
    
  }else{

  
  
  ## -- Se deu erro -- ##
  if("try-error" %in% class(classificationModel)){
    resultadoFinal <- data.frame(PARTE, NUMEROCLUSTERS, CLUSTER, CUSTO, "Erro", TECNICA,
                                 numObsBsTreino, numClCrime, numClNaoCrime,
                                 numObsBsTeste, numClCrimeTst, numClNaoCrimeTst,
                                 NA, NA, NA, NA, NA, NA, NA, NA,
                                 t1, t2, NA, NA)
  } else{
    ## -- Predição -- ##
    t3 <- Sys.time()
    if(TECNICA == "SVM"){modelPred <- stats::predict(classificationModel, bsTeste)}
    t4 <- Sys.time()
    
    if(class(modelPred) == "list"){modelPred <- unlist(modelPred)}
    
    
    if(TECNICA == "SVM"){
      modelPredNum <- as.numeric(as.character(modelPred))
      identifTst$"Predicao" <- modelPredNum
    }
    
  
    # Juntando a informacao da predicao com as informacoes da Base de Teste
    identifTst <- merge(identifTst, dfModelPred, by = "idTweet")
    
    ## -- Métrica de acurácia -- ##
    rotulado0_model0 <- length(which(identifTst$"Classif" == 0 & identifTst$"Predicao" == 0))
    rotulado0_model1 <- length(which(identifTst$"Classif" == 0 & identifTst$"Predicao" == 1))
    
    rotulado1_model0 <- length(which(identifTst$"Classif" == 1 & identifTst$"Predicao" == 0))
    rotulado1_model1 <- length(which(identifTst$"Classif" == 1 & identifTst$"Predicao" == 1))
    
    precisao <- rotulado1_model1 / (rotulado1_model1 + rotulado0_model1)
    recall <- rotulado1_model1 / (rotulado1_model1 +  rotulado1_model0 )
    f1 <- (2*recall*precisao) / (recall+precisao)
    acuracia <- sum(rotulado0_model0, rotulado1_model1)/sum(rotulado0_model0, rotulado0_model1, rotulado1_model0, rotulado1_model1)
  }
  
  ## -- Salvando ----
  # Resultado Final
  if(TECNICA %in% c("SVM", "RandomForest", "LogisticRegression", "Boosting")){
    resultadoFinal <- data.frame(PARTE, NUMEROCLUSTERS, CLUSTER, CUSTO, "Feito", TECNICA,
                                 numObsBsTreino, numClCrime, numClNaoCrime,
                                 numObsBsTeste, numClCrimeTst, numClNaoCrimeTst,
                                 rotulado0_model0, rotulado0_model1,
                                 rotulado1_model0, rotulado1_model1,
                                 precisao, recall, f1, acuracia,
                                 t1, t2,
                                 t3, t4)
    
    # Naming the columns with English expressions
    names(resultadoFinal) <-   c("Part", "numGroups", "Base", "Cost", "ModelStatus", "MachLearnTech",
                                 "numTrainInst", "numCrimeTrainDB", "numClNonCrimeTrainDB",
                                 "numTestInst", "numCrimeTestDB", "numClNonCrimeTestDB",
                                 "LabelNonCrime_ModelNonCrime", "LabelNonCrime_ModelCrime", "LabelCrime_ModelNonCrime", "LabelCrime_ModelCrime",
                                 "Precision", "Recall", "F1", "Accuracy",
                                 "ModelBegtime", "ModelEndtime",
                                 "PredictionBegtime", "PredictionEndtime")
    
    nomeArqSalvar <- paste("res",
                           paste0("tecnica", TECNICA),
                           paste0("parte", PARTE), paste0("numGrupos", NUMEROCLUSTERS),
                           paste0("base", CLUSTER), paste0("custo", CUSTO), sep = "_")
    nomeArqSalvar <- paste0(nomeArqSalvar, ".RData")
    
    nomeArqIteracao <- paste0("res",
                              "_tec", TECNICA,
                              "_iter", ITERACAO,
                              "_parte", PARTE,
                              "_numGr", NUMEROCLUSTERS,
                              "_base", CLUSTER,
                              "_custo", CUSTO,
                              ".RData")
    
    TEMPOFIMITER <- Sys.time()
    iter <- data.frame(TECNICA = TECNICA,
                       ITERACAO = ITERACAO,
                       PARTE = PARTE,
                       NUMEROCLUSTERS = NUMEROCLUSTERS,
                       CLUSTER = CLUSTER,
                       CUSTO = CUSTO,
                       TEMPOCOMITER = TEMPOCOMITER,
                       TEMPOFIMITER = TEMPOFIMITER)
  }
  
  save(resultadoFinal, file = file.path(CAMSALVAR, nomeArqSalvar))
  save(x = iter, file = file.path(CAMSALVARITER, nomeArqIteracao))
}