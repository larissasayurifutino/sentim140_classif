args = commandArgs(trailingOnly = TRUE)
# rm(list = ls()); gc()
# CLUSTER=1
# CUSTO=0.1
# TIPOSVM=5
# NUMEROCLUSTERS = 7
# R CMD BATCH '--args CLUSTER=2 CUSTO=0.1 TIPOSVM=5 NUMEROCLUSTERS=7' script26_SVM.R
if(length(args)==0){
  print("No arguments supplied.")
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

## -- Arquivos ----

#cam <- "/media/luis/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/matDTM"
cam <- "/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/matDTM"
CAMSALVAR <- "/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/resClassif"

arqs <- list.files(cam)

arqsMatDTMTr <- arqs[stringr::str_detect(string = arqs, pattern = "listaTr")]
#arqsMatDTMTr <- arqsMatDTMTr[!stringr::str_detect(string = arqsMatDTMTr, pattern = "Tst")]
arqsMatDTMTst <- arqs[stringr::str_detect(string = arqs, pattern = "listaTst")]
arqsLabelsTr <- arqs[stringr::str_detect(string = arqs, pattern = "labelsTr")]
arqsLabelsTst <- arqs[stringr::str_detect(string = arqs, pattern = "labelsTst")]
rm(arqs)


#CLUSTER <- 1
TECNICA <- "SVM"
#TIPOSVM <- 5
#CUSTO <- 0.1
# NUMEROCLUSTERS <- 7

## -- Carregando objeto Treino ----
load(file = file.path(cam , arqsMatDTMTr[CLUSTER])) # matDTMCast


#if(all.equal(matDTMCast$"id", labelsTr$"document")){
#bsTreino <- matDTMCast
#rm(matDTMCast); gc(reset = TRUE)




## -- Padronizando variaveis ----
# tCom <- Sys.time(); tCom
# bsTreino <- apply(X = matDTMCast, MARGIN = 2, FUN = function(x) as.numeric(x))
# rm(matDTMCast); gc(reset = TRUE)
# bsTreinoScaled <- scale(bsTreino, center = TRUE, scale = TRUE)
# tFim <- Sys.time(); tFim
# 
# vetMedias <- apply(bsTreino, 2, mean)
# vetDesvios <- apply(bsTreino, 2, sd)

# rm(bsTreino); gc(reset = TRUE)
#}else{
#print("Objeto matDTMCast e labels nao tem mesma ordenacao")
#}


load(file = file.path(cam , arqsLabelsTr[CLUSTER])) # labelsTr
labelsTr <- unique(labelsTr)
varBsTreinoLabel <- labelsTr$"target"
rm(labelsTr); gc(reset = TRUE)

numClCrime <- length(which(varBsTreinoLabel == 4))
numObsBsTreino <- nrow(listaTr$bsTreinoScaled)
numClNaoCrime <- numObsBsTreino - numClCrime

## -- SVM ----
t1 <- Sys.time(); t1
classificationModel <- try(LiblineaR::LiblineaR(data = listaTr$"bsTreinoScaled",
                                                target = varBsTreinoLabel,
                                                type = TIPOSVM,
                                                cost = CUSTO,
                                                bias = TRUE,
                                                verbose = FALSE),
                           silent = TRUE)

t2 <- Sys.time(); t2
t2 - t1

gc(reset = TRUE)

## -- Se deu erro ----
if("try-error" %in% class(classificationModel)){
  resultadoFinal <- data.frame(PARTE, NUMEROCLUSTERS, CLUSTER, CUSTO, "Erro", TECNICA,
                               numObsBsTreino, numClCrime, numClNaoCrime,
                               numObsBsTeste, numClCrimeTst, numClNaoCrimeTst,
                               NA, NA, NA, NA, NA, NA, NA, NA,
                               t1, t2, NA, NA)
}else{
  ## -- Carregando objeto matriz DTM Teste ----
  load(file = file.path(cam , arqsMatDTMTst[CLUSTER])) # matDTMCastTst
  
  #bsTeste <- matDTMCastTst
  
  #bsTeste <- apply(X = matDTMCastTst, MARGIN = 2, FUN = function(x) as.numeric(x))
  #rm(matDTMCastTst); gc(reset = TRUE)
  numInstancesBsTeste <- nrow(listaTst$bsTeste)
  numFeaturesBsTeste <- ncol(listaTst$bsTeste)
  
  # if(class(listaTst$bsTeste) != "matrix"){
  #   bsTeste <- matrix(bsTeste, nrow = numInstancesBsTeste, ncol = numFeaturesBsTeste, byrow = T)
  # }
  
  bsTesteScaled <- scale(x = listaTst$bsTeste,
                         center = listaTr$vetMedias,
                         scale = listaTr$vetDesvios)
  rm(listaTst); gc(reset = TRUE)
  
  ## -- Predição ----
  t3 <- Sys.time()
  modelPred <- stats::predict(classificationModel, bsTesteScaled)
  t4 <- Sys.time()
  
  if(class(modelPred) == "list"){modelPred <- unlist(modelPred)}
  
  ## -- Carregando objeto labels de Teste ----
  load(file = file.path(cam , arqsLabelsTst[CLUSTER])) # labelsTst
  
  ## -- Base identificacao ----
  identifTst <- labelsTst
  rm(labelsTst); gc(reset = TRUE)
  
  ## -- Juntando a informacao da predicao com as informacoes da Base de Teste ----
  modelPredNum <- as.numeric(as.character(modelPred))
  identifTst$"Predicao" <- modelPredNum
  
  
  numClCrimeTst <- length(which(identifTst$"target" == 4))
  numObsBsTeste <- numInstancesBsTeste
  numClNaoCrimeTst <- numObsBsTeste - numClCrimeTst
  
  
  ## -- Métrica de acurácia ----
  rotulado0_model0 <- length(which(identifTst$"target" == 0 & identifTst$"Predicao" == 0))
  rotulado0_model4 <- length(which(identifTst$"target" == 0 & identifTst$"Predicao" == 4))
  
  rotulado4_model0 <- length(which(identifTst$"target" == 4 & identifTst$"Predicao" == 0))
  rotulado4_model4 <- length(which(identifTst$"target" == 4 & identifTst$"Predicao" == 4))
  
  precisao <- rotulado4_model4 / (rotulado4_model4 + rotulado0_model4)
  recall <- rotulado4_model4 / (rotulado4_model4 +  rotulado4_model0 )
  f1 <- (2*recall*precisao) / (recall+precisao)
  acuracia <- sum(rotulado0_model0, rotulado4_model4)/sum(rotulado0_model0, rotulado0_model4, rotulado4_model0, rotulado4_model4)
  
  
  #NUMEROCLUSTERS <- length(arqsMatDTMTr)
  
  resultadoFinal <- data.frame(#PARTE, 
    NUMEROCLUSTERS, 
    CLUSTER, 
    CUSTO, 
    "Feito", 
    TECNICA,
    numObsBsTreino, numClCrime, numClNaoCrime,
    numObsBsTeste, numClCrimeTst, numClNaoCrimeTst,
    rotulado0_model0, rotulado0_model4,
    rotulado4_model0, rotulado4_model4,
    precisao, recall, f1, acuracia,
    t1, t2,
    t3, t4)
  
  # Naming the columns with English expressions
  names(resultadoFinal) <-   c(#"Part", 
    "numGroups", 
    "Base", 
    "Cost", 
    "ModelStatus", 
    "MachLearnTech",
    "numTrainInst", "numCrimeTrainDB", "numClNonCrimeTrainDB",
    "numTestInst", "numCrimeTestDB", "numClNonCrimeTestDB",
    "LabelNonCrime_ModelNonCrime", "LabelNonCrime_ModelCrime", "LabelCrime_ModelNonCrime", "LabelCrime_ModelCrime",
    "Precision", "Recall", "F1", "Accuracy",
    "ModelBegtime", "ModelEndtime",
    "PredictionBegtime", "PredictionEndtime")
  
  NUMEROCLUSTERSCH <- ifelse(nchar(NUMEROCLUSTERS) != 2, paste0("0", NUMEROCLUSTERS), as.character(NUMEROCLUSTERS))  
  CLUSTERCH <- ifelse(nchar(CLUSTER) != 2, paste0("0", CLUSTER), as.character(CLUSTER))    
  
  nomeArqSalvar <- paste("res",
                         paste0("tecnica", TECNICA),
                         #paste0("parte", PARTE), 
                         paste0("numGrupos", NUMEROCLUSTERSCH),
                         paste0("base", CLUSTERCH), 
                         paste0("custo", CUSTO), sep = "_")
  nomeArqSalvar <- paste0(nomeArqSalvar, ".RData")
  
  save(resultadoFinal, file = file.path(CAMSALVAR, nomeArqSalvar))
}


