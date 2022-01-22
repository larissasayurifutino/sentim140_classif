rm(list = ls()); gc()

library(dplyr)
library(data.table)
library(foreach)
doParallel::registerDoParallel(cores = 3)
library(FSelectorRcpp)

## -- Funcao ----
fcImputarFreqTermos <- function(df, dfLabel){
  dt <- data.table(df)
  
  termo <- unique(dt[, 'term'])
  aux <- merge(x = dt, 
               y = dfLabel, 
               by = c("document", "target"), 
               all.y = TRUE)
  
  # Atualizando contagens
  aux[is.na(count), count := 0]
  aux[is.na(term), term := as.character(termo$term)]
  aux$"target" <- factor(aux$"target")
  
  return(aux)
}


fcMacro <- function(dfUpdate, dfLabel = dfLabel){
  dfImput <- fcImputarFreqTermos(df = dfUpdate, dfLabel = dfLabel)
  infoGainCalc <- FSelectorRcpp::information_gain(x = data.frame(att = dfImput$count), 
                                                  y = dfImput$target, 
                                                  type = "infogain")
  return(infoGainCalc)
}

## -- Dados ----
cam <- "/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases"
load(file = file.path(cam, "distFreqDadosSentSemSW.RData"))
load(file = file.path(cam, "df.RData"))

# aux <- df[seq(200),]
# 
# xx <- df %>%
#   group_by(term, target) %>%
#   summarise(numDifDocs = n_distinct(document))

h_att <- h_att[which(h_att$"freq" > 50),]

tstLogQualTermManter <- which(df$"term" %in% h_att$"term")
#tstLogQualTermManter <- which(df$"term" == "indors")
dfUpdate <- df[tstLogQualTermManter, ]
#dim(df) - dim(dfUpdate)

## -- Auxiliar ----
dfLabel <- unique(dfUpdate[, c("document", "target")])

## -- Quebrando a base por termos ----
lista <- split(x = dfUpdate, f = dfUpdate$"term")

# dd <- list(lista[[1]], lista[[2]], lista[[3]], 
#            lista[[4]], lista[[5]], lista[[6]],
#            lista[[7]], lista[[8]], lista[[9]],
#            lista[[10]], lista[[11]], lista[[12]])

# FSelector::information.gain(formula = target ~ count, 
#                             data = ww)

rm(h_att); rm(dfUpdate); gc(reset = TRUE)

tCom <- Sys.time(); tCom # "2019-06-02 16:06:20 -03"
numTermos <- length(lista)
resultFinal <- foreach(posicao = 1:numTermos, .combine = rbind) %dopar%{
  result <- fcMacro(dfUpdate = lista[[posicao]], dfLabel = dfLabel)
  result$term <- unique(lista[[posicao]]$term)
  gc(reset = TRUE)
  
  return(result)
}
tFim <- Sys.time(); tFim # 
tFim - tCom

tempoInfoGain <- data.frame(tCom = tCom,
tFim = tFim,
dif = tFim - tCom)

save(tempoInfoGain, file = file.path(cam, "tempoInfoGain.RData"))

#load(file = file.path(cam, "distFreqDadosSentSemSWPorLabel.RData"))

# fawcet: 0.0001460276
# abandoned: 1.813515e-05
# inapropriate: 0
# indoor: 2.864924e-05
