rm(list = ls()); gc()
library(tm)
library(dplyr)
library(tidytext)
library(stringr)
library(slam)
## -- Funcaoes -- ##
# funcao
funcao <- function(ANOT, ID){
  print(ID)
  anotSplit <- strsplit(x = as.character(ANOT), split = " ")
  anotSplit <- unlist(anotSplit)
  df <- data.frame(id = ID,
                   anotSemant = anotSplit)  
  return(df)
}

# funcaoPassaArgs
# Precisa de: funcao
funcaoPassaArgs <- function(DFARGS, NOMEVARANOT, NOMEVARID = "id"){
  colAnot <- which(names(DFARGS) == NOMEVARANOT)
  colID <- which(names(DFARGS) == NOMEVARID)
  DF <- funcao(ANOT = DFARGS[, colAnot], ID = DFARGS[, colID]) 
  return(DF)
}

diret <- "/home/leste/DBSET/Doutorado/BaseDadosTese/06_AplicacaoSentimento/01_Bases"
arq <- list.files(diret)
nomeArqLoad <- arq[str_detect(string = arq, pattern = "sentiment")]
sentiment140 <- readRDS(file.path(diret, nomeArqLoad))

# funcaoCriarMatrizDTMAnot <- function(dfTexto, 
#                                       nomeVarClassif, 
#                                       nomeVarTexto, 
#                                       nomeVarId,
#                                       peso){
dfTexto <- sentiment140; rm(sentiment140); gc()
nomeVarClassif <- "target" 
nomeVarTexto <- "text"
nomeVarId <- "id"
peso <- "binary"

# Objeto de Retorno da Funcao
listaRetorno <- vector(mode = "list", length = 2)
names(listaRetorno) <- c("dfmatDTMComClassif", "descritivaDfmatDTMComClassif")

# O indice das colunas associadas às informacoes que queremos guardar
colunaVarId <- which(names(dfTexto) == nomeVarId)
colunaVarClassif <- which(names(dfTexto) == nomeVarClassif)
colunaVarTexto <- which(names(dfTexto) == nomeVarTexto)

## -- Informacao da classificacao de kd tweet -- ##
dfClassif <- data.frame(id = as.character(dfTexto[, colunaVarId]),
                        Classif = dfTexto[, colunaVarClassif],
                        stringsAsFactors = FALSE)

dfClassif$"id" <- as.character(dfClassif$"id")
dfClassif$"Classif" <- as.factor(dfClassif$"Classif")
levels(dfClassif$"Classif") <- c("0", "4")
row.names(dfClassif) <- dfClassif$"id"


df <- dfTexto[, c(colunaVarId, colunaVarTexto)]; gc()
colunaVarIdDF <- which(names(df) == nomeVarId)

df <- df[-which(df$text == ""),]

#dfSplit <- split(x = df, f = df[, colunaVarIdDF]); gc()

## -- Empilhando ----
# tStringColunas <- system.time(
#   anotEmpilSplit <- lapply(X = dfSplit, FUN = funcaoPassaArgs, NOMEVARANOT = nomeVarTexto, NOMEVARID = nomeVarId)  
# )
# tStringColunas
# 
# 
# save(anotEmpilSplit, file = "/home/leste/DBSET/Doutorado/BaseDadosTese/06_AplicacaoSentimento/01_Bases/anotEmpilSplit.RData")

rm(dfSplit); rm(dfTexto); gc()

tEmpilDf <- system.time(
dfEmpil <- df %>%
  unnest_tokens(word, text)
)

# tEmpilDf <- system.time(
#   anotEmpilDf <- do.call(rbind, anotEmpilSplit)  
# )
# tEmpilDf
## -- DTM -- ##
# trainvector <- as.vector(dfTexto[, colunaVarTexto])
# names(trainvector) <- dfTexto[, colunaVarId]
# 
# traincorpus <- Corpus(VectorSource(trainvector))
# traincorpus <- tm_map(traincorpus, stripWhitespace)

# Objeto com os codigos de anotacao
names(dfEmpil) <- c("id", "word")

# Contando o numero de codigos anotados por documento e filtrando casos com frequencia pelo menos = 5
freqMatDTM <- dfEmpil %>%
  group_by(word) %>%
  summarise(n = n(), sort = TRUE) %>%
  filter(n > 4)

#save(freqMatDTM, file = "/home/leste/DBSET/Doutorado/BaseDadosTese/06_AplicacaoSentimento/01_Bases/freqMatDTM.RData")
# listaRetorno$"descritivaDfmatDTMComClassif" <- freqMatDTM
# rm(freqMatDTM)


freqMatDTMAux <- dfEmpil %>%
  group_by(id, word) %>%
  count(word)

 
freqMatDTMAuxPalComFreqMin <-freqMatDTMAux %>%
  filter(word %in% freqMatDTM$"word")


freqMatDTMAuxPalComFreqMin$"n" <- 1


# Desempilhando os dados
matDTMCast <- cast_dtm(data = freqMatDTMAuxPalComFreqMin, 
                       document = "id",
                       term =  "word", 
                       value = "n", 
                       weighting = function(x) weightSMART(x, spec = "bnn"))

# if(tolower(peso) == "binary"){
#   matDTMCastPesoAdeq <- as.DocumentTermMatrix(x = matDTMCast, 
#                                               control = list(weighting = function(x) weightSMART(x, spec = "bnn")))
# }else{
#   if(toupper(peso) == "TF-IDF"){
#     matDTMCastPesoAdeq <- as.DocumentTermMatrix(x = matDTMCast, 
#                                                 control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))
#   }else{
#     print("Nao foi possivel identificar o tipo de peso desejado.")
#   }  
# }
# 
# rm(matDTMCast); gc()

# xx <- inspect(matDTMCast)
# matDTMCast <- cbind("id" = row.names(matDTMCast), matDTMCast)
# dimnames(matDTMCast)[[2]][1:4] <- "id"

# matDTMMatrix <- as.matrix(matDTMCastPesoAdeq) # passando um DTM para uma matriz. Acaba imprimindo na tela... Mas é necessário.
# dim(matDTMMatrix)
# head(row.names(matDTMMatrix))

# dfmatDTM <- as.data.frame(matDTMMatrix)
# head(row.names(dfmatDTM))

# dfmatDTM$"id" <- as.character(row.names(dfmatDTM))
# dim(dfmatDTM)


idsEstaoMatDTM <- which(row.names(dfClassif) %in% row.names(matDTMCast))
dfClassif <- dfClassif[idsEstaoMatDTM,]

dfClassifSTM <- as.simple_triplet_matrix(x = dfClassif)
# if(class(matDTMCast$"id") == "character" & class(dfClassif$"id") == "character"){
#   dfmatDTMComClassif <- merge(matDTMCast, dfClassif, by = "id")
#   row.names(dfmatDTMComClassif) <- dfmatDTMComClassif$"id"
# }

mat.end <- abind_simple_sparse_array(matDTMCast, dfClassifSTM, MARGIN = 2L)

## -- Retorno ----
listaRetorno$"dfmatDTMComClassif" <- dfmatDTMComClassif
return(listaRetorno)
#}