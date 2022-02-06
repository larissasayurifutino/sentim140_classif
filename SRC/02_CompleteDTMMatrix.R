rm(list = ls()); gc()
library(tm)
library(dplyr)
library(tidytext)
library(stringr)
library(slam)

## -- Funcoes ----

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
funcaoPassaArgs <- function(DFARGS, NOMEVARANOT, nameVarId = "id"){
  colAnot <- which(names(DFARGS) == NOMEVARANOT)
  colID <- which(names(DFARGS) == nameVarId)
  DF <- funcao(ANOT = DFARGS[, colAnot], ID = DFARGS[, colID]) 
  return(DF)
}

## -- File ----
diret <- "DATA"
files <- list.files(diret)
nameFileLoad <- files[str_detect(string = files, pattern = "sentiment")]
dfText <- readRDS(file.path(diret, nameFileLoad))


## -- Applying function to data ----
nameVarClassif <- "target" 
nomeVarText <- "text"
nameVarId <- "id"
#peso <- "binary"

# Objeto de Retorno da Funcao
listaRetorno <- vector(mode = "list", length = 1)
names(listaRetorno) <- "dfDTMMatClassif"

# O indice das colunas associadas às informacoes que queremos guardar
colunaVarId <- which(names(dfText) == nameVarId)
colunaVarClassif <- which(names(dfText) == nameVarClassif)
colunaVarTexto <- which(names(dfText) == nomeVarText)

## -- Informacao da classificacao de kd tweet -- ##
dfClassif <- data.frame(id = as.character(dfText[, colunaVarId]),
                        Classif = dfText[, colunaVarClassif],
                        stringsAsFactors = FALSE)

dfClassif$"id" <- as.character(dfClassif$"id")
dfClassif$"Classif" <- as.factor(dfClassif$"Classif")
levels(dfClassif$"Classif") <- c("0", "4")
row.names(dfClassif) <- dfClassif$"id"


df <- dfText[, c(colunaVarId, colunaVarTexto)]; gc()
colunaVarIdDF <- which(names(df) == nameVarId)

# Remove empty text entries
df <- df[-which(df$text == ""),]

#dfSplit <- split(x = df, f = df[, colunaVarIdDF]); gc()

## -- Empilhando ----
# tStringColunas <- system.time(
#   anotEmpilSplit <- lapply(X = dfSplit, FUN = funcaoPassaArgs, NOMEVARANOT = nomeVarText, nameVarId = nameVarId)  
# )
# tStringColunas
# 
# 
# save(anotEmpilSplit, file = "/home/leste/DBSET/Doutorado/BaseDadosTese/06_AplicacaoSentimento/01_Bases/anotEmpilSplit.RData")
#rm(dfSplit); 

rm(dfText); gc()

tEmpilDf <- system.time(
dfEmpil <- df %>%
  unnest_tokens(word, text)
)
names(dfEmpil) <- c("id", "word")

# tEmpilDf <- system.time(
#   anotEmpilDf <- do.call(rbind, anotEmpilSplit)  
# )
# tEmpilDf
## -- DTM -- ##
# trainvector <- as.vector(dfText[, colunaVarTexto])
# names(trainvector) <- dfText[, colunaVarId]
# 
# traincorpus <- Corpus(VectorSource(trainvector))
# traincorpus <- tm_map(traincorpus, stripWhitespace)



# To count the number of words occurrence and to filter the words with at least 05 appearances
freqMatDTM <- dfEmpil %>%
  group_by(word) %>%
  summarise(n = n(), sort = TRUE) %>%
  filter(n > 4)

save(freqMatDTM, file = "DATA/freqMatDTM.RData")


# To count the number of words occurrence by text id
freqMatDTMAux <- dfEmpil %>%
  group_by(id, word) %>%
  count(word)


# To keep only the words that are at freqMatDTM
freqMatDTMAuxPalComFreqMin <-freqMatDTMAux %>%
  filter(word %in% freqMatDTM$"word")

freqMatDTMAuxPalComFreqMin$"n" <- 1

save(freqMatDTMAuxPalComFreqMin, file = "DATA/freqMatDTMAuxPalComFreqMin.RData")



# Data as a matrix
matDTMCast <- cast_dtm(data = freqMatDTMAuxPalComFreqMin, 
                       document = "id",
                       term =  "word", 
                       value = "n", 
                       weighting = function(x) weightSMART(x, spec = "bnn"))

save(matDTMCast, file = "DATA/matDTMCast.RData")
