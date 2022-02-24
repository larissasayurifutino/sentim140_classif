rm(list = ls()); gc()
library(tm)
library(dplyr)
library(tidytext)
library(stringr)
library(slam)


## -- File ----
diret <- "DATA"
files <- list.files(diret)
nameFileLoad <- files[str_detect(string = files, pattern = "sentiment")]
dfText <- readRDS(file.path(diret, nameFileLoad))


## -- Applying function to data ----
nameVarClassif <- "target" 
nameVarText <- "text"
nameVarId <- "id"


# Column indexes of the variables we are interested
column_idVar <- which(names(dfText) == nameVarId)
column_targetVar <- which(names(dfText) == nameVarClassif)
column_textVar <- which(names(dfText) == nameVarText)

## -- Remove empty text entries ----
df <- dfText[, c(column_idVar, column_textVar)]; gc()
column_idVarDF <- which(names(df) == nameVarId)
df <- df[-which(df$text == ""),]


## -- Tweets classification/target ----
dfClassif <- data.frame(id = as.character(dfText[, column_idVar]),
                        Classif = dfText[, column_targetVar],
                        stringsAsFactors = FALSE)

dfClassif$"id" <- as.character(dfClassif$"id")
dfClassif$"Classif" <- as.factor(dfClassif$"Classif")
levels(dfClassif$"Classif") <- c("0", "4")
row.names(dfClassif) <- dfClassif$"id"

save(dfClassif, file = "DATA/fileTargetNoDuplic.RData")


rm(dfText); gc()

tEmpilDf <- system.time(
dfEmpil <- df %>%
  unnest_tokens(word, text)
)
names(dfEmpil) <- c("id", "word")


## -- Frequency objects ----

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
