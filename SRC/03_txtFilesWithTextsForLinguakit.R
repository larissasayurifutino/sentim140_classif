## -- This script saves .txt files to be read and labeled by linguakit 
rm(list = ls()); gc()

# TO avoid scientific notation
options(scipen = 999)

## -- To load text files -- ##
direct <- "DATA"
sentiment140 <- readRDS(file.path(direct, "sentiment140.rds"))

## -- Where to save the files -- ##
directLinguakit <- file.path(direct, "FilesLinguakitSentiment140")
if(!file.exists(directLinguakit)){
  dir.create(directLinguakit)
}
saveToAnnotate <- file.path(directLinguakit, "01_ToAnnotate")
if(!file.exists(saveToAnnotate)){
  dir.create(saveToAnnotate)
}
saveAlrAnnotate <- file.path(directLinguakit, "02_AlreadyAnnot")
if(!file.exists(saveAlrAnnotate)){
  dir.create(saveAlrAnnotate)
}
#setwd(saveToAnnotate)


## -- .txt files for each text to be annotated -- ##
howManyTweets <- nrow(sentiment140)

# Notebook
i = 1
tBeg <- Sys.time(); tBeg
  for(i in 1:howManyTweets){
    fileName <- paste0(sentiment140$"id"[i], ".txt")
    texto <- paste(sentiment140$"text"[i])
    write.table(x = texto,
                file = file.path(saveToAnnotate, fileName),
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE,
                fileEncoding = "UTF-8")
  }
tEnd <- Sys.time(); tEnd
df_time <- data.frame(time = c("begin", "end", "dif"),
           value = c(tBeg, tEnd, tEnd - tBeg))
df_time


## -- Writing a SINGLE FILE with all texts to be annotated -- ##
## -- This file is going to be used at Linux terminal -- ##
nameFileToAnnot <- paste0("./linguakit tagger en /home/larissa/Documents/repos/sentim140_classif/DATA/FilesLinguakitSentiment140/01_ToAnnotate/", 
                          sentiment140$"id", ".txt ")
nameFileAlrAnnot <- paste0("> /home/larissa/Documents/repos/sentim140_classif/DATA/FilesLinguakitSentiment140/02_AlreadyAnnot/", 
                        sentiment140$"id", ".txt")

fileToBatch <- paste0(nameFileToAnnot, nameFileAlrAnnot)
fileToBatch <- c("#!/bin/sh", "", fileToBatch)
fileToBatch <- data.frame(fileToBatch)

#setwd("../../..")

write.table(x = fileToBatch,
            file = file.path("SRC", "fileToBatch.sh"),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            fileEncoding = "UTF-8")

## To install Linguakit:
# https://github.com/citiususc/Linguakit