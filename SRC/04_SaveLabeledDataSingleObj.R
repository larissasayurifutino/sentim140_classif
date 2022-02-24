## -- This script reads .txt files with the labeled data by linguakit and it saves in a single RData file
rm(list = ls()); gc()

library(stringr)
library(future)
library(future.apply)
library(doParallel)
options(scipen = 999)


path <- "/home/larissa/Documents/repos/sentim140_classif/DATA/FilesLinguakitSentiment140/02_AlreadyAnnot/"


## --------------- ##
## -- Functions -- ##
## --------------- ##
readAlrAnnotated <- function(file, 
                             charactWithID = nchar(path)){
  id <- substr(file, charactWithID + 2, nchar(file) - 4)
  

  readFile <- try(read.table(file, header = FALSE, 
                            sep = "", stringsAsFactors = FALSE,
                            quote = "&&"),
                 silent = TRUE) 

  
  if("try-error" %in% class(readFile)){
    message(paste('Error at id:', id))

    finalFile <- 999999
    names(finalFile) <- id
  }else{
    finalFile <- paste0(readFile$'V3'[-length(readFile$'V3')], collapse = "")
    names(finalFile) <- id
  }
  
  return(finalFile)
  
}

## -------------------------------------- ##
## -- Which files we are going to read -- ##
## -------------------------------------- ##

# To list all files of a directory faster
allAnnotatedFiles <- system("ls /home/larissa/Documents/repos/sentim140_classif/DATA/FilesLinguakitSentiment140/02_AlreadyAnnot/", intern = TRUE)

# To test (with a single file):
first_file <- readAlrAnnotated(file = file.path(path, allAnnotatedFiles[1]), 
                 charactWithID = nchar(path))


# All files:
## To run in parallel on local computer
cl <- makeCluster(2)
registerDoParallel(cores = 2)
plan(multisession) 



tBeg <- Sys.time(); tBeg
listSeqAnnotFiles <- future_lapply(X = file.path(path, allAnnotatedFiles), FUN = readAlrAnnotated)
tEnd <- Sys.time(); tEnd

fileSaveRData <- "/home/larissa/Documents/repos/sentim140_classif/DATA"

save(listSeqAnnotFiles,
     file = paste(fileSaveRData, "listSeqAnnotFiles.RData", sep = "/"))
