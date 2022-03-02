## -- This script applies Information Gain to parts of the original data set
# 15
rm(list = ls()); gc()

library(dplyr)
library(data.table)
library(foreach)
doParallel::registerDoParallel(cores = 3)
library(FSelectorRcpp)

## -- Functions ----
imputeTermsFreq <- function(df, dfLabel){
  dt <- data.table(df)
  
  termo <- unique(dt[, 'term'])
  aux <- merge(x = dt, 
               y = dfLabel, 
               by = c("document", "target"), 
               all.y = TRUE)
  
  # Update counts
  aux[is.na(count), count := 0]
  aux[is.na(term), term := as.character(termo$term)]
  aux$"target" <- factor(aux$"target")
  
  return(aux)
}


imputeTermsInfoGain <- function(dfUpdate, dfLabel = dfLabel){
  dfImput <- imputeTermsFreq(df = dfUpdate, dfLabel = dfLabel)
  infoGainCalc <- FSelectorRcpp::information_gain(x = data.frame(att = dfImput$count), 
                                                  y = dfImput$target, 
                                                  type = "infogain")
  return(infoGainCalc)
}

## -- Data ----
path <- "DATA"
load(file = file.path(path, "freqDistSentDataNoSW.RData"))
load(file = file.path(path, "df.RData"))


# Filter terms to keep only those with more than 50 occurrences
h_att <- h_att[which(h_att$"freq" > 50),]

termsToKeep <- which(df$"term" %in% h_att$"term")
dfUpdate <- df[termsToKeep, ]
#dim(df) - dim(dfUpdate)

## -- Label object ----
dfLabel <- unique(dfUpdate[, c("document", "target")])

## -- To break data base by terms ----
dfUpdateList <- split(x = dfUpdate, f = dfUpdate$"term")

rm(h_att); rm(dfUpdate); gc(reset = TRUE)

## -- to Apply function ----
tBeg <- Sys.time(); tBeg 
numTerms <- length(dfUpdateList)
resultFinal <- foreach(i = 1:numTerms, .combine = rbind) %dopar%{
  result <- imputeTermsInfoGain(dfUpdate = dfUpdateList[[i]], dfLabel = dfLabel)
  result$term <- unique(dfUpdateList[[i]]$term)
  gc(reset = TRUE)
  
  return(result)
}
tEnd <- Sys.time(); tEnd  
tEnd - tBeg

timeInfoGain <- data.frame(tBeg = tBeg,
                           tEnd = tEnd,
                           dif = tEnd - tBeg)


## -- To save objects ----
save(resultFinal, file = file.path(path, "resultInfoGain.RData"))
save(timeInfoGain, file = file.path(path, "timeInfoGain.RData"))
