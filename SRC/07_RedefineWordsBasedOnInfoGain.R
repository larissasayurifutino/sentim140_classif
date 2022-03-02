## -- This script saves an object with tokens with importance != 0(zero) based 
## on Information Gain metric and that are numbers -- ##
# 17
rm(list = ls()); gc()
library(stringr)


## -- Data ----
path <- "DATA"
load(file = file.path(path, "resultInfoGain.RData"))

## -- To remove tokens with importance = 0 (zero) ----
featuresInfoGain <- which(resultFinal$importance != 0)
percFeaturesToKeep <- length(featuresInfoGain)/ nrow(resultFinal)

featuresToKeep <- resultFinal[featuresInfoGain,]

## -- To remove tokens that are numbers ----
featuresWithDigits <- str_detect(string = featuresToKeep$"term", pattern = "[0-9]")
percFeaturesToKeep <- nrow(featuresToKeep)/ nrow(resultFinal)

featuresToKeep <- featuresToKeep[!featuresWithDigits,]

## -- To save ----
save(featuresToKeep, file = file.path(path, "featuresToKeep.RData"))
