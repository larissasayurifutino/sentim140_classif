rm(list = ls()); gc()
library(stringr)

cam <- "/home/leste/06_AplicacaoSentimento/01_Bases"
load(file = file.path(cam, "resultInfoGain.RData"))

featuresInfoGain <- which(resultFinal$importance != 0)
percFeaturesManter <- length(featuresInfoGain)/ nrow(resultFinal)

featuresManter <- resultFinal[featuresInfoGain,]

featuresComAlgarismos <- str_detect(string = featuresManter$"term", pattern = "[0-9]")
length(which(featuresComAlgarismos))
featuresManter$"term"[featuresComAlgarismos]

featuresManter <- featuresManter[!featuresComAlgarismos,]
percFeaturesManter <- nrow(featuresManter)/ nrow(resultFinal)

save(featuresManter, file = file.path(cam, "featuresManter.RData"))
