cam <- "/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/resClassif"
arqs <- list.files(cam)

lista <- lapply(arqs, FUN = function(x){load(file.path(cam, x)); return(resultadoFinal)})
dfResultados <- do.call(rbind, lista)

save(dfResultados, file = file.path("/media/leste/5B8F-C6B3/06_AplicacaoSentimento/01_Bases", "dfResultados.RData"))
