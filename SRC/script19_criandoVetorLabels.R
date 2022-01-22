rm(list = ls()); gc()

## -- Dados ----
cam <- "/home/leste/06_AplicacaoSentimento/01_Bases"
camSalvar <- "/home/leste/06_AplicacaoSentimento/01_Bases/matDTM"
# cluster
load(file.path(cam, "cluster980000DefMelhorClusterInduzido20000.RData"))
# document e target
load(file.path(cam, "df.RData"))

## -- Separando bases por Grupos/Clusters ----
cl <- data.frame(matDissimilUnseenDataDT[, c("rn", "medoid")])
clusters <- sort(unique(cl$"medoid"))
cl$"medoid" <- factor(x = cl$"medoid", levels = clusters, labels = clusters)
cl$"rn" <- as.character(cl$"rn")
cl$"rn" <- formatC(x = as.numeric(cl$"rn"), digits = 0, format = "f")
rm(matDissimilUnseenDataDT); gc(reset = TRUE)

## -- Id/document e label ----
docTarget <- df[, c("document", "target")]
docTarget <- unique(docTarget)
rm(df); gc(reset = TRUE)
docTarget$"document" <- formatC(x = as.numeric(docTarget$"document"), digits = 0, format = "f")

## -- Juntando ----
dfComplete <- merge(docTarget, cl, by.x = "document", by.y = "rn", all.y = TRUE)
dfComplete$"document" <- formatC(x = dfComplete$"document", digits = 0, format = "f")

listaGruposDados <- split(x = dfComplete, f = dfComplete$"medoid")

lapply(X = listaGruposDados, FUN = nrow)

## -- Vetor de Labels ----
grupo <- 1
for(grupo in seq(numCluster)[-1]){
  df <- listaGruposDados[[grupo]] 
  df$medoid <- NULL
  #df <- data.table(df)
  
  tCom <- Sys.time(); tCom
  matDTMCast <- reshape2::dcast(df, 
                                id ~ word, fun.aggregate = sum,
                                value.var = "n")
  tFim <- Sys.time(); tFim 
  tFim - tCom

  
  ## -- Salvando ----
  grupoCh <- ifelse(nchar(grupo) == 1, paste0("0", grupo), as.character(grupo))
  nomeArqSalvar <- paste0("matDTMCast", grupoCh, ".RData", collapse = "")
  save(matDTMCast, file = file.path(camSalvar, nomeArqSalvar))
  
  rm(matDTMCast); rm(df); gc()
}

