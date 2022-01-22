rm(list = ls()); gc(reset = TRUE)

library(dplyr)

cam <- "/media/luis/5B8F-C6B3/06_AplicacaoSentimento/01_Bases/resultClaraAdap/descritiva"

arqs <- list.files(cam)

descr <- lapply(X = arqs, FUN = function(x) {load(file.path(cam, x)); return(descritivaCl)})

descritivaCl <- do.call(rbind, descr)

maiorPercPorCl <- descritivaCl %>%
  group_by(iteracao) %>%
  summarise(maxPerc = max(perc)) %>%
  arrange()

melhorIter <- maiorPercPorCl %>%
  filter(maxPerc == min(maxPerc)) %>%
  select(iteracao)
  
melhorIter

save(descritivaCl, file = file.path(cam, "maiorPercPorCl.RData"))
save(melhorIter, file = file.path(cam, "melhorIter.RData"))
