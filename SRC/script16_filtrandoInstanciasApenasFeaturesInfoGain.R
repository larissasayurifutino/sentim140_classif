rm(list = ls()); gc(reset = TRUE)

options(scipen = 0)

## -- Dados ----
cam <- "/home/luis/Desktop/clara/dataset"
load(file.path(cam, "df.RData"))
load(file.path(cam, "featuresManter.RData"))

## -- Os iids dos docs estao com notacao cientifica ----
df$"document" <- formatC(x = as.numeric(df$"document"), format = "f", digits = 0)

## -- Entradas das features escolhidas pelo Information Gain ----
entManter <- which(df$"term" %in% featuresManter$"term")
dfManter <- df[entManter,]

length(unique(df$"document")) - length(unique(dfManter$"document"))

save(dfManter, file = file.path(cam, "dfManter.RData"))
