## -- This script removes tokens with importance = 0 (zero) and that are numbers based on Information Gain metric
# 16

rm(list = ls()); gc(reset = TRUE)

## -- To avoid scientific notation ----
options(scipen = 0)

## -- Data ----
path <- "DATA"
load(file.path(path, "df.RData"))
load(file.path(path, "featuresToKeep.RData"))

## -- To fix documents ids that are in scientific notation ----
df$"document" <- formatC(x = as.numeric(df$"document"), format = "f", digits = 0)

## -- Features entries chosen in accordance with Information Gain metric ----
entriesToKeep <- which(df$"term" %in% featuresToKeep$"term")
dfToKeep <- df[entriesToKeep,]

## -- How many documents were lost removing features ----
length(unique(df$"document")) - length(unique(dfToKeep$"document"))

## -- To save ----
save(dfToKeep, file = file.path(path, "dfToKeep.RData"))
