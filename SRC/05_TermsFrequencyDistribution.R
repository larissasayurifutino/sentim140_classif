## -- This script reads the long format file and saves terms frequencies with and without stopwords
# 14

rm(list = ls()); gc()
library(dplyr)
library(stopwords)

cam <- "DATA"
load(file = file.path(cam, "df.RData"))

# Count word frequencies
h_att <- df %>%
  group_by(term) %>%
  summarise(freq = n_distinct(document))


h_att <- h_att[order(h_att$freq, decreasing = TRUE),]
save(h_att, file = file.path(cam, "freqDistSentData.RData"))


# Removing stop words
h_att$"term" <- tolower(h_att$"term")
sw <- stopwords(language = "en", source = "smart")

posStopWords <- which(h_att$"term" %in% sw)
h_att <- h_att[-posStopWords,]

save(h_att, file = file.path(cam, "freqDistSentDataNoSW.RData"))

